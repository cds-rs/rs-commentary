//! Iterator-based Abstract Syntax Tree (AST) traversal.
//!
//! Provides a lazy iterator over AST events, allowing consumers to fold/scan
//! with their own state machines. This eliminates duplicate traversal code
//! across the codebase.
//!
//! # Example
//!
//! ```ignore
//! use rs_commentary::util::{AstIter, AstEvent};
//!
//! // Single consumer
//! let state = AstIter::new(&source_file)
//!     .fold(MyState::new(), |mut state, event| {
//!         state.process(event);
//!         state
//!     });
//!
//! // Multiple consumers in one pass
//! for event in AstIter::new(&source_file) {
//!     ownership.process(&event);
//!     call_detector.process(&event);
//! }
//! ```

use ra_ap_syntax::ast::{self, HasArgList, HasLoopBody, HasModuleItem, RangeItem};
use ra_ap_syntax::AstNode;

/// Events yielded during AST traversal.
///
/// Events fall into three categories:
///
/// - **Scope boundaries**: Paired enter/exit events for constructs that create scopes
///   (functions, blocks, for loops, match arms, closures)
/// - **Nodes**: Individual AST nodes (items, statements, expressions, patterns)
/// - **Special contexts**: Events that provide additional semantic context
///   (call arguments, method receivers, macros)
///
/// AST nodes are thin wrappers around `Arc`-based `SyntaxNode`, so cloning is cheap.
#[derive(Debug, Clone)]
pub enum AstEvent {
    // === Scope boundaries (paired enter/exit) ===
    /// Entering a function body. Parameters are yielded as `Pat` events after this.
    EnterFn(ast::Fn),
    /// Exiting a function body.
    ExitFn,
    /// Entering a block expression `{ ... }`.
    EnterBlock(ast::BlockExpr),
    /// Exiting a block expression.
    ExitBlock,
    /// Entering a for loop. The loop variable is yielded as a `Pat` event after this.
    EnterFor(ast::ForExpr),
    /// Exiting a for loop.
    ExitFor,
    /// Entering a match arm. The pattern is yielded as a `Pat` event after this.
    EnterMatchArm(ast::MatchArm),
    /// Exiting a match arm.
    ExitMatchArm,
    /// Entering a closure. Parameters are yielded as `Pat` events after this.
    EnterClosure(ast::ClosureExpr),
    /// Exiting a closure.
    ExitClosure,

    // === Nodes ===
    /// Top-level item (function, struct, impl, etc.).
    Item(ast::Item),
    /// Impl block.
    Impl(ast::Impl),
    /// Statement in a block.
    Stmt(ast::Stmt),
    /// Expression.
    Expr(ast::Expr),
    /// Pattern (in let bindings, function params, match arms, etc.).
    Pat {
        pat: ast::Pat,
        /// Whether the pattern has `mut` (e.g., `let mut x`).
        is_mut: bool,
    },

    // === Special contexts ===
    /// Argument in a function/method call.
    ///
    /// Used for detecting borrows and moves at call sites.
    CallArg {
        arg: ast::Expr,
        /// The call target name (function or method name)
        call_target: String,
    },
    /// Method receiver (for NLL borrow ending)
    MethodReceiver {
        receiver: ast::Expr,
        method_call: ast::MethodCallExpr,
    },
    /// Macro expression (for synthetic event generation)
    Macro(ast::MacroExpr),
}

/// Work items for the traversal stack.
#[derive(Debug, Clone)]
enum WorkItem {
    // Visit items - push children, yield event
    SourceFile(ast::SourceFile),
    Item(ast::Item),
    Impl(ast::Impl),
    Fn(ast::Fn),
    Block(ast::BlockExpr),
    Stmt(ast::Stmt),
    Expr(ast::Expr),
    Pat { pat: ast::Pat, is_mut: bool },

    // Exit markers - yield exit event
    ExitFn,
    ExitBlock,
    ExitFor,
    ExitMatchArm,
    ExitClosure,

    // Direct event emission
    Yield(YieldEvent),
}

/// Events that need to be yielded directly.
#[derive(Debug, Clone)]
enum YieldEvent {
    EnterFor(ast::ForExpr),
    EnterMatchArm(ast::MatchArm),
    EnterClosure(ast::ClosureExpr),
    CallArg { arg: ast::Expr, call_target: String },
    MethodReceiver {
        receiver: ast::Expr,
        method_call: ast::MethodCallExpr,
    },
    Macro(ast::MacroExpr),
}

/// Lazy iterator over AST events.
///
/// Uses a stack-based approach for traversal, yielding events in pre-order
/// (enter before children) with explicit exit events for scope boundaries.
///
/// # Entry Points
///
/// - [`AstIter::new`] - Traverse an entire source file
/// - [`AstIter::from_fn`] - Traverse a single function
/// - [`AstIter::from_expr`] - Traverse an expression tree
///
/// # Benefits
///
/// - **Single implementation**: Traversal logic is defined once, shared by all consumers
/// - **Composable**: Multiple consumers can process the same event stream
/// - **Lazy**: Only traverses as far as needed (early exit possible)
/// - **Extensible**: Add new event types without changing consumers
///
/// # Event Flow
///
/// ```text
/// SourceFile
///   └─ Item (function)
///        └─ EnterFn
///             └─ Pat (parameters)
///             └─ EnterBlock
///                  └─ Stmt
///                       └─ Expr / Pat / CallArg / ...
///             └─ ExitBlock
///        └─ ExitFn
/// ```
///
/// Scope boundaries (`EnterFn`/`ExitFn`, `EnterBlock`/`ExitBlock`, etc.) are
/// always paired, allowing consumers to maintain scope stacks.
pub struct AstIter {
    stack: Vec<WorkItem>,
}

impl AstIter {
    /// Create a new iterator starting from a source file.
    pub fn new(file: &ast::SourceFile) -> Self {
        Self {
            stack: vec![WorkItem::SourceFile(file.clone())],
        }
    }

    /// Create a new iterator starting from a function.
    pub fn from_fn(func: &ast::Fn) -> Self {
        Self {
            stack: vec![WorkItem::Fn(func.clone())],
        }
    }

    /// Create a new iterator starting from an expression.
    pub fn from_expr(expr: &ast::Expr) -> Self {
        Self {
            stack: vec![WorkItem::Expr(expr.clone())],
        }
    }

    /// Push [`CallExpr`] children onto the stack.
    ///
    /// Processes arguments in reverse order (so they're visited left-to-right),
    /// emitting [`CallArg`](YieldEvent::CallArg) events for each argument.
    fn push_call_children(&mut self, call_expr: &ast::CallExpr) {
        let call_target = call_expr
            .expr()
            .and_then(|e| {
                if let ast::Expr::PathExpr(path) = e {
                    path.path().map(|p| p.syntax().text().to_string())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| "call".to_string());

        if let Some(args) = call_expr.arg_list() {
            let args_vec: Vec<_> = args.args().collect();
            for arg in args_vec.into_iter().rev() {
                self.stack.push(WorkItem::Expr(arg.clone()));
                self.stack.push(WorkItem::Yield(YieldEvent::CallArg {
                    arg,
                    call_target: call_target.clone(),
                }));
            }
        }
        if let Some(callee) = call_expr.expr() {
            self.stack.push(WorkItem::Expr(callee));
        }
    }

    /// Push [`MethodCallExpr`] children onto the stack.
    ///
    /// Processes arguments in reverse order, then the receiver. Emits
    /// [`MethodReceiver`](YieldEvent::MethodReceiver) for the receiver and
    /// [`CallArg`](YieldEvent::CallArg) for each argument.
    fn push_method_call_children(&mut self, method_call: &ast::MethodCallExpr) {
        let call_target = method_call
            .name_ref()
            .map(|n| n.text().to_string())
            .unwrap_or_else(|| "call".to_string());

        if let Some(args) = method_call.arg_list() {
            let args_vec: Vec<_> = args.args().collect();
            for arg in args_vec.into_iter().rev() {
                self.stack.push(WorkItem::Expr(arg.clone()));
                self.stack.push(WorkItem::Yield(YieldEvent::CallArg {
                    arg,
                    call_target: call_target.clone(),
                }));
            }
        }
        if let Some(receiver) = method_call.receiver() {
            self.stack.push(WorkItem::Expr(receiver.clone()));
            self.stack
                .push(WorkItem::Yield(YieldEvent::MethodReceiver {
                    receiver,
                    method_call: method_call.clone(),
                }));
        }
    }

    /// Push [`IfExpr`] children onto the stack.
    ///
    /// Handles `if`/`else if`/`else` chains by recursively pushing nested
    /// `IfExpr` nodes. Children are pushed in reverse order (else, then, condition)
    /// so they're visited in source order.
    fn push_if_children(&mut self, if_expr: &ast::IfExpr) {
        if let Some(else_branch) = if_expr.else_branch() {
            match else_branch {
                ast::ElseBranch::Block(block) => {
                    self.stack.push(WorkItem::Block(block));
                }
                ast::ElseBranch::IfExpr(nested_if) => {
                    self.stack
                        .push(WorkItem::Expr(ast::Expr::IfExpr(nested_if)));
                }
            }
        }
        if let Some(then_branch) = if_expr.then_branch() {
            self.stack.push(WorkItem::Block(then_branch));
        }
        if let Some(cond) = if_expr.condition() {
            self.stack.push(WorkItem::Expr(cond));
        }
    }

    /// Push [`ForExpr`] children onto the stack.
    ///
    /// For loops create their own scope for the loop variable. Emits
    /// [`EnterFor`](YieldEvent::EnterFor) before the pattern and `ExitFor`
    /// after the body. The iterable expression is visited outside the loop scope.
    fn push_for_children(&mut self, for_expr: &ast::ForExpr) {
        self.stack.push(WorkItem::ExitFor);
        if let Some(body) = for_expr.loop_body() {
            self.stack.push(WorkItem::Block(body));
        }
        if let Some(pat) = for_expr.pat() {
            let is_mut = matches!(&pat, ast::Pat::IdentPat(id) if id.mut_token().is_some());
            self.stack.push(WorkItem::Pat { pat, is_mut });
        }
        self.stack
            .push(WorkItem::Yield(YieldEvent::EnterFor(for_expr.clone())));
        if let Some(iterable) = for_expr.iterable() {
            self.stack.push(WorkItem::Expr(iterable));
        }
    }

    /// Push [`MatchExpr`] children onto the stack.
    ///
    /// Each match arm creates its own scope. Emits [`EnterMatchArm`](YieldEvent::EnterMatchArm)
    /// before each arm's pattern and `ExitMatchArm` after its body. Arms are processed
    /// in reverse order so they're visited top-to-bottom. Guard expressions are
    /// visited between the pattern and body.
    fn push_match_children(&mut self, match_expr: &ast::MatchExpr) {
        if let Some(arm_list) = match_expr.match_arm_list() {
            let arms: Vec<_> = arm_list.arms().collect();
            for arm in arms.into_iter().rev() {
                self.stack.push(WorkItem::ExitMatchArm);
                if let Some(body) = arm.expr() {
                    self.stack.push(WorkItem::Expr(body));
                }
                if let Some(guard) = arm.guard() {
                    if let Some(cond) = guard.condition() {
                        self.stack.push(WorkItem::Expr(cond));
                    }
                }
                if let Some(pat) = arm.pat() {
                    self.stack.push(WorkItem::Pat { pat, is_mut: false });
                }
                self.stack
                    .push(WorkItem::Yield(YieldEvent::EnterMatchArm(arm)));
            }
        }
        if let Some(scrutinee) = match_expr.expr() {
            self.stack.push(WorkItem::Expr(scrutinee));
        }
    }

    /// Push [`ClosureExpr`] children onto the stack.
    ///
    /// Closures create their own scope for parameters. Emits
    /// [`EnterClosure`](YieldEvent::EnterClosure) before parameters and
    /// `ExitClosure` after the body. Parameters are processed in reverse
    /// order so they're visited left-to-right.
    fn push_closure_children(&mut self, closure: &ast::ClosureExpr) {
        self.stack.push(WorkItem::ExitClosure);
        if let Some(body) = closure.body() {
            self.stack.push(WorkItem::Expr(body));
        }
        if let Some(param_list) = closure.param_list() {
            let params: Vec<_> = param_list.params().collect();
            for param in params.into_iter().rev() {
                if let Some(pat) = param.pat() {
                    let is_mut =
                        matches!(&pat, ast::Pat::IdentPat(id) if id.mut_token().is_some());
                    self.stack.push(WorkItem::Pat { pat, is_mut });
                }
            }
        }
        self.stack
            .push(WorkItem::Yield(YieldEvent::EnterClosure(closure.clone())));
    }

    /// Push children of an expression onto the stack (in reverse order for correct traversal).
    fn push_expr_children(&mut self, expr: &ast::Expr) {
        match expr {
            // Leaf nodes - no children
            ast::Expr::PathExpr(_) | ast::Expr::Literal(_) | ast::Expr::UnderscoreExpr(_) => {}

            ast::Expr::RefExpr(ref_expr) => {
                if let Some(inner) = ref_expr.expr() {
                    self.stack.push(WorkItem::Expr(inner));
                }
            }

            // Complex nodes - delegate to helpers
            ast::Expr::CallExpr(e) => self.push_call_children(e),
            ast::Expr::MethodCallExpr(e) => self.push_method_call_children(e),
            ast::Expr::IfExpr(e) => self.push_if_children(e),
            ast::Expr::ForExpr(e) => self.push_for_children(e),
            ast::Expr::MatchExpr(e) => self.push_match_children(e),
            ast::Expr::ClosureExpr(e) => self.push_closure_children(e),

            ast::Expr::BlockExpr(block) => {
                self.stack.push(WorkItem::Block(block.clone()));
            }

            ast::Expr::LoopExpr(loop_expr) => {
                if let Some(body) = loop_expr.loop_body() {
                    self.stack.push(WorkItem::Block(body));
                }
            }

            ast::Expr::WhileExpr(while_expr) => {
                if let Some(body) = while_expr.loop_body() {
                    self.stack.push(WorkItem::Block(body));
                }
                if let Some(cond) = while_expr.condition() {
                    self.stack.push(WorkItem::Expr(cond));
                }
            }

            ast::Expr::ReturnExpr(return_expr) => {
                if let Some(expr) = return_expr.expr() {
                    self.stack.push(WorkItem::Expr(expr));
                }
            }

            ast::Expr::BreakExpr(break_expr) => {
                if let Some(expr) = break_expr.expr() {
                    self.stack.push(WorkItem::Expr(expr));
                }
            }

            ast::Expr::ContinueExpr(_) => {}

            ast::Expr::BinExpr(bin_expr) => {
                if let Some(rhs) = bin_expr.rhs() {
                    self.stack.push(WorkItem::Expr(rhs));
                }
                if let Some(lhs) = bin_expr.lhs() {
                    self.stack.push(WorkItem::Expr(lhs));
                }
            }

            ast::Expr::PrefixExpr(prefix_expr) => {
                if let Some(inner) = prefix_expr.expr() {
                    self.stack.push(WorkItem::Expr(inner));
                }
            }

            ast::Expr::IndexExpr(index_expr) => {
                if let Some(index) = index_expr.index() {
                    self.stack.push(WorkItem::Expr(index));
                }
                if let Some(base) = index_expr.base() {
                    self.stack.push(WorkItem::Expr(base));
                }
            }

            ast::Expr::FieldExpr(field_expr) => {
                if let Some(base) = field_expr.expr() {
                    self.stack.push(WorkItem::Expr(base));
                }
            }

            ast::Expr::TupleExpr(tuple) => {
                let fields: Vec<_> = tuple.fields().collect();
                for field in fields.into_iter().rev() {
                    self.stack.push(WorkItem::Expr(field));
                }
            }

            ast::Expr::ArrayExpr(array) => match array.kind() {
                ast::ArrayExprKind::ElementList(elements) => {
                    let elems: Vec<_> = elements.collect();
                    for elem in elems.into_iter().rev() {
                        self.stack.push(WorkItem::Expr(elem));
                    }
                }
                ast::ArrayExprKind::Repeat { initializer, repeat } => {
                    if let Some(rep) = repeat {
                        self.stack.push(WorkItem::Expr(rep));
                    }
                    if let Some(init) = initializer {
                        self.stack.push(WorkItem::Expr(init));
                    }
                }
            },

            ast::Expr::ParenExpr(paren) => {
                if let Some(inner) = paren.expr() {
                    self.stack.push(WorkItem::Expr(inner));
                }
            }

            ast::Expr::CastExpr(cast) => {
                if let Some(expr) = cast.expr() {
                    self.stack.push(WorkItem::Expr(expr));
                }
            }

            ast::Expr::AwaitExpr(await_expr) => {
                if let Some(expr) = await_expr.expr() {
                    self.stack.push(WorkItem::Expr(expr));
                }
            }

            ast::Expr::TryExpr(try_expr) => {
                if let Some(expr) = try_expr.expr() {
                    self.stack.push(WorkItem::Expr(expr));
                }
            }

            ast::Expr::MacroExpr(macro_expr) => {
                // Macros are opaque - yield event for consumers to handle
                self.stack
                    .push(WorkItem::Yield(YieldEvent::Macro(macro_expr.clone())));
            }

            ast::Expr::RecordExpr(record) => {
                if let Some(field_list) = record.record_expr_field_list() {
                    if let Some(spread) = field_list.spread() {
                        self.stack.push(WorkItem::Expr(spread));
                    }
                    let fields: Vec<_> = field_list.fields().collect();
                    for field in fields.into_iter().rev() {
                        if let Some(expr) = field.expr() {
                            self.stack.push(WorkItem::Expr(expr));
                        }
                    }
                }
            }

            ast::Expr::RangeExpr(range) => {
                if let Some(end) = range.end() {
                    self.stack.push(WorkItem::Expr(end));
                }
                if let Some(start) = range.start() {
                    self.stack.push(WorkItem::Expr(start));
                }
            }

            ast::Expr::LetExpr(let_expr) => {
                if let Some(pat) = let_expr.pat() {
                    self.stack.push(WorkItem::Pat { pat, is_mut: false });
                }
                if let Some(expr) = let_expr.expr() {
                    self.stack.push(WorkItem::Expr(expr));
                }
            }

            // Catch-all for any expressions we haven't explicitly handled
            _ => {}
        }
    }

    /// Push children of a pattern onto the stack.
    fn push_pat_children(&mut self, pat: &ast::Pat) {
        match pat {
            ast::Pat::TuplePat(tuple) => {
                let fields: Vec<_> = tuple.fields().collect();
                for field in fields.into_iter().rev() {
                    self.stack.push(WorkItem::Pat {
                        pat: field,
                        is_mut: false,
                    });
                }
            }
            ast::Pat::RecordPat(record) => {
                if let Some(field_list) = record.record_pat_field_list() {
                    let fields: Vec<_> = field_list.fields().collect();
                    for field in fields.into_iter().rev() {
                        if let Some(pat) = field.pat() {
                            self.stack.push(WorkItem::Pat { pat, is_mut: false });
                        }
                    }
                }
            }
            ast::Pat::SlicePat(slice) => {
                let pats: Vec<_> = slice.pats().collect();
                for p in pats.into_iter().rev() {
                    self.stack.push(WorkItem::Pat { pat: p, is_mut: false });
                }
            }
            ast::Pat::OrPat(or_pat) => {
                let pats: Vec<_> = or_pat.pats().collect();
                for p in pats.into_iter().rev() {
                    self.stack.push(WorkItem::Pat { pat: p, is_mut: false });
                }
            }
            ast::Pat::RefPat(ref_pat) => {
                if let Some(inner) = ref_pat.pat() {
                    self.stack.push(WorkItem::Pat {
                        pat: inner,
                        is_mut: false,
                    });
                }
            }
            ast::Pat::BoxPat(box_pat) => {
                if let Some(inner) = box_pat.pat() {
                    self.stack.push(WorkItem::Pat {
                        pat: inner,
                        is_mut: false,
                    });
                }
            }
            ast::Pat::ParenPat(paren) => {
                if let Some(inner) = paren.pat() {
                    self.stack.push(WorkItem::Pat {
                        pat: inner,
                        is_mut: false,
                    });
                }
            }
            ast::Pat::TupleStructPat(tuple_struct) => {
                let fields: Vec<_> = tuple_struct.fields().collect();
                for field in fields.into_iter().rev() {
                    self.stack.push(WorkItem::Pat {
                        pat: field,
                        is_mut: false,
                    });
                }
            }
            // IdentPat, WildcardPat, LiteralPat, etc. are leaf patterns
            _ => {}
        }
    }
}

impl Iterator for AstIter {
    type Item = AstEvent;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let item = self.stack.pop()?;

            match item {
                WorkItem::SourceFile(file) => {
                    // Push items in reverse order
                    let items: Vec<_> = file.items().collect();
                    for item in items.into_iter().rev() {
                        self.stack.push(WorkItem::Item(item));
                    }
                    // Continue to next item (SourceFile doesn't yield an event)
                }

                WorkItem::Item(item) => {
                    // Push children based on item type
                    match &item {
                        ast::Item::Fn(func) => {
                            self.stack.push(WorkItem::Fn(func.clone()));
                        }
                        ast::Item::Impl(impl_block) => {
                            self.stack.push(WorkItem::Impl(impl_block.clone()));
                        }
                        _ => {}
                    }
                    return Some(AstEvent::Item(item));
                }

                WorkItem::Impl(impl_block) => {
                    if let Some(assoc_items) = impl_block.assoc_item_list() {
                        let items: Vec<_> = assoc_items.assoc_items().collect();
                        for assoc_item in items.into_iter().rev() {
                            if let ast::AssocItem::Fn(func) = assoc_item {
                                self.stack.push(WorkItem::Fn(func));
                            }
                        }
                    }
                    return Some(AstEvent::Impl(impl_block));
                }

                WorkItem::Fn(func) => {
                    // Push: ExitFn, body, params, then yield EnterFn
                    self.stack.push(WorkItem::ExitFn);

                    if let Some(body) = func.body() {
                        self.stack.push(WorkItem::Block(body));
                    }

                    if let Some(param_list) = func.param_list() {
                        let params: Vec<_> = param_list.params().collect();
                        for param in params.into_iter().rev() {
                            if let Some(pat) = param.pat() {
                                let is_mut =
                                    matches!(&pat, ast::Pat::IdentPat(id) if id.mut_token().is_some());
                                self.stack.push(WorkItem::Pat { pat, is_mut });
                            }
                        }
                    }

                    return Some(AstEvent::EnterFn(func));
                }

                WorkItem::Block(block) => {
                    // Push: ExitBlock, tail, stmts (reversed)
                    self.stack.push(WorkItem::ExitBlock);

                    if let Some(tail) = block.tail_expr() {
                        self.stack.push(WorkItem::Expr(tail));
                    }

                    let stmts: Vec<_> = block.statements().collect();
                    for stmt in stmts.into_iter().rev() {
                        self.stack.push(WorkItem::Stmt(stmt));
                    }

                    return Some(AstEvent::EnterBlock(block));
                }

                WorkItem::Stmt(stmt) => {
                    // Push children based on statement type
                    match &stmt {
                        ast::Stmt::LetStmt(let_stmt) => {
                            // Pattern after initializer (but we push in reverse)
                            if let Some(pat) = let_stmt.pat() {
                                let is_mut =
                                    matches!(&pat, ast::Pat::IdentPat(id) if id.mut_token().is_some());
                                self.stack.push(WorkItem::Pat { pat, is_mut });
                            }
                            if let Some(init) = let_stmt.initializer() {
                                self.stack.push(WorkItem::Expr(init));
                            }
                        }
                        ast::Stmt::ExprStmt(expr_stmt) => {
                            if let Some(expr) = expr_stmt.expr() {
                                self.stack.push(WorkItem::Expr(expr));
                            }
                        }
                        ast::Stmt::Item(nested_item) => {
                            self.stack.push(WorkItem::Item(nested_item.clone()));
                        }
                    }
                    return Some(AstEvent::Stmt(stmt));
                }

                WorkItem::Expr(expr) => {
                    self.push_expr_children(&expr);
                    return Some(AstEvent::Expr(expr));
                }

                WorkItem::Pat { pat, is_mut } => {
                    self.push_pat_children(&pat);
                    return Some(AstEvent::Pat { pat, is_mut });
                }

                // Exit markers
                WorkItem::ExitFn => return Some(AstEvent::ExitFn),
                WorkItem::ExitBlock => return Some(AstEvent::ExitBlock),
                WorkItem::ExitFor => return Some(AstEvent::ExitFor),
                WorkItem::ExitMatchArm => return Some(AstEvent::ExitMatchArm),
                WorkItem::ExitClosure => return Some(AstEvent::ExitClosure),

                // Direct yields
                WorkItem::Yield(yield_event) => {
                    return Some(match yield_event {
                        YieldEvent::EnterFor(f) => AstEvent::EnterFor(f),
                        YieldEvent::EnterMatchArm(a) => AstEvent::EnterMatchArm(a),
                        YieldEvent::EnterClosure(c) => AstEvent::EnterClosure(c),
                        YieldEvent::CallArg { arg, call_target } => {
                            AstEvent::CallArg { arg, call_target }
                        }
                        YieldEvent::MethodReceiver {
                            receiver,
                            method_call,
                        } => AstEvent::MethodReceiver {
                            receiver,
                            method_call,
                        },
                        YieldEvent::Macro(m) => AstEvent::Macro(m),
                    });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ra_ap_syntax::SourceFile;

    #[test]
    fn test_simple_function() {
        let source = r#"
fn main() {
    let x = 1;
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have: Item, EnterFn, Pat(x), EnterBlock, Stmt, Expr, ExitBlock, ExitFn
        assert!(events.iter().any(|e| matches!(e, AstEvent::EnterFn(_))));
        assert!(events.iter().any(|e| matches!(e, AstEvent::ExitFn)));
        assert!(events.iter().any(|e| matches!(e, AstEvent::EnterBlock(_))));
        assert!(events.iter().any(|e| matches!(e, AstEvent::ExitBlock)));
    }

    #[test]
    fn test_method_call_events() {
        let source = r#"
fn main() {
    x.push(1);
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have MethodReceiver and CallArg events
        assert!(events
            .iter()
            .any(|e| matches!(e, AstEvent::MethodReceiver { .. })));
        assert!(events.iter().any(|e| matches!(e, AstEvent::CallArg { .. })));
    }

    #[test]
    fn test_for_loop_scope() {
        let source = r#"
fn main() {
    for i in 0..10 {
        println!("{}", i);
    }
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have EnterFor and ExitFor
        assert!(events.iter().any(|e| matches!(e, AstEvent::EnterFor(_))));
        assert!(events.iter().any(|e| matches!(e, AstEvent::ExitFor)));
    }

    #[test]
    fn test_match_arm_scope() {
        let source = r#"
fn main() {
    match x {
        Some(v) => v,
        None => 0,
    }
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have EnterMatchArm and ExitMatchArm (twice, one per arm)
        let enter_count = events
            .iter()
            .filter(|e| matches!(e, AstEvent::EnterMatchArm(_)))
            .count();
        let exit_count = events
            .iter()
            .filter(|e| matches!(e, AstEvent::ExitMatchArm))
            .count();

        assert_eq!(enter_count, 2);
        assert_eq!(exit_count, 2);
    }

    #[test]
    fn test_closure_scope() {
        let source = r#"
fn main() {
    let f = |x| x + 1;
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        assert!(events
            .iter()
            .any(|e| matches!(e, AstEvent::EnterClosure(_))));
        assert!(events.iter().any(|e| matches!(e, AstEvent::ExitClosure)));
    }

    #[test]
    fn test_macro_event() {
        let source = r#"
fn main() {
    println!("hello");
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        assert!(events.iter().any(|e| matches!(e, AstEvent::Macro(_))));
    }

    #[test]
    fn test_event_order() {
        let source = r#"
fn foo() {
    let x = 1;
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Find positions of key events
        let enter_fn_pos = events
            .iter()
            .position(|e| matches!(e, AstEvent::EnterFn(_)))
            .unwrap();
        let enter_block_pos = events
            .iter()
            .position(|e| matches!(e, AstEvent::EnterBlock(_)))
            .unwrap();
        let exit_block_pos = events
            .iter()
            .position(|e| matches!(e, AstEvent::ExitBlock))
            .unwrap();
        let exit_fn_pos = events
            .iter()
            .position(|e| matches!(e, AstEvent::ExitFn))
            .unwrap();

        // Verify order: EnterFn < EnterBlock < ExitBlock < ExitFn
        assert!(enter_fn_pos < enter_block_pos);
        assert!(enter_block_pos < exit_block_pos);
        assert!(exit_block_pos < exit_fn_pos);
    }

    // Tests for complex expression types handled by push_expr_children

    #[test]
    fn test_call_expr_no_args() {
        let source = r#"
fn main() {
    foo();
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have an Expr event for the call, but no CallArg events
        assert!(events.iter().any(|e| matches!(e, AstEvent::Expr(_))));
        assert!(!events.iter().any(|e| matches!(e, AstEvent::CallArg { .. })));
    }

    #[test]
    fn test_if_else_chain() {
        let source = r#"
fn main() {
    if a {
        1
    } else if b {
        2
    } else {
        3
    }
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have multiple EnterBlock/ExitBlock pairs for each branch
        let enter_block_count = events
            .iter()
            .filter(|e| matches!(e, AstEvent::EnterBlock(_)))
            .count();
        let exit_block_count = events
            .iter()
            .filter(|e| matches!(e, AstEvent::ExitBlock))
            .count();

        // 1 fn body + 3 if/else blocks = 4 block pairs
        assert_eq!(enter_block_count, 4);
        assert_eq!(exit_block_count, 4);
    }

    #[test]
    fn test_match_with_guards() {
        let source = r#"
fn main() {
    match x {
        Some(v) if v > 0 => v,
        Some(v) => -v,
        None => 0,
    }
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have 3 match arms
        let enter_arm_count = events
            .iter()
            .filter(|e| matches!(e, AstEvent::EnterMatchArm(_)))
            .count();
        assert_eq!(enter_arm_count, 3);

        // Should have Pat events for the patterns (Some(v) bindings)
        let pat_count = events
            .iter()
            .filter(|e| matches!(e, AstEvent::Pat { .. }))
            .count();
        assert!(pat_count >= 3); // At least one per arm
    }

    #[test]
    fn test_closure_with_move() {
        let source = r#"
fn main() {
    let x = String::new();
    let f = move || x.len();
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have EnterClosure and ExitClosure
        assert!(events
            .iter()
            .any(|e| matches!(e, AstEvent::EnterClosure(_))));
        assert!(events.iter().any(|e| matches!(e, AstEvent::ExitClosure)));

        // Should have MethodReceiver for x.len()
        assert!(events
            .iter()
            .any(|e| matches!(e, AstEvent::MethodReceiver { .. })));
    }

    #[test]
    fn test_nested_impl_fns() {
        let source = r#"
struct Foo;

impl Foo {
    fn outer(&self) {
        fn inner() {
            let x = 1;
        }
        inner();
    }
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have 2 EnterFn events (outer and inner)
        let enter_fn_count = events
            .iter()
            .filter(|e| matches!(e, AstEvent::EnterFn(_)))
            .count();
        assert_eq!(enter_fn_count, 2);

        // Should have 2 ExitFn events
        let exit_fn_count = events
            .iter()
            .filter(|e| matches!(e, AstEvent::ExitFn))
            .count();
        assert_eq!(exit_fn_count, 2);
    }

    #[test]
    fn test_stmt_with_nested_item() {
        let source = r#"
fn main() {
    struct Local { x: i32 }
    let s = Local { x: 1 };
}
"#;
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        let events: Vec<_> = AstIter::new(&file).collect();

        // Should have Item event for the nested struct
        assert!(events.iter().any(|e| matches!(e, AstEvent::Item(_))));

        // Should still have Pat event for the let binding
        assert!(events.iter().any(|e| matches!(e, AstEvent::Pat { .. })));
    }
}
