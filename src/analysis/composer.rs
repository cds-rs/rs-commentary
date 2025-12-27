//! Layer composition: combining AST analysis with type oracle.
//!
//! This module composes Layer 1 (AST state machine) with Layer 2 (type oracle)
//! to produce accurate ownership annotations.

use ra_ap_syntax::{
    ast::{self, HasArgList, HasLoopBody, HasModuleItem, HasName},
    AstNode, SourceFile, TextRange, TextSize,
};
use std::collections::HashMap;

use super::layers::{AstEvent, AstStateMachine, CopyStatus, MoveContext, TypeOracle, VarState};
use super::state::{Annotation, BindingId, BindingState, OwnershipSet, SetAnnotation};

/// Layered ownership analyzer.
///
/// Composes Layer 1 (AST state machine) with optional Layer 2 (type oracle)
/// to produce ownership annotations.
pub struct LayeredAnalyzer<O: TypeOracle> {
    /// The core state machine
    state_machine: AstStateMachine,
    /// Optional type oracle for refinement
    oracle: Option<O>,
    /// Generated annotations
    annotations: Vec<Annotation>,
    /// Set annotations per line
    set_annotations: Vec<SetAnnotation>,
    /// Current function name for set notation
    current_fn_name: String,
    /// Source text
    source: String,
    /// Mapping from binding ID to its declaration range
    binding_ranges: HashMap<BindingId, TextRange>,
}

impl<O: TypeOracle> LayeredAnalyzer<O> {
    pub fn new(oracle: Option<O>) -> Self {
        Self {
            state_machine: AstStateMachine::new(),
            oracle,
            annotations: Vec::new(),
            set_annotations: Vec::new(),
            current_fn_name: String::new(),
            source: String::new(),
            binding_ranges: HashMap::new(),
        }
    }

    /// Analyze source and generate annotations.
    pub fn analyze(&mut self, source: &str) -> Vec<Annotation> {
        self.source = source.to_string();
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        self.visit_source_file(&file);
        self.annotations.clone()
    }

    /// Get set annotations.
    pub fn set_annotations(&self) -> &[SetAnnotation] {
        &self.set_annotations
    }

    /// Convert state machine VarState to BindingState for annotations.
    fn var_state_to_binding_state(&self, state: &VarState, binding_id: BindingId) -> BindingState {
        match state {
            VarState::Uninit => BindingState::Owned { mutable: false }, // Treat as owned until initialized
            VarState::Owned { mutable } => BindingState::Owned { mutable: *mutable },
            VarState::Shared { original_mutable, .. } => BindingState::Shared {
                original_mutable: *original_mutable,
                borrowed_by: vec![],
            },
            VarState::Frozen { original_mutable } => BindingState::Frozen {
                original_mutable: *original_mutable,
                borrowed_by: binding_id,
            },
            VarState::Moved => BindingState::Moved { to: None },
            VarState::Dropped => BindingState::Dropped,
        }
    }

    /// Add an annotation based on current state.
    fn annotate(&mut self, binding_id: BindingId, range: TextRange, explanation: String) {
        let name = self.state_machine
            .get_binding(binding_id)
            .map(|m| m.name.clone())
            .unwrap_or_else(|| "?".to_string());

        let state = self.state_machine
            .get_state(binding_id)
            .cloned()
            .unwrap_or(VarState::Uninit);

        let binding_state = self.var_state_to_binding_state(&state, binding_id);

        self.annotations.push(Annotation::new(
            range,
            name,
            binding_state,
            explanation,
        ));
    }

    /// Snapshot the current ownership set.
    fn snapshot_set(&mut self, offset: u32) {
        let line = self.offset_to_line(offset);
        let mut set = OwnershipSet::new(self.current_fn_name.clone(), line);

        for (id, state) in self.state_machine.all_states() {
            let meta = match self.state_machine.get_binding(*id) {
                Some(m) => m,
                None => continue,
            };

            match state {
                VarState::Owned { mutable } => {
                    set.add_owned(meta.name.clone(), *mutable);
                }
                VarState::Shared { original_mutable, .. } => {
                    set.add_shared(meta.name.clone(), *original_mutable);
                }
                VarState::Frozen { original_mutable } => {
                    set.add_frozen(meta.name.clone(), *original_mutable);
                }
                VarState::Moved | VarState::Dropped | VarState::Uninit => {
                    // Not in set
                }
            }
        }

        self.set_annotations.push(SetAnnotation { line, set });
    }

    fn offset_to_line(&self, offset: u32) -> u32 {
        let offset = offset as usize;
        self.source[..offset.min(self.source.len())]
            .chars()
            .filter(|&c| c == '\n')
            .count() as u32
    }

    /// Query the type oracle if available.
    fn query_is_copy(&self, offset: TextSize) -> Option<bool> {
        self.oracle.as_ref().and_then(|o| o.is_copy_at(offset))
    }

    // --- AST Visitor Methods ---

    fn visit_source_file(&mut self, file: &SourceFile) {
        for item in file.items() {
            self.visit_item(&item);
        }
    }

    fn visit_item(&mut self, item: &ast::Item) {
        match item {
            ast::Item::Fn(func) => self.visit_fn(func),
            ast::Item::Impl(impl_block) => {
                if let Some(assoc_items) = impl_block.assoc_item_list() {
                    for assoc_item in assoc_items.assoc_items() {
                        if let ast::AssocItem::Fn(func) = assoc_item {
                            self.visit_fn(&func);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn visit_fn(&mut self, func: &ast::Fn) {
        let prev_fn_name = self.current_fn_name.clone();
        if let Some(name) = func.name() {
            self.current_fn_name = name.text().to_string();
        }

        let scope = self.state_machine.new_scope_id();
        self.state_machine.process(AstEvent::ScopeEnter { scope });

        // Process parameters
        if let Some(param_list) = func.param_list() {
            for param in param_list.params() {
                let type_str = param.ty().map(|ty| ty.syntax().text().to_string());
                if let Some(pat) = param.pat() {
                    self.visit_param(&pat, type_str.as_deref());
                }
            }
        }

        // Snapshot at function entry
        if let Some(body) = func.body() {
            let start = u32::from(body.syntax().text_range().start());
            self.snapshot_set(start);
            self.visit_block_expr(&body);
        }

        // Collect bindings in this scope for exit event
        let bindings: Vec<BindingId> = self.state_machine
            .all_states()
            .keys()
            .filter(|id| {
                self.state_machine.get_binding(**id)
                    .map(|m| m.scope == scope)
                    .unwrap_or(false)
            })
            .cloned()
            .collect();

        self.state_machine.process(AstEvent::ScopeExit { scope, bindings });
        self.current_fn_name = prev_fn_name;
    }

    fn visit_param(&mut self, pat: &ast::Pat, type_str: Option<&str>) {
        let ast::Pat::IdentPat(ident_pat) = pat else {
            return;
        };

        let Some(name) = ident_pat.name() else { return };
        let name_str = name.text().to_string();
        let range = pat.syntax().text_range();
        let is_mutable = ident_pat.mut_token().is_some();

        let id = self.state_machine.new_binding_id();
        self.binding_ranges.insert(id, range);

        self.state_machine.process(AstEvent::Declaration {
            id,
            name: name_str.clone(),
            mutable: is_mutable,
            has_initializer: true,
            type_annotation: type_str.map(String::from),
            initializer: None,
        });

        // Generate annotation
        let explanation = match type_str {
            Some(t) if t.starts_with("&mut ") => format!("{}: mutable borrow parameter", name_str),
            Some(t) if t.starts_with('&') => format!("{}: shared borrow parameter", name_str),
            _ => format!("{}: owned parameter", name_str),
        };

        self.annotate(id, range, explanation);
    }

    fn visit_block_expr(&mut self, block: &ast::BlockExpr) {
        let scope = self.state_machine.new_scope_id();
        self.state_machine.process(AstEvent::ScopeEnter { scope });

        for stmt in block.statements() {
            self.visit_stmt(&stmt);
        }

        if let Some(expr) = block.tail_expr() {
            self.visit_expr(&expr);
        }

        // Collect bindings for this scope
        let bindings: Vec<BindingId> = self.state_machine
            .all_states()
            .keys()
            .filter(|id| {
                self.state_machine.get_binding(**id)
                    .map(|m| m.scope == scope)
                    .unwrap_or(false)
            })
            .cloned()
            .collect();

        self.state_machine.process(AstEvent::ScopeExit { scope, bindings });
    }

    fn visit_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::LetStmt(let_stmt) => self.visit_let_stmt(let_stmt),
            ast::Stmt::ExprStmt(expr_stmt) => {
                if let Some(expr) = expr_stmt.expr() {
                    self.visit_expr(&expr);
                }
            }
            ast::Stmt::Item(item) => self.visit_item(item),
        }

        let end = u32::from(stmt.syntax().text_range().end());
        self.snapshot_set(end);
    }

    fn visit_let_stmt(&mut self, let_stmt: &ast::LetStmt) {
        let type_hint = let_stmt.ty().map(|ty| ty.syntax().text().to_string());
        let init_text = let_stmt.initializer().map(|e| e.syntax().text().to_string());

        // Visit initializer first
        if let Some(init) = let_stmt.initializer() {
            self.visit_expr(&init);
        }

        // Create binding
        if let Some(pat) = let_stmt.pat() {
            self.visit_let_pat(&pat, type_hint.as_deref(), init_text.as_deref());
        }
    }

    fn visit_let_pat(&mut self, pat: &ast::Pat, type_hint: Option<&str>, init: Option<&str>) {
        match pat {
            ast::Pat::IdentPat(ident_pat) => {
                if let Some(name) = ident_pat.name() {
                    let id = self.state_machine.new_binding_id();
                    let name_str = name.text().to_string();
                    let range = pat.syntax().text_range();
                    let is_mutable = ident_pat.mut_token().is_some();

                    self.binding_ranges.insert(id, range);

                    self.state_machine.process(AstEvent::Declaration {
                        id,
                        name: name_str.clone(),
                        mutable: is_mutable,
                        has_initializer: init.is_some(),
                        type_annotation: type_hint.map(String::from),
                        initializer: init.map(String::from),
                    });

                    // Determine Copy status and generate annotation
                    let copy_status = self.state_machine.get_binding(id)
                        .map(|m| m.is_copy.clone())
                        .unwrap_or(CopyStatus::Unknown);

                    let explanation = match copy_status {
                        CopyStatus::Yes => format!("{}: owned (Copy type)", name_str),
                        CopyStatus::No => format!("{}: owned (move semantics)", name_str),
                        CopyStatus::Unknown => format!("{}: owned", name_str),
                    };

                    self.annotate(id, range, explanation);
                }
            }
            ast::Pat::TuplePat(tuple) => {
                for field in tuple.fields() {
                    self.visit_let_pat(&field, None, None);
                }
            }
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::RefExpr(ref_expr) => {
                let is_mut = ref_expr.mut_token().is_some();
                if let Some(inner) = ref_expr.expr() {
                    if let ast::Expr::PathExpr(path_expr) = &inner {
                        if let Some(binding_id) = self.resolve_path(path_expr) {
                            let range = expr.syntax().text_range();

                            if is_mut {
                                self.state_machine.process(AstEvent::MutBorrow {
                                    borrowed: binding_id,
                                    borrow_binding: None,
                                });
                                self.annotate(binding_id, range, "mutable borrow".to_string());
                            } else {
                                self.state_machine.process(AstEvent::SharedBorrow {
                                    borrowed: binding_id,
                                    borrow_binding: None,
                                });
                                self.annotate(binding_id, range, "shared borrow".to_string());
                            }
                        }
                    }
                    self.visit_expr(&inner);
                }
            }

            ast::Expr::CallExpr(call) => {
                if let Some(callee) = call.expr() {
                    self.visit_expr(&callee);
                }
                if let Some(args) = call.arg_list() {
                    for (idx, arg) in args.args().enumerate() {
                        self.visit_call_arg(&arg, idx);
                    }
                }
            }

            ast::Expr::MethodCallExpr(method) => {
                if let Some(receiver) = method.receiver() {
                    self.visit_call_arg(&receiver, 0);
                }
                if let Some(args) = method.arg_list() {
                    for (idx, arg) in args.args().enumerate() {
                        self.visit_call_arg(&arg, idx + 1);
                    }
                }
            }

            ast::Expr::BlockExpr(block) => {
                self.visit_block_expr(block);
            }

            ast::Expr::IfExpr(if_expr) => {
                if let Some(cond) = if_expr.condition() {
                    self.visit_expr(&cond);
                }
                if let Some(then_branch) = if_expr.then_branch() {
                    self.visit_block_expr(&then_branch);
                }
                if let Some(else_branch) = if_expr.else_branch() {
                    match else_branch {
                        ast::ElseBranch::Block(block) => self.visit_block_expr(&block),
                        ast::ElseBranch::IfExpr(nested) => {
                            self.visit_expr(&ast::Expr::IfExpr(nested))
                        }
                    }
                }
            }

            ast::Expr::LoopExpr(loop_expr) => {
                if let Some(body) = loop_expr.loop_body() {
                    self.visit_block_expr(&body);
                }
            }

            ast::Expr::WhileExpr(while_expr) => {
                if let Some(cond) = while_expr.condition() {
                    self.visit_expr(&cond);
                }
                if let Some(body) = while_expr.loop_body() {
                    self.visit_block_expr(&body);
                }
            }

            ast::Expr::ForExpr(for_expr) => {
                if let Some(iterable) = for_expr.iterable() {
                    self.visit_expr(&iterable);
                }

                let scope = self.state_machine.new_scope_id();
                self.state_machine.process(AstEvent::ScopeEnter { scope });

                if let Some(pat) = for_expr.pat() {
                    self.visit_let_pat(&pat, None, None);
                }
                if let Some(body) = for_expr.loop_body() {
                    self.visit_block_expr(&body);
                }

                self.state_machine.process(AstEvent::ScopeExit { scope, bindings: vec![] });
            }

            ast::Expr::MatchExpr(match_expr) => {
                if let Some(scrutinee) = match_expr.expr() {
                    self.visit_expr(&scrutinee);
                }
                if let Some(arm_list) = match_expr.match_arm_list() {
                    for arm in arm_list.arms() {
                        let scope = self.state_machine.new_scope_id();
                        self.state_machine.process(AstEvent::ScopeEnter { scope });

                        if let Some(pat) = arm.pat() {
                            self.visit_let_pat(&pat, None, None);
                        }
                        if let Some(expr) = arm.expr() {
                            self.visit_expr(&expr);
                        }

                        self.state_machine.process(AstEvent::ScopeExit { scope, bindings: vec![] });
                    }
                }
            }

            ast::Expr::MacroExpr(macro_expr) => {
                self.visit_macro_expr(macro_expr);
            }

            ast::Expr::BinExpr(bin) => {
                if let Some(lhs) = bin.lhs() {
                    self.visit_expr(&lhs);
                }
                if let Some(rhs) = bin.rhs() {
                    self.visit_expr(&rhs);
                }
            }

            ast::Expr::ReturnExpr(ret) => {
                if let Some(expr) = ret.expr() {
                    self.visit_expr(&expr);
                }
            }

            ast::Expr::ClosureExpr(closure) => {
                self.visit_closure(closure);
            }

            _ => {}
        }
    }

    fn visit_call_arg(&mut self, arg: &ast::Expr, arg_index: usize) {
        match arg {
            // Explicit borrow
            ast::Expr::RefExpr(_) => {
                self.visit_expr(arg);
            }

            // Plain variable - possible move
            ast::Expr::PathExpr(path_expr) => {
                if let Some(binding_id) = self.resolve_path(path_expr) {
                    let range = arg.syntax().text_range();
                    let offset = TextSize::from(u32::from(range.start()));

                    // Try Layer 2 first
                    let is_copy = self.query_is_copy(offset);

                    // If oracle says it's Copy, refine the state
                    if let Some(true) = is_copy {
                        self.state_machine.refine_copy_status(binding_id, true);
                        self.annotate(binding_id, range, "copied to call".to_string());
                    } else {
                        // Otherwise emit possible move
                        self.state_machine.process(AstEvent::PossibleMove {
                            binding: binding_id,
                            context: MoveContext::FunctionArg {
                                func_name: None,
                                arg_index,
                            },
                        });

                        let copy_status = self.state_machine.get_binding(binding_id)
                            .map(|m| m.is_copy.clone())
                            .unwrap_or(CopyStatus::Unknown);

                        let explanation = match copy_status {
                            CopyStatus::Yes => "copied to call".to_string(),
                            CopyStatus::No => "moved to call".to_string(),
                            CopyStatus::Unknown => "possibly moved to call".to_string(),
                        };

                        self.annotate(binding_id, range, explanation);
                    }
                }
            }

            _ => self.visit_expr(arg),
        }
    }

    fn visit_macro_expr(&mut self, macro_expr: &ast::MacroExpr) {
        let Some(macro_call) = macro_expr.macro_call() else { return };
        let Some(path) = macro_call.path() else { return };

        let macro_name = path.syntax().text().to_string();

        // Format macros borrow their arguments
        let is_format_macro = matches!(
            macro_name.as_str(),
            "println" | "print" | "eprintln" | "eprint"
            | "format" | "write" | "writeln"
            | "dbg" | "assert" | "assert_eq" | "assert_ne"
        );

        if !is_format_macro {
            return;
        }

        let Some(token_tree) = macro_call.token_tree() else { return };
        let text = token_tree.syntax().text().to_string();

        // Parse inline format args and traditional args
        let mut recorded: std::collections::HashSet<String> = std::collections::HashSet::new();

        // Find {name} patterns in format string
        let mut in_string = false;
        let mut in_placeholder = false;
        let mut ident = String::new();

        for ch in text.chars() {
            match ch {
                '"' => {
                    in_string = !in_string;
                    in_placeholder = false;
                    ident.clear();
                }
                '{' if in_string => {
                    in_placeholder = true;
                    ident.clear();
                }
                '}' if in_placeholder => {
                    if !ident.is_empty() && ident.chars().next().map(|c| c.is_alphabetic() || c == '_').unwrap_or(false) {
                        if !recorded.contains(&ident) {
                            self.record_macro_borrow(&ident, macro_expr, &macro_name);
                            recorded.insert(ident.clone());
                        }
                    }
                    in_placeholder = false;
                    ident.clear();
                }
                ':' if in_placeholder => {
                    if !ident.is_empty() && ident.chars().next().map(|c| c.is_alphabetic() || c == '_').unwrap_or(false) {
                        if !recorded.contains(&ident) {
                            self.record_macro_borrow(&ident, macro_expr, &macro_name);
                            recorded.insert(ident.clone());
                        }
                    }
                    ident.clear();
                    in_placeholder = false;
                }
                c if in_placeholder && (c.is_alphanumeric() || c == '_') => {
                    ident.push(c);
                }
                _ => {}
            }
        }

        // Traditional args after format string
        let mut found_comma = false;
        let mut current = String::new();
        in_string = false;

        for ch in text.chars() {
            match ch {
                '"' => in_string = !in_string,
                ',' if !in_string => {
                    if !current.is_empty() && found_comma && !recorded.contains(&current) {
                        self.record_macro_borrow(&current, macro_expr, &macro_name);
                        recorded.insert(current.clone());
                    }
                    current.clear();
                    found_comma = true;
                }
                c if c.is_alphanumeric() || c == '_' => {
                    if !in_string {
                        current.push(c);
                    }
                }
                _ => {
                    if !current.is_empty() && found_comma && !in_string && !recorded.contains(&current) {
                        self.record_macro_borrow(&current, macro_expr, &macro_name);
                        recorded.insert(current.clone());
                    }
                    current.clear();
                }
            }
        }

        if !current.is_empty() && found_comma && !recorded.contains(&current) {
            self.record_macro_borrow(&current, macro_expr, &macro_name);
        }
    }

    fn record_macro_borrow(&mut self, name: &str, macro_expr: &ast::MacroExpr, macro_name: &str) {
        if let Some(binding_id) = self.state_machine.lookup(name) {
            let range = macro_expr.syntax().text_range();

            self.state_machine.process(AstEvent::SharedBorrow {
                borrowed: binding_id,
                borrow_binding: None,
            });

            self.annotate(binding_id, range, format!("borrowed by {}!", macro_name));
        }
    }

    fn visit_closure(&mut self, closure: &ast::ClosureExpr) {
        let is_move = closure.move_token().is_some();
        let range = closure.syntax().text_range();

        let scope = self.state_machine.new_scope_id();
        self.state_machine.process(AstEvent::ScopeEnter { scope });

        // Process parameters
        if let Some(param_list) = closure.param_list() {
            for param in param_list.params() {
                let type_str = param.ty().map(|ty| ty.syntax().text().to_string());
                if let Some(pat) = param.pat() {
                    self.visit_param(&pat, type_str.as_deref());
                }
            }
        }

        // For move closures, captured variables are moved
        if is_move {
            if let Some(body) = closure.body() {
                let captures = self.collect_captures(&body);
                for (name, binding_id) in captures {
                    self.state_machine.process(AstEvent::PossibleMove {
                        binding: binding_id,
                        context: MoveContext::Macro { macro_name: "move closure".to_string() },
                    });
                    self.annotate(binding_id, range, format!("{}: moved into closure", name));
                }
            }
        }

        if let Some(body) = closure.body() {
            self.visit_expr(&body);
        }

        self.state_machine.process(AstEvent::ScopeExit { scope, bindings: vec![] });
    }

    fn collect_captures(&self, expr: &ast::Expr) -> Vec<(String, BindingId)> {
        let mut captures = Vec::new();
        self.collect_captures_recursive(expr, &mut captures);
        captures
    }

    fn collect_captures_recursive(&self, expr: &ast::Expr, captures: &mut Vec<(String, BindingId)>) {
        match expr {
            ast::Expr::PathExpr(path_expr) => {
                if let Some(id) = self.resolve_path(path_expr) {
                    if let Some(meta) = self.state_machine.get_binding(id) {
                        // Check if from outer scope
                        if meta.scope != self.state_machine.current_scope() {
                            if !captures.iter().any(|(_, bid)| *bid == id) {
                                captures.push((meta.name.clone(), id));
                            }
                        }
                    }
                }
            }
            ast::Expr::BlockExpr(block) => {
                for stmt in block.statements() {
                    if let ast::Stmt::ExprStmt(es) = stmt {
                        if let Some(e) = es.expr() {
                            self.collect_captures_recursive(&e, captures);
                        }
                    }
                }
                if let Some(tail) = block.tail_expr() {
                    self.collect_captures_recursive(&tail, captures);
                }
            }
            ast::Expr::CallExpr(call) => {
                if let Some(e) = call.expr() {
                    self.collect_captures_recursive(&e, captures);
                }
                if let Some(args) = call.arg_list() {
                    for arg in args.args() {
                        self.collect_captures_recursive(&arg, captures);
                    }
                }
            }
            ast::Expr::MethodCallExpr(method) => {
                if let Some(recv) = method.receiver() {
                    self.collect_captures_recursive(&recv, captures);
                }
                if let Some(args) = method.arg_list() {
                    for arg in args.args() {
                        self.collect_captures_recursive(&arg, captures);
                    }
                }
            }
            ast::Expr::RefExpr(ref_expr) => {
                if let Some(inner) = ref_expr.expr() {
                    self.collect_captures_recursive(&inner, captures);
                }
            }
            ast::Expr::BinExpr(bin) => {
                if let Some(lhs) = bin.lhs() {
                    self.collect_captures_recursive(&lhs, captures);
                }
                if let Some(rhs) = bin.rhs() {
                    self.collect_captures_recursive(&rhs, captures);
                }
            }
            _ => {}
        }
    }

    fn resolve_path(&self, path_expr: &ast::PathExpr) -> Option<BindingId> {
        let path = path_expr.path()?;
        if path.qualifier().is_some() {
            return None;
        }
        let segment = path.segment()?;
        let name_ref = segment.name_ref()?;
        let name = name_ref.text().to_string();
        self.state_machine.lookup(&name)
    }
}

/// A no-op oracle for when Layer 2 is not available.
pub struct NoOpOracle;

impl TypeOracle for NoOpOracle {
    fn is_copy_at(&self, _offset: TextSize) -> Option<bool> {
        None
    }

    fn param_mode(&self, _call_offset: TextSize, _arg_index: usize) -> Option<super::layers::ParamMode> {
        None
    }

    fn is_type_copy(&self, _type_name: &str) -> Option<bool> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_layered_basic() {
        let source = r#"
fn main() {
    let x = 42;
}
"#;
        let mut analyzer = LayeredAnalyzer::<NoOpOracle>::new(None);
        let annotations = analyzer.analyze(source);

        assert!(!annotations.is_empty());
        assert!(annotations.iter().any(|a| a.binding == "x"));
    }

    #[test]
    fn test_layered_borrow() {
        let source = r#"
fn main() {
    let mut x = vec![1, 2, 3];
    let r = &x;
}
"#;
        let mut analyzer = LayeredAnalyzer::<NoOpOracle>::new(None);
        let annotations = analyzer.analyze(source);

        // Should have borrow annotation
        assert!(annotations.iter().any(|a| a.explanation.contains("borrow")));
    }
}
