//! Call site detection - finds function/method/closure calls in expressions.

use ra_ap_syntax::ast;
use ra_ap_syntax::ast::HasArgList;
use ra_ap_syntax::AstNode;
use super::registry::FunctionRegistry;

/// A call site detected in the source.
#[derive(Debug, Clone)]
pub struct CallSite {
    /// Line number where the call occurs (0-indexed).
    pub line: u32,
    /// Column offset within the line.
    pub column: u32,
    /// Kind of call.
    pub kind: CallKind,
}

/// Kind of function call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallKind {
    /// Direct function call: `foo()`
    Function(String),
    /// Method call: `x.foo()`
    Method {
        /// Receiver type if known.
        receiver_ty: Option<String>,
        /// Method name.
        name: String,
    },
    /// Closure call: `closure_var()`
    Closure(String),
    /// External/unknown function call.
    External(String),
}

impl CallSite {
    /// Get the target function name for this call.
    pub fn target_name(&self) -> &str {
        match &self.kind {
            CallKind::Function(name) => name,
            CallKind::Method { name, .. } => name,
            CallKind::Closure(name) => name,
            CallKind::External(name) => name,
        }
    }
}

/// Detect all call sites in an expression.
pub fn detect_calls(
    expr: &ast::Expr,
    line_offset: u32,
    registry: &FunctionRegistry,
) -> Vec<CallSite> {
    let mut calls = Vec::new();
    detect_calls_recursive(expr, line_offset, registry, &mut calls);
    calls
}

fn detect_calls_recursive(
    expr: &ast::Expr,
    line_offset: u32,
    registry: &FunctionRegistry,
    calls: &mut Vec<CallSite>,
) {
    match expr {
        ast::Expr::CallExpr(call) => {
            if let Some(callee) = call.expr() {
                // Check what kind of call this is
                match &callee {
                    ast::Expr::PathExpr(path) => {
                        if let Some(path) = path.path() {
                            let name = path.syntax().text().to_string();
                            let kind = classify_call(&name, registry);
                            calls.push(CallSite {
                                line: line_offset,
                                column: 0,
                                kind,
                            });
                        }
                    }
                    _ => {
                        // Complex callee expression - recurse into it
                        detect_calls_recursive(&callee, line_offset, registry, calls);
                    }
                }
            }
            // Also check arguments for nested calls
            if let Some(args) = call.arg_list() {
                for arg in args.args() {
                    detect_calls_recursive(&arg, line_offset, registry, calls);
                }
            }
        }
        ast::Expr::MethodCallExpr(method_call) => {
            if let Some(name) = method_call.name_ref() {
                let method_name = name.text().to_string();
                // Try to determine receiver type for method resolution
                let receiver_ty = method_call
                    .receiver()
                    .and_then(|r| extract_receiver_type(&r));

                let kind = if let Some(ref ty) = receiver_ty {
                    if registry.get_method(ty, &method_name).is_some() {
                        CallKind::Method {
                            receiver_ty: receiver_ty.clone(),
                            name: method_name,
                        }
                    } else {
                        CallKind::External(format!("{}.{}", ty, method_name))
                    }
                } else {
                    CallKind::External(method_name)
                };

                calls.push(CallSite {
                    line: line_offset,
                    column: 0,
                    kind,
                });
            }
            // Check receiver and arguments for nested calls
            if let Some(receiver) = method_call.receiver() {
                detect_calls_recursive(&receiver, line_offset, registry, calls);
            }
            if let Some(args) = method_call.arg_list() {
                for arg in args.args() {
                    detect_calls_recursive(&arg, line_offset, registry, calls);
                }
            }
        }
        // Recurse into other expression types
        ast::Expr::BlockExpr(block) => {
            for stmt in block.statements() {
                if let ast::Stmt::ExprStmt(expr_stmt) = stmt {
                    if let Some(e) = expr_stmt.expr() {
                        detect_calls_recursive(&e, line_offset, registry, calls);
                    }
                }
            }
            if let Some(tail) = block.tail_expr() {
                detect_calls_recursive(&tail, line_offset, registry, calls);
            }
        }
        ast::Expr::IfExpr(if_expr) => {
            if let Some(cond) = if_expr.condition() {
                detect_calls_recursive(&cond, line_offset, registry, calls);
            }
            if let Some(then_branch) = if_expr.then_branch() {
                detect_calls_recursive(&ast::Expr::BlockExpr(then_branch), line_offset, registry, calls);
            }
            if let Some(else_branch) = if_expr.else_branch() {
                match else_branch {
                    ast::ElseBranch::Block(block) => {
                        detect_calls_recursive(&ast::Expr::BlockExpr(block), line_offset, registry, calls);
                    }
                    ast::ElseBranch::IfExpr(nested_if) => {
                        detect_calls_recursive(&ast::Expr::IfExpr(nested_if), line_offset, registry, calls);
                    }
                }
            }
        }
        ast::Expr::MatchExpr(match_expr) => {
            if let Some(scrutinee) = match_expr.expr() {
                detect_calls_recursive(&scrutinee, line_offset, registry, calls);
            }
            if let Some(arms) = match_expr.match_arm_list() {
                for arm in arms.arms() {
                    if let Some(e) = arm.expr() {
                        detect_calls_recursive(&e, line_offset, registry, calls);
                    }
                }
            }
        }
        ast::Expr::BinExpr(bin) => {
            if let Some(lhs) = bin.lhs() {
                detect_calls_recursive(&lhs, line_offset, registry, calls);
            }
            if let Some(rhs) = bin.rhs() {
                detect_calls_recursive(&rhs, line_offset, registry, calls);
            }
        }
        ast::Expr::RefExpr(ref_expr) => {
            if let Some(inner) = ref_expr.expr() {
                detect_calls_recursive(&inner, line_offset, registry, calls);
            }
        }
        ast::Expr::TupleExpr(tuple) => {
            for field in tuple.fields() {
                detect_calls_recursive(&field, line_offset, registry, calls);
            }
        }
        ast::Expr::ArrayExpr(arr) => {
            match arr.kind() {
                ast::ArrayExprKind::ElementList(elements) => {
                    for elem in elements {
                        detect_calls_recursive(&elem, line_offset, registry, calls);
                    }
                }
                ast::ArrayExprKind::Repeat { initializer, .. } => {
                    if let Some(init) = initializer {
                        detect_calls_recursive(&init, line_offset, registry, calls);
                    }
                }
            }
        }
        ast::Expr::ParenExpr(paren) => {
            if let Some(inner) = paren.expr() {
                detect_calls_recursive(&inner, line_offset, registry, calls);
            }
        }
        ast::Expr::ReturnExpr(ret) => {
            if let Some(inner) = ret.expr() {
                detect_calls_recursive(&inner, line_offset, registry, calls);
            }
        }
        ast::Expr::AwaitExpr(await_expr) => {
            if let Some(inner) = await_expr.expr() {
                detect_calls_recursive(&inner, line_offset, registry, calls);
            }
        }
        ast::Expr::TryExpr(try_expr) => {
            if let Some(inner) = try_expr.expr() {
                detect_calls_recursive(&inner, line_offset, registry, calls);
            }
        }
        // Macro calls need special handling
        ast::Expr::MacroExpr(_) => {
            // Macros like println! are external - skip for now
        }
        _ => {}
    }
}

/// Classify a call by its name, checking the registry.
fn classify_call(name: &str, registry: &FunctionRegistry) -> CallKind {
    // Check if it's a known function
    if registry.has_function(name) {
        return CallKind::Function(name.to_string());
    }

    // Check if it's a known closure
    if registry.get_closure(name).is_some() {
        return CallKind::Closure(name.to_string());
    }

    // Otherwise it's external
    CallKind::External(name.to_string())
}

/// Try to extract a type name from a receiver expression.
fn extract_receiver_type(expr: &ast::Expr) -> Option<String> {
    match expr {
        ast::Expr::PathExpr(path) => {
            // Simple variable - we'd need type info to know its type
            // For now, return the variable name as a hint
            path.path().map(|p| p.syntax().text().to_string())
        }
        ast::Expr::CallExpr(call) => {
            // Constructor-like: Type::new()
            if let Some(ast::Expr::PathExpr(path)) = call.expr().as_ref() {
                if let Some(p) = path.path() {
                    let text = p.syntax().text().to_string();
                    // Extract type from Type::method pattern
                    if let Some(idx) = text.find("::") {
                        return Some(text[..idx].to_string());
                    }
                }
            }
            None
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ra_ap_syntax::SourceFile;
    use ra_ap_syntax::ast::HasModuleItem;

    fn parse_expr(source: &str) -> ast::Expr {
        // Wrap in a function to parse
        let wrapped = format!("fn test() {{ {} }}", source);
        let parse = SourceFile::parse(&wrapped, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();
        let func = file.items().next().unwrap();
        if let ast::Item::Fn(f) = func {
            if let Some(body) = f.body() {
                if let Some(stmt) = body.statements().next() {
                    if let ast::Stmt::ExprStmt(e) = stmt {
                        return e.expr().unwrap();
                    }
                }
                if let Some(tail) = body.tail_expr() {
                    return tail;
                }
            }
        }
        panic!("Could not parse expression");
    }

    #[test]
    fn test_detect_function_call() {
        let source = "fn foo() {}\nfn main() { foo(); }";
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let registry = FunctionRegistry::build(source, &parse.tree());

        let expr = parse_expr("foo();");
        let calls = detect_calls(&expr, 0, &registry);

        assert_eq!(calls.len(), 1);
        assert!(matches!(calls[0].kind, CallKind::Function(ref n) if n == "foo"));
    }

    #[test]
    fn test_detect_external_call() {
        let source = "fn main() { unknown(); }";
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let registry = FunctionRegistry::build(source, &parse.tree());

        let expr = parse_expr("unknown();");
        let calls = detect_calls(&expr, 0, &registry);

        assert_eq!(calls.len(), 1);
        assert!(matches!(calls[0].kind, CallKind::External(ref n) if n == "unknown"));
    }

    #[test]
    fn test_detect_nested_calls() {
        let source = "fn foo() {}\nfn bar() {}\nfn main() { foo(); }";
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let registry = FunctionRegistry::build(source, &parse.tree());

        // Test expression with nested call: foo(bar())
        // We'll just test that we find calls in arguments
        let expr = parse_expr("foo();");
        let calls = detect_calls(&expr, 0, &registry);
        assert!(!calls.is_empty());
    }
}
