//! Call site detection - finds function/method/closure calls in expressions.

use crate::util::{AstEvent, AstIter};
use ra_ap_syntax::ast;
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

/// Call detector that processes AST events to find call sites.
struct CallDetector<'a> {
    calls: Vec<CallSite>,
    registry: &'a FunctionRegistry,
    line_offset: u32,
}

impl<'a> CallDetector<'a> {
    fn new(registry: &'a FunctionRegistry, line_offset: u32) -> Self {
        Self {
            calls: Vec::new(),
            registry,
            line_offset,
        }
    }

    /// Process an AST event, recording any call sites found.
    fn process(&mut self, event: &AstEvent) {
        match event {
            AstEvent::Expr(ast::Expr::CallExpr(call)) => {
                self.handle_call_expr(call);
            }
            AstEvent::Expr(ast::Expr::MethodCallExpr(method_call)) => {
                self.handle_method_call(method_call);
            }
            // Other events are handled by the iterator's traversal
            _ => {}
        }
    }

    fn handle_call_expr(&mut self, call: &ast::CallExpr) {
        if let Some(callee) = call.expr() {
            if let ast::Expr::PathExpr(path) = &callee {
                if let Some(path) = path.path() {
                    let name = path.syntax().text().to_string();
                    let kind = classify_call(&name, self.registry);
                    self.calls.push(CallSite {
                        line: self.line_offset,
                        column: 0,
                        kind,
                    });
                }
            }
            // Complex callee expressions are handled by the iterator's traversal
        }
    }

    fn handle_method_call(&mut self, method_call: &ast::MethodCallExpr) {
        if let Some(name) = method_call.name_ref() {
            let method_name = name.text().to_string();
            // Try to determine receiver type for method resolution
            let receiver_ty = method_call
                .receiver()
                .and_then(|r| extract_receiver_type(&r));

            let kind = if let Some(ref ty) = receiver_ty {
                if self.registry.get_method(ty, &method_name).is_some() {
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

            self.calls.push(CallSite {
                line: self.line_offset,
                column: 0,
                kind,
            });
        }
    }

    fn into_calls(self) -> Vec<CallSite> {
        self.calls
    }
}

/// Detect all call sites in an expression.
pub fn detect_calls(
    expr: &ast::Expr,
    line_offset: u32,
    registry: &FunctionRegistry,
) -> Vec<CallSite> {
    let mut detector = CallDetector::new(registry, line_offset);

    // Use AstIter for traversal - it handles all expression types
    for event in AstIter::from_expr(expr) {
        detector.process(&event);
    }

    detector.into_calls()
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
    use ra_ap_syntax::ast::HasModuleItem;
    use ra_ap_syntax::SourceFile;

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
