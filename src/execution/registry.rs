//! Function registry - maps callable items to their source locations.
//!
//! Walks the AST to collect:
//! - Top-level `fn` items
//! - `impl` blocks and their methods
//! - Closure expressions bound to variables

use std::collections::HashMap;
use ra_ap_syntax::{ast, AstNode, SourceFile, SyntaxNode, TextRange};
use ra_ap_syntax::ast::{HasModuleItem, HasName};

/// Information about a callable function/method/closure.
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    /// Name of the function.
    pub name: String,
    /// Line range (start, end) in source (0-indexed).
    pub line_range: (u32, u32),
    /// Kind of callable.
    pub kind: FunctionKind,
    /// The syntax node for the function body.
    pub body_range: Option<TextRange>,
}

/// Kind of callable item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionKind {
    /// Free function: `fn foo()`
    Free,
    /// Method in impl block: `impl T { fn foo() }`
    Method {
        /// The type this method is implemented for.
        impl_type: String,
    },
    /// Closure bound to a variable: `let f = || { ... }`
    Closure {
        /// Line where the closure is defined.
        defined_at: u32,
    },
}

/// Registry of all callable items in a source file.
#[derive(Debug, Default)]
pub struct FunctionRegistry {
    /// Free functions: name → info
    pub functions: HashMap<String, FunctionInfo>,
    /// Methods: (type_name, method_name) → info
    pub methods: HashMap<(String, String), FunctionInfo>,
    /// Closures: variable_name → info (in current scope)
    pub closures: HashMap<String, FunctionInfo>,
    /// Source text for line number calculation.
    line_starts: Vec<u32>,
}

impl FunctionRegistry {
    /// Build a registry from a source file.
    pub fn build(source: &str, file: &SourceFile) -> Self {
        let mut registry = Self {
            functions: HashMap::new(),
            methods: HashMap::new(),
            closures: HashMap::new(),
            line_starts: compute_line_starts(source),
        };

        registry.visit_source_file(file);
        registry
    }

    /// Check if a function name exists in the registry.
    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Get a function by name.
    pub fn get_function(&self, name: &str) -> Option<&FunctionInfo> {
        self.functions.get(name)
    }

    /// Get a method by type and name.
    pub fn get_method(&self, type_name: &str, method_name: &str) -> Option<&FunctionInfo> {
        self.methods.get(&(type_name.to_string(), method_name.to_string()))
    }

    /// Get a closure by variable name.
    pub fn get_closure(&self, var_name: &str) -> Option<&FunctionInfo> {
        self.closures.get(var_name)
    }

    /// Iterate over all functions (free functions only, not methods or closures).
    pub fn all_functions(&self) -> impl Iterator<Item = &FunctionInfo> {
        self.functions.values()
    }

    /// Iterate over all callables (functions, methods, closures).
    pub fn all_callables(&self) -> impl Iterator<Item = &FunctionInfo> {
        self.functions
            .values()
            .chain(self.methods.values())
            .chain(self.closures.values())
    }

    /// Convert a byte offset to a line number (0-indexed).
    fn offset_to_line(&self, offset: u32) -> u32 {
        match self.line_starts.binary_search(&offset) {
            Ok(line) => line as u32,
            Err(line) => line.saturating_sub(1) as u32,
        }
    }

    /// Get line range for a syntax node.
    fn node_line_range(&self, node: &SyntaxNode) -> (u32, u32) {
        let range = node.text_range();
        let start = self.offset_to_line(range.start().into());
        let end = self.offset_to_line(range.end().into());
        (start, end)
    }

    /// Visit the source file to collect all callables.
    fn visit_source_file(&mut self, file: &SourceFile) {
        for item in file.items() {
            self.visit_item(&item);
        }
    }

    /// Visit a top-level item.
    fn visit_item(&mut self, item: &ast::Item) {
        match item {
            ast::Item::Fn(f) => self.visit_fn(f, None),
            ast::Item::Impl(impl_block) => self.visit_impl(impl_block),
            _ => {}
        }
    }

    /// Visit a function definition.
    fn visit_fn(&mut self, f: &ast::Fn, impl_type: Option<&str>) {
        let Some(name) = f.name() else { return };
        let name_str = name.text().to_string();
        let line_range = self.node_line_range(f.syntax());
        let body_range = f.body().map(|b| b.syntax().text_range());

        let kind = match impl_type {
            Some(ty) => FunctionKind::Method {
                impl_type: ty.to_string(),
            },
            None => FunctionKind::Free,
        };

        let info = FunctionInfo {
            name: name_str.clone(),
            line_range,
            kind: kind.clone(),
            body_range,
        };

        match &kind {
            FunctionKind::Free => {
                self.functions.insert(name_str, info);
            }
            FunctionKind::Method { impl_type } => {
                self.methods.insert((impl_type.clone(), name_str), info);
            }
            FunctionKind::Closure { .. } => unreachable!(),
        }

        // Also visit the function body for closures
        if let Some(body) = f.body() {
            self.visit_block_for_closures(&body);
        }
    }

    /// Visit an impl block.
    fn visit_impl(&mut self, impl_block: &ast::Impl) {
        // Get the type name
        let type_name = impl_block
            .self_ty()
            .map(|ty| ty.syntax().text().to_string())
            .unwrap_or_else(|| "Unknown".to_string());

        // Visit all methods in the impl
        if let Some(assoc_items) = impl_block.assoc_item_list() {
            for item in assoc_items.assoc_items() {
                if let ast::AssocItem::Fn(f) = item {
                    self.visit_fn(&f, Some(&type_name));
                }
            }
        }
    }

    /// Visit a block expression looking for closure definitions.
    fn visit_block_for_closures(&mut self, block: &ast::BlockExpr) {
        for stmt in block.statements() {
            if let ast::Stmt::LetStmt(let_stmt) = stmt {
                self.visit_let_for_closure(&let_stmt);
            }
        }
    }

    /// Visit a let statement looking for closure assignment.
    fn visit_let_for_closure(&mut self, let_stmt: &ast::LetStmt) {
        // Check if the initializer is a closure
        let Some(init) = let_stmt.initializer() else {
            return;
        };

        // Get the variable name from the pattern
        let Some(pat) = let_stmt.pat() else { return };
        let var_name = match pat {
            ast::Pat::IdentPat(ident) => ident.name().map(|n| n.text().to_string()),
            _ => None,
        };
        let Some(var_name) = var_name else { return };

        // Check if it's a closure
        if let ast::Expr::ClosureExpr(closure) = init {
            let line_range = self.node_line_range(closure.syntax());
            let body_range = closure.body().map(|b| b.syntax().text_range());

            let info = FunctionInfo {
                name: var_name.clone(),
                line_range,
                kind: FunctionKind::Closure {
                    defined_at: line_range.0,
                },
                body_range,
            };

            self.closures.insert(var_name, info);
        }
    }
}

/// Compute line start offsets for a source string.
fn compute_line_starts(source: &str) -> Vec<u32> {
    let mut starts = vec![0];
    for (i, c) in source.char_indices() {
        if c == '\n' {
            starts.push((i + 1) as u32);
        }
    }
    starts
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_and_build(source: &str) -> FunctionRegistry {
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        FunctionRegistry::build(source, &parse.tree())
    }

    #[test]
    fn test_free_functions() {
        let source = r#"
fn foo() {}
fn bar() {}
"#;
        let registry = parse_and_build(source);
        assert!(registry.has_function("foo"));
        assert!(registry.has_function("bar"));
        assert!(!registry.has_function("baz"));
    }

    #[test]
    fn test_impl_methods() {
        let source = r#"
struct Point { x: i32, y: i32 }

impl Point {
    fn new() -> Self { Point { x: 0, y: 0 } }
    fn move_to(&mut self, x: i32, y: i32) {}
}
"#;
        let registry = parse_and_build(source);
        assert!(registry.get_method("Point", "new").is_some());
        assert!(registry.get_method("Point", "move_to").is_some());
        assert!(registry.get_method("Point", "unknown").is_none());
    }

    #[test]
    fn test_closures() {
        let source = r#"
fn main() {
    let add = |a, b| a + b;
    let mul = |a, b| a * b;
}
"#;
        let registry = parse_and_build(source);
        assert!(registry.get_closure("add").is_some());
        assert!(registry.get_closure("mul").is_some());
    }

    #[test]
    fn test_function_line_range() {
        let source = r#"fn foo() {
    let x = 1;
    let y = 2;
}
"#;
        let registry = parse_and_build(source);
        let foo = registry.get_function("foo").unwrap();
        assert_eq!(foo.line_range, (0, 3));
    }
}
