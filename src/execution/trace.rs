//! Per-function analysis support.
//!
//! Instead of simulating execution order, this module provides utilities
//! for analyzing each function independently - matching how the borrow
//! checker actually works (static analysis, not execution).

use ra_ap_syntax::SourceFile;
use super::registry::FunctionRegistry;

/// Information about a function for per-function stepping.
#[derive(Debug, Clone)]
pub struct FunctionView {
    /// Function name.
    pub name: String,
    /// Line range (0-indexed, inclusive).
    pub line_range: (u32, u32),
    /// Number of lines in this function.
    pub line_count: u32,
}

impl FunctionView {
    /// Create views for all functions in the source.
    pub fn from_source(source: &str) -> Vec<FunctionView> {
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();
        let registry = FunctionRegistry::build(source, &file);

        let mut views: Vec<_> = registry
            .all_functions()
            .map(|info| FunctionView {
                name: info.name.clone(),
                line_range: info.line_range,
                line_count: info.line_range.1 - info.line_range.0 + 1,
            })
            .collect();

        // Sort by line number (file order)
        views.sort_by_key(|v| v.line_range.0);
        views
    }

    /// Get the lines belonging to this function (0-indexed).
    pub fn lines(&self) -> impl Iterator<Item = u32> {
        self.line_range.0..=self.line_range.1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_views() {
        let source = r#"fn foo() {
    let x = 1;
}

fn main() {
    foo();
}
"#;
        let views = FunctionView::from_source(source);

        assert_eq!(views.len(), 2);
        assert_eq!(views[0].name, "foo");
        assert_eq!(views[1].name, "main");
    }

    #[test]
    fn test_function_lines() {
        let source = r#"fn main() {
    let x = 1;
    let y = 2;
}
"#;
        let views = FunctionView::from_source(source);

        assert_eq!(views.len(), 1);
        let lines: Vec<_> = views[0].lines().collect();
        assert_eq!(lines, vec![0, 1, 2, 3]);
    }
}
