//! Renderer traits - separated by output category.

use super::context::RenderContext;

/// Trait for renderers that produce valid Rust output.
///
/// Implementations must ensure output can be compiled by rustc.
/// This typically means annotations are placed inside `//` comments.
pub trait ValidRustRenderer {
    /// Render source with annotations as valid Rust.
    fn render(&self, ctx: &mut RenderContext) -> String;
}

/// Trait for renderers that produce rich text output.
///
/// Implementations have full typographical freedom - box drawing,
/// diagrams, custom layouts. Output is for display only, not compilation.
pub trait RichTextRenderer {
    /// Render source with rich text annotations.
    fn render(&self, ctx: &mut RenderContext) -> String;
}
