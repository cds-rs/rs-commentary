//! Output formatting for ownership annotations.
//!
//! This module provides a pluggable renderer framework for displaying
//! ownership state annotations in various formats.
//!
//! # Available Styles
//!
//! | Style | Description |
//! |-------|-------------|
//! | `diagnostic` | Rustc-style with line numbers, underlines (default) |
//! | `inline` | Comments at end of each line |
//! | `html` | Interactive HTML with hover tooltips |
//! | `columnar` | Fixed columns per variable |
//! | `grouped` | Horizontal rules between state changes |
//!
//! # Renderer Traits
//!
//! Two traits define the renderer interface:
//!
//! - [`ValidRustRenderer`] - Produces valid Rust (comments only)
//! - [`RichTextRenderer`] - Produces formatted output (may not be valid Rust)
//!
//! # Adding a New Renderer
//!
//! 1. Implement [`ValidRustRenderer`] or [`RichTextRenderer`]
//! 2. Add variant to [`RenderStyle`] enum in `types.rs`
//! 3. Add case in `render_source_semantic()` match
//!
//! # Module Structure
//!
//! - `types` - Core enums ([`RenderStyle`], [`RenderConfig`])
//! - `context` - [`RenderContext`] aggregates data for renderers
//! - `traits` - Renderer trait definitions
//! - `helpers` - Shared formatting functions
//! - [`renderers`] - All renderer implementations
//! - [`lsp`] - LSP-specific formatters (hover, inlay hints)

mod context;
mod helpers;
mod render;
mod traits;
mod types;

pub mod lsp;
pub mod renderers;

// Re-export public types
pub use context::RenderContext;
pub use render::render_source_semantic;
pub use renderers::{
    ColumnarRenderer, DiagnosticRenderer, GroupedRenderer, HtmlRenderer, InlineRenderer,
    SetNotationRenderer, ValidatedRenderContext, ValidatedRenderer, VerticalSpansRenderer,
};
pub use traits::{RichTextRenderer, ValidRustRenderer};
pub use types::{RenderCategory, RenderConfig, RenderStyle};

// Re-export LSP formatters
pub use lsp::{format_hover, format_inlay_hints};
