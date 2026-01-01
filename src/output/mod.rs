//! Output formatting for ownership annotations.
//!
//! ## Module Structure
//!
//! - `types` - Core enums and config
//! - `context` - RenderContext for renderers
//! - `traits` - ValidRustRenderer, RichTextRenderer
//! - `helpers` - Shared formatting functions
//! - `renderers/` - All renderer implementations
//! - `lsp/` - LSP-specific formatters (hover, inlay hints)
//! - `render` - Top-level render functions

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
