//! LSP-specific output formatting.

mod hover;
mod inlay_hints;

pub use hover::format_hover;
pub use inlay_hints::format_inlay_hints;
