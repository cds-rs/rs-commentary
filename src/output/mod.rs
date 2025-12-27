//! Output formatting for ownership annotations.
//!
//! Provides pluggable renderers for different output styles.

mod hover;
mod inlay_hints;
mod renderer;

pub use hover::format_hover;
pub use inlay_hints::format_inlay_hints;
pub use renderer::{render_source, RenderConfig, RenderStyle, Renderer};
