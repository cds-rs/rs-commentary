//! Output renderers for ownership annotations.

mod diagnostic;
mod html;
mod set_notation;
mod valid_rust;
mod validated;
mod vertical_spans;

pub use diagnostic::DiagnosticRenderer;
pub use html::HtmlRenderer;
pub use set_notation::SetNotationRenderer;
pub use valid_rust::{ColumnarRenderer, GroupedRenderer, InlineRenderer};
pub use validated::{ValidatedRenderContext, ValidatedRenderer};
pub use vertical_spans::VerticalSpansRenderer;
