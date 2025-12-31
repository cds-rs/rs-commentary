//! Output formatting for ownership annotations.
//!
//! Provides pluggable renderers for different output styles.
//!
//! ## Module Structure
//!
//! - `types` - Core enums and config
//! - `context` - RenderContext for renderers
//! - `traits` - ValidRustRenderer, RichTextRenderer
//! - `helpers` - Shared formatting functions
//! - `valid_rust` - Inline, Columnar, Grouped renderers
//! - `diagnostic` - Rustc-style diagnostic renderer
//! - `set_notation` - Set notation renderer
//! - `vertical_spans` - Vertical borrow spans renderer
//! - `html` - Interactive HTML renderer
//! - `validated` - Rust-analyzer validated renderer

mod context;
mod diagnostic;
mod helpers;
mod hover;
mod html;
mod inlay_hints;
mod set_notation;
mod traits;
mod types;
mod valid_rust;
mod validated;
mod vertical_spans;

// Re-export public types
pub use context::RenderContext;
pub use hover::format_hover;
pub use inlay_hints::format_inlay_hints;
pub use traits::{RichTextRenderer, ValidRustRenderer};
pub use types::{RenderCategory, RenderConfig, RenderStyle};
pub use validated::{ValidatedRenderContext, ValidatedRenderer};

// Re-export renderers
pub use diagnostic::DiagnosticRenderer;
pub use html::HtmlRenderer;
pub use set_notation::SetNotationRenderer;
pub use valid_rust::{ColumnarRenderer, GroupedRenderer, InlineRenderer};
pub use vertical_spans::VerticalSpansRenderer;

use crate::analysis::{OwnershipAnalyzer, SemanticAnalyzer, SemanticResult};
use std::collections::HashMap;
use std::path::Path;

/// Render source with the specified style.
///
/// Returns annotated source code. Check `style.category()` to determine
/// whether the output is valid Rust or rich text.
///
/// Note: For `RenderStyle::Validated`, use `render_source_semantic` instead
/// which provides rust-analyzer diagnostics.
pub fn render_source(source: &str, style: RenderStyle, config: RenderConfig) -> String {
    let mut analyzer = OwnershipAnalyzer::new();

    let Ok(_) = analyzer.analyze(source) else {
        return source.to_string();
    };

    let set_annotations = analyzer.set_annotations();
    let mut ctx = RenderContext::new(source, set_annotations, config);

    match style {
        // ValidRustRenderer implementations
        RenderStyle::Inline => ValidRustRenderer::render(&InlineRenderer, &mut ctx),
        RenderStyle::Columnar => ValidRustRenderer::render(&ColumnarRenderer, &mut ctx),
        RenderStyle::Grouped => ValidRustRenderer::render(&GroupedRenderer, &mut ctx),
        // RichTextRenderer implementations
        RenderStyle::Diagnostic => RichTextRenderer::render(&DiagnosticRenderer, &mut ctx),
        RenderStyle::SetNotation => RichTextRenderer::render(&SetNotationRenderer, &mut ctx),
        RenderStyle::VerticalSpans => RichTextRenderer::render(&VerticalSpansRenderer, &mut ctx),
        RenderStyle::Html => RichTextRenderer::render(&HtmlRenderer, &mut ctx),
        // Validated requires semantic analysis - fallback to SetNotation
        RenderStyle::Validated => {
            // Without file path, we can't load rust-analyzer
            // Fall back to SetNotation with a warning
            let mut output = String::from(
                "╭─ rust-analyzer: ⚠ semantic analysis requires file path\n│  use render_source_semantic() instead\n│\n",
            );
            output.push_str(&RichTextRenderer::render(&SetNotationRenderer, &mut ctx));
            output
        }
    }
}

/// Render source with semantic analysis from rust-analyzer.
///
/// This loads the cargo workspace and provides actual compiler diagnostics
/// alongside ownership state annotations. Uses accurate NLL drop detection
/// based on rust-analyzer's reference analysis.
///
/// Returns `None` if the file is not part of a cargo project or loading fails.
pub fn render_source_semantic(
    file_path: &Path,
    source: &str,
    style: RenderStyle,
    config: RenderConfig,
) -> Option<String> {
    // Load semantic analysis
    let analyzer = match SemanticAnalyzer::load(file_path) {
        SemanticResult::Available(a) => a,
        SemanticResult::NotCargoProject => return None,
        SemanticResult::LoadFailed => return None,
    };

    let file_id = analyzer.file_id(file_path)?;
    let diagnostics = analyzer.diagnostics(file_id);

    // Get semantic last-use data for accurate NLL drops
    let last_use_info = analyzer.find_all_last_uses(file_id, source);

    // Extract drop lines: line AFTER last use (already computed by semantic analysis)
    // Key is (name, decl_line) to disambiguate variables with the same name in different scopes
    let drop_lines: HashMap<(String, u32), u32> = last_use_info
        .values()
        .map(|info| ((info.name.clone(), info.decl_line), info.drop_line))
        .collect();

    // Extract Copy type info for accurate filtering
    let copy_types: HashMap<String, bool> = last_use_info
        .values()
        .map(|info| (info.name.clone(), info.is_copy))
        .collect();

    // Run ownership analysis
    let mut ownership_analyzer = OwnershipAnalyzer::new();
    let Ok(_) = ownership_analyzer.analyze(source) else {
        return None;
    };

    let set_annotations = ownership_analyzer.set_annotations();

    // Create context with semantic Copy info for accurate filtering
    let mut ctx = RenderContext::new_with_semantic(source, set_annotations, config.clone(), copy_types.clone());
    ctx.set_semantic_drop_lines(drop_lines);

    // For Validated style, use the special renderer
    if matches!(style, RenderStyle::Validated) {
        let mut validated_ctx = ValidatedRenderContext::new(ctx, &diagnostics, source);
        return Some(ValidatedRenderer::render(&mut validated_ctx));
    }

    Some(match style {
        RenderStyle::Inline => ValidRustRenderer::render(&InlineRenderer, &mut ctx),
        RenderStyle::Columnar => ValidRustRenderer::render(&ColumnarRenderer, &mut ctx),
        RenderStyle::Grouped => ValidRustRenderer::render(&GroupedRenderer, &mut ctx),
        RenderStyle::Diagnostic => RichTextRenderer::render(&DiagnosticRenderer, &mut ctx),
        RenderStyle::SetNotation => RichTextRenderer::render(&SetNotationRenderer, &mut ctx),
        RenderStyle::VerticalSpans => RichTextRenderer::render(&VerticalSpansRenderer, &mut ctx),
        RenderStyle::Html => RichTextRenderer::render(&HtmlRenderer, &mut ctx),
        RenderStyle::Validated => unreachable!(),
    })
}
