//! Inlay hint generation from annotations.

use crate::analysis::Annotation;
use crate::util::offset_to_position;
use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, MarkupContent, MarkupKind, Position};

/// Format annotations as LSP inlay hints.
#[allow(dead_code)]
pub fn format_inlay_hints(
    annotations: &[Annotation],
    document: &str,
    _range: lsp_types::Range,
) -> Vec<InlayHint> {
    let lines: Vec<&str> = document.lines().collect();
    let mut hints = Vec::new();

    for annotation in annotations {
        let (start, _end) = annotation.span;
        let position = offset_to_position(document, start as usize);

        // Position at end of line
        let hint_position = Position {
            line: position.line,
            character: lines
                .get(position.line as usize)
                .map(|l| l.len() as u32)
                .unwrap_or(position.character),
        };

        let capabilities = annotation.capabilities();
        let label = format!(" // {}: {}", annotation.binding, capabilities);

        hints.push(InlayHint {
            position: hint_position,
            label: InlayHintLabel::String(label),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: Some(lsp_types::InlayHintTooltip::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!(
                    "**{}**\n\nState: {}\n\n{}",
                    annotation.binding, annotation.state, annotation.explanation
                ),
            })),
            padding_left: Some(true),
            padding_right: None,
            data: None,
        });
    }

    hints
}
