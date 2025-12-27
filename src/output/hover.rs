//! Hover information generation from annotations.

use crate::analysis::Annotation;
use crate::util::position_to_offset;
use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

/// Format an annotation as hover information.
pub fn format_hover(annotation: &Annotation) -> Hover {
    let capabilities = annotation.capabilities();

    let content = format!(
        "## {} — {}\n\n\
        **State:** {}\n\n\
        **Capabilities:**\n\
        - O (ownership): {}\n\
        - R (read): {}\n\
        - W (write): {}\n\n\
        ---\n\n\
        {}",
        annotation.binding,
        capabilities,
        annotation.state,
        if capabilities.owned { "✓" } else { "✗" },
        if capabilities.read { "✓" } else { "✗" },
        if capabilities.write { "✓" } else { "✗" },
        annotation.explanation
    );

    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    }
}

/// Find the annotation at a given position.
#[allow(dead_code)]
pub fn find_annotation_at_position<'a>(
    annotations: &'a [Annotation],
    document: &str,
    position: Position,
) -> Option<&'a Annotation> {
    let offset = position_to_offset(document, position);

    annotations
        .iter()
        .find(|ann| {
            let (start, end) = ann.span;
            offset >= start as usize && offset <= end as usize
        })
}
