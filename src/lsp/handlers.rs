//! LSP request and notification handlers.

use crate::analysis::{OwnershipAnalyzer, OwnershipSet, SetAnnotation, SetEntry, SetEntryState};
use crate::util::position_to_offset;
use anyhow::Result;
use lsp_types::{
    Hover, HoverContents, HoverParams, InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams,
    MarkupContent, MarkupKind, Position, Uri,
};
use std::collections::HashMap;

type Url = Uri;

/// Cached analysis for a document.
struct AnalysisCache {
    annotations: Vec<crate::analysis::Annotation>,
    set_annotations: Vec<SetAnnotation>,
}

/// State for the LSP server.
pub struct ServerState {
    /// Document contents by URI
    pub documents: HashMap<Url, String>,
    /// Cached analysis results by URI
    pub analysis_cache: HashMap<Url, AnalysisCache>,
}

impl ServerState {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            analysis_cache: HashMap::new(),
        }
    }

    /// Open a document and analyze it.
    pub fn open_document(&mut self, uri: Url, content: String) {
        self.documents.insert(uri.clone(), content.clone());
        self.analyze_document(&uri);
    }

    /// Update a document and re-analyze.
    pub fn update_document(&mut self, uri: Url, content: String) {
        self.documents.insert(uri.clone(), content.clone());
        self.analyze_document(&uri);
    }

    /// Close a document.
    pub fn close_document(&mut self, uri: &Url) {
        self.documents.remove(uri);
        self.analysis_cache.remove(uri);
    }

    /// Analyze a document and cache results.
    fn analyze_document(&mut self, uri: &Url) {
        let Some(content) = self.documents.get(uri).cloned() else { return };

        let mut analyzer = OwnershipAnalyzer::new();
        match analyzer.analyze(&content) {
            Ok(annotations) => {
                let set_annotations = analyzer.set_annotations().to_vec();
                self.analysis_cache.insert(uri.clone(), AnalysisCache {
                    annotations,
                    set_annotations,
                });
            }
            Err(e) => {
                tracing::warn!("Analysis failed for {}: {}", uri.as_str(), e);
            }
        }
    }

    /// Get annotations for a document.
    pub fn get_annotations(&self, uri: &Url) -> Option<&Vec<crate::analysis::Annotation>> {
        self.analysis_cache.get(uri).map(|c| &c.annotations)
    }

    /// Get set annotations for a document.
    pub fn get_set_annotations(&self, uri: &Url) -> Option<&Vec<SetAnnotation>> {
        self.analysis_cache.get(uri).map(|c| &c.set_annotations)
    }
}

impl Default for ServerState {
    fn default() -> Self {
        Self::new()
    }
}

/// Handle textDocument/inlayHint request.
pub fn handle_inlay_hints(
    state: &ServerState,
    params: InlayHintParams,
) -> Result<Option<Vec<InlayHint>>> {
    let uri = &params.text_document.uri;
    tracing::info!("Inlay hint request for: {}", uri.as_str());

    let document = match state.documents.get(uri) {
        Some(doc) => doc,
        None => {
            tracing::warn!("Document not found: {}", uri.as_str());
            return Ok(None);
        }
    };

    let set_annotations = match state.get_set_annotations(uri) {
        Some(ann) => {
            tracing::info!("Found {} set annotations", ann.len());
            ann
        }
        None => {
            tracing::warn!("No set annotations for: {}", uri.as_str());
            return Ok(None);
        }
    };

    let mut hints = Vec::new();
    let lines: Vec<&str> = document.lines().collect();

    // Group set annotations by line, keeping only the last one per line
    let mut sets_by_line: HashMap<u32, &OwnershipSet> = HashMap::new();
    for set_ann in set_annotations {
        sets_by_line.insert(set_ann.line, &set_ann.set);
    }

    // Create hints for each line with a set
    for (line, set) in sets_by_line {
        if set.is_empty() {
            continue;
        }

        let hint_position = Position {
            line,
            character: lines
                .get(line as usize)
                .map(|l| l.len() as u32)
                .unwrap_or(0),
        };

        // Format per-variable capabilities: "x: ORW, y: O-R-W (frozen)"
        let label = format!(" // {}", format_capabilities_hint(set));

        hints.push(InlayHint {
            position: hint_position,
            label: InlayHintLabel::String(label),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: Some(lsp_types::InlayHintTooltip::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!(
                    "**Ownership Set**\n\n`{}`\n\n---\n\n{}",
                    set,
                    format_set_explanation(set)
                ),
            })),
            padding_left: Some(true),
            padding_right: None,
            data: None,
        });
    }

    tracing::info!("Returning {} inlay hints", hints.len());
    Ok(Some(hints))
}

/// Format per-variable capabilities for inlay hint display.
/// Shows each variable with its O/R/W capabilities.
fn format_capabilities_hint(set: &OwnershipSet) -> String {
    set.entries
        .iter()
        .map(|entry| format_entry_capabilities(entry))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format a single entry's capabilities using circle notation.
/// ● = has capability, ○ = no capability
/// Order: O R W (Ownership, Read, Write)
fn format_entry_capabilities(entry: &SetEntry) -> String {
    match &entry.state {
        SetEntryState::Owned => {
            if entry.mutable {
                format!("{} ●●●", entry.name)  // ORW
            } else {
                format!("{} ●●○", entry.name)  // OR
            }
        }
        SetEntryState::Shared => {
            format!("{} ●●○", entry.name)  // OR (shared)
        }
        SetEntryState::Frozen => {
            format!("{} ●○○", entry.name)  // O only
        }
        SetEntryState::SharedBorrow => {
            if let Some(from) = &entry.borrows_from {
                format!("{} ○●○ &{}", entry.name, from)  // R
            } else {
                format!("{} ○●○", entry.name)
            }
        }
        SetEntryState::MutBorrow => {
            if let Some(from) = &entry.borrows_from {
                format!("{} ○●● &mut {}", entry.name, from)  // RW
            } else {
                format!("{} ○●●", entry.name)
            }
        }
        SetEntryState::Dropped => {
            format!("†{}", entry.name)
        }
    }
}

/// Format a human-readable explanation of the ownership set.
fn format_set_explanation(set: &OwnershipSet) -> String {
    if set.entries.is_empty() {
        return "Empty set - no live bindings in scope.".to_string();
    }

    let mut lines = Vec::new();
    for entry in &set.entries {
        let desc = match &entry.state {
            SetEntryState::Owned => {
                if entry.mutable {
                    format!("**{}**: mutable owned (ORW)", entry.name)
                } else {
                    format!("**{}**: owned (OR)", entry.name)
                }
            }
            SetEntryState::Shared => {
                format!("**{}**: shared (shr) - read-only while borrowed", entry.name)
            }
            SetEntryState::Frozen => {
                format!("**{}**: frozen (frz) - no access while &mut active", entry.name)
            }
            SetEntryState::SharedBorrow => {
                let from = entry.borrows_from.as_deref().unwrap_or("?");
                format!("**{}**: shared borrow of {} (R)", entry.name, from)
            }
            SetEntryState::MutBorrow => {
                let from = entry.borrows_from.as_deref().unwrap_or("?");
                format!("**{}**: mutable borrow of {} (RW)", entry.name, from)
            }
            SetEntryState::Dropped => {
                format!("**{}**: dropped (†) - out of scope", entry.name)
            }
        };
        lines.push(format!("- {}", desc));
    }
    lines.join("\n")
}

/// Handle textDocument/hover request.
pub fn handle_hover(state: &ServerState, params: HoverParams) -> Result<Option<Hover>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let document = match state.documents.get(uri) {
        Some(doc) => doc,
        None => return Ok(None),
    };

    let annotations = state.get_annotations(uri);

    // Get ownership set for this line (or nearest previous line with a set)
    let set_annotations = state.get_set_annotations(uri);
    let current_line = position.line;
    let set_for_line = set_annotations.and_then(|sets| {
        // Find the most recent set at or before the current line
        sets.iter()
            .filter(|s| s.line <= current_line)
            .max_by_key(|s| s.line)
            .map(|s| &s.set)
    });

    // Find the offset for this position
    let offset = position_to_offset(document, position);

    // First, try to find an annotation that contains this offset
    if let Some(annotations) = annotations {
        for annotation in annotations {
            let (start, end) = annotation.span;
            if offset >= start as usize && offset <= end as usize {
                let capabilities = annotation.capabilities();
                let state_detail = annotation.state.hover_explanation();

                let set_section = if let Some(set) = set_for_line {
                    format!(
                        "\n\n---\n\n**Ownership Set:** `{}`\n\n{}",
                        set,
                        format_set_explanation(set)
                    )
                } else {
                    String::new()
                };

                let content = format!(
                    "## {} — {}\n\n\
                     **State:** {}\n\n\
                     **Capabilities:**\n\
                     - O (ownership): {}\n\
                     - R (read): {}\n\
                     - W (write): {}\n\n\
                     ---\n\n\
                     {}\n\n\
                     ---\n\n\
                     *{}*{}",
                    annotation.binding,
                    capabilities,
                    annotation.state,
                    if capabilities.owned { "✓" } else { "✗" },
                    if capabilities.read { "✓" } else { "✗" },
                    if capabilities.write { "✓" } else { "✗" },
                    annotation.explanation,
                    state_detail,
                    set_section
                );

                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: content,
                    }),
                    range: None,
                }));
            }
        }
    }

    // No annotation found - try to find a variable name at cursor and show its state from the set
    if let Some(set) = set_for_line {
        if let Some(var_name) = extract_identifier_at(document, offset) {
            // Look up this variable in the ownership set
            if let Some(entry) = set.entries.iter().find(|e| e.name == var_name) {
                let content = format_entry_hover(entry, set);
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: content,
                    }),
                    range: None,
                }));
            }
        }
    }

    Ok(None)
}

/// Extract the identifier (variable name) at the given byte offset.
fn extract_identifier_at(text: &str, offset: usize) -> Option<String> {
    if offset >= text.len() {
        return None;
    }

    let bytes = text.as_bytes();

    // Check if we're on an identifier character
    let ch = bytes[offset] as char;
    if !ch.is_alphanumeric() && ch != '_' {
        return None;
    }

    // Find the start of the identifier
    let mut start = offset;
    while start > 0 {
        let prev = bytes[start - 1] as char;
        if prev.is_alphanumeric() || prev == '_' {
            start -= 1;
        } else {
            break;
        }
    }

    // Find the end of the identifier
    let mut end = offset;
    while end < bytes.len() {
        let curr = bytes[end] as char;
        if curr.is_alphanumeric() || curr == '_' {
            end += 1;
        } else {
            break;
        }
    }

    if start < end {
        Some(text[start..end].to_string())
    } else {
        None
    }
}

/// Format hover content for a variable from the ownership set.
fn format_entry_hover(entry: &SetEntry, set: &OwnershipSet) -> String {
    let (caps, state_desc, explanation) = match &entry.state {
        SetEntryState::Owned => {
            if entry.mutable {
                ("ORW", "owned (mut)", "Full ownership with mutation rights. Can read, write, move, or drop.")
            } else {
                ("OR", "owned", "Immutable ownership. Can read, move, or drop, but cannot mutate.")
            }
        }
        SetEntryState::Shared => {
            ("OR", "shared (shr)", "Temporarily shared: read-only while borrowed. Mutation blocked until borrows end.")
        }
        SetEntryState::Frozen => {
            ("O", "frozen (frz)", "Temporarily frozen: active &mut borrow exists. No read or write access until the borrow ends.")
        }
        SetEntryState::SharedBorrow => {
            ("R", "&borrow", "Shared reference (&T). Read-only access to borrowed value.")
        }
        SetEntryState::MutBorrow => {
            ("RW", "&mut borrow", "Mutable reference (&mut T). Exclusive read-write access to borrowed value.")
        }
        SetEntryState::Dropped => {
            ("†", "dropped", "Value has been dropped (freed). Scope has ended, memory is released.")
        }
    };

    let borrow_info = match (&entry.state, &entry.borrows_from) {
        (SetEntryState::SharedBorrow, Some(from)) => format!(" (borrows from `{}`)", from),
        (SetEntryState::MutBorrow, Some(from)) => format!(" (borrows from `{}`)", from),
        _ => String::new(),
    };

    format!(
        "## {} — {}\n\n\
         **State:** {}{}\n\n\
         ---\n\n\
         *{}*\n\n\
         ---\n\n\
         **Ownership Set:** `{}`\n\n{}",
        entry.name,
        caps,
        state_desc,
        borrow_info,
        explanation,
        set,
        format_set_explanation(set)
    )
}
