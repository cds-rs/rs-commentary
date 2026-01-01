//! Shared helper functions for renderers.

use crate::analysis::{SetEntry, SetEntryState};
use crate::util::{ChangeType, InvalidationReason, StateChange, StateTimeline, VariableDrop};

/// A unified annotation for rendering on a source line.
#[derive(Debug, Clone)]
pub struct LineAnnotation {
    /// Variable name
    pub name: String,
    /// Column position in the source line
    pub position: usize,
    /// State label (empty for drop-only annotations)
    pub state_label: Option<String>,
    /// Whether this variable is dropped after this line
    pub has_drop: bool,
    /// Why the binding became invalid (if has_drop is true)
    pub drop_reason: Option<InvalidationReason>,
    /// Whether this is a borrow (references don't "drop", their borrows "end")
    pub is_borrow: bool,
}

impl LineAnnotation {
    /// Create annotation from a state change.
    pub fn from_change(change: &StateChange, position: usize, drop_reason: Option<InvalidationReason>) -> Self {
        let is_borrow = matches!(
            change.state,
            SetEntryState::SharedBorrow | SetEntryState::MutBorrow
        );
        Self {
            name: change.name.clone(),
            position,
            state_label: Some(format_change_label(change)),
            has_drop: drop_reason.is_some(),
            drop_reason,
            is_borrow,
        }
    }

    /// Create drop-only annotation.
    pub fn drop_only(name: String, position: usize, reason: InvalidationReason, is_borrow: bool) -> Self {
        Self {
            name,
            position,
            state_label: None,
            has_drop: true,
            drop_reason: Some(reason),
            is_borrow,
        }
    }
}

/// Collect all annotations for a source line, combining state changes and drops.
/// Returns annotations sorted by position (rightmost first).
pub fn get_line_annotations(
    line: &str,
    changes: &[StateChange],
    pending_drops: &[VariableDrop],
    timeline: &StateTimeline,
) -> Vec<LineAnnotation> {
    let mut annotations = Vec::new();

    // Add state change annotations
    for change in changes {
        if let Some(pos) = find_var_position(line, &change.name) {
            let drop_reason = pending_drops
                .iter()
                .find(|d| d.name == change.name)
                .map(|d| d.reason.clone());
            annotations.push(LineAnnotation::from_change(change, pos, drop_reason));
        }
    }

    // Add drop-only annotations for variables dropped on this line
    // but without a state change annotation
    for drop in pending_drops {
        if !timeline.is_drop_shown(&drop.name) {
            let already_annotated = annotations.iter().any(|a| a.name == drop.name);
            if !already_annotated {
                if let Some(pos) = find_var_position(line, &drop.name) {
                    // frees_source being Some indicates this was a borrow
                    let is_borrow = drop.frees_source.is_some();
                    annotations.push(LineAnnotation::drop_only(
                        drop.name.clone(),
                        pos,
                        drop.reason.clone(),
                        is_borrow,
                    ));
                }
            }
        }
    }

    // Sort by position (rightmost first for rendering order)
    annotations.sort_by_key(|a| std::cmp::Reverse(a.position));
    annotations
}

/// Calculate padding needed to reach target column.
pub fn pad_to_column(current: usize, target: usize) -> usize {
    if current < target {
        target - current
    } else {
        2
    }
}

/// Format capability dots for a state.
pub fn format_capability_dots(entry: &SetEntry) -> &'static str {
    match &entry.state {
        SetEntryState::Owned => {
            if entry.mutable {
                "●●●"
            } else {
                "●●○"
            }
        }
        SetEntryState::Shared { .. } => "●●○",
        SetEntryState::Frozen { .. } => "●○○",
        SetEntryState::SharedBorrow => "○●○",
        SetEntryState::MutBorrow => "○●●",
        SetEntryState::Moved { .. } => "───",
        SetEntryState::Dropped => "───",
    }
}

/// Format a single entry in short form: `x ●●●`
pub fn format_entry_short(entry: &SetEntry) -> String {
    match &entry.state {
        SetEntryState::Owned => {
            if entry.mutable {
                format!("{} ●●●", entry.name)
            } else {
                format!("{} ●●○", entry.name)
            }
        }
        SetEntryState::Shared { borrowed_by } => {
            if borrowed_by.is_empty() {
                format!("{} ●●○ shr", entry.name)
            } else {
                format!("{} ●●○ shr({})", entry.name, borrowed_by.join(","))
            }
        }
        SetEntryState::Frozen { borrowed_by } => {
            format!("{} ●○○ frz({})", entry.name, borrowed_by)
        }
        SetEntryState::SharedBorrow => {
            if let Some(from) = &entry.borrows_from {
                format!("{} ○●○ &{}", entry.name, from)
            } else {
                format!("{} ○●○", entry.name)
            }
        }
        SetEntryState::MutBorrow => {
            if let Some(from) = &entry.borrows_from {
                format!("{} ○●● &mut {}", entry.name, from)
            } else {
                format!("{} ○●●", entry.name)
            }
        }
        SetEntryState::Moved { to } => {
            if let Some(target) = to {
                format!("{} → {}", entry.name, target)
            } else {
                format!("{} → _", entry.name)
            }
        }
        SetEntryState::Dropped => format!("{}†", entry.name),
    }
}

/// Format entries as inline comment: `// x ●●●  y ●●○`
pub fn format_entries_inline(entries: &[&SetEntry]) -> String {
    let parts: Vec<String> = entries
        .iter()
        .filter(|e| !matches!(e.state, SetEntryState::Dropped))
        .map(|e| format_entry_short(e))
        .collect();

    if parts.is_empty() {
        let dropped: Vec<String> = entries
            .iter()
            .filter(|e| matches!(e.state, SetEntryState::Dropped))
            .map(|e| format!("{}†", e.name))
            .collect();
        if !dropped.is_empty() {
            return format!("// {}", dropped.join(", "));
        }
        return String::new();
    }

    format!("// {}", parts.join("  "))
}

/// Format entries in columnar form with fixed-width columns.
pub fn format_entries_columnar(
    entries: &[&SetEntry],
    all_vars: &[String],
    col_width: usize,
    timeline: &StateTimeline,
) -> String {
    let mut cols: Vec<String> = Vec::new();

    for var in all_vars {
        let entry = entries.iter().find(|e| &e.name == var);
        let col = match entry {
            Some(e) => {
                if matches!(e.state, SetEntryState::Dropped) && timeline.is_drop_shown(var) {
                    String::new()
                } else {
                    format_entry_short(e)
                }
            }
            None => String::new(),
        };
        cols.push(format!("{:<width$}", col, width = col_width));
    }

    format!("// {}", cols.join(" "))
}

/// Format entries in compact form for grouped renderer.
pub fn format_entries_compact(entries: &[&SetEntry], timeline: &StateTimeline) -> String {
    let parts: Vec<String> = entries
        .iter()
        .filter(|e| {
            if matches!(e.state, SetEntryState::Dropped) {
                !timeline.is_drop_shown(&e.name)
            } else {
                true
            }
        })
        .map(|e| format_entry_short(e))
        .collect();

    parts.join("  ")
}

/// Find the position of a variable name in a line of code.
/// Prioritizes declaration patterns over usage.
pub fn find_var_position(line: &str, var_name: &str) -> Option<usize> {
    // Priority 1: let declarations
    for prefix in &["let mut ", "let "] {
        if let Some(let_pos) = line.find(prefix) {
            let after_let = &line[let_pos + prefix.len()..];
            if after_let.starts_with(var_name) {
                // Check it's followed by non-identifier char
                let next_char = after_let.chars().nth(var_name.len());
                if next_char.map(|c| !c.is_alphanumeric() && c != '_').unwrap_or(true) {
                    return Some(let_pos + prefix.len());
                }
            }
        }
    }

    // Priority 2: &var or &mut var (for borrows)
    for prefix in &["&mut ", "&"] {
        if let Some(pos) = line.find(&format!("{}{}", prefix, var_name)) {
            return Some(pos + prefix.len());
        }
    }

    // Priority 3: First occurrence as whole word
    let mut search_start = 0;
    while let Some(pos) = line[search_start..].find(var_name) {
        let abs_pos = search_start + pos;
        // Check word boundaries
        let before_ok = abs_pos == 0 || !line.chars().nth(abs_pos - 1).map(|c| c.is_alphanumeric() || c == '_').unwrap_or(false);
        let after_ok = abs_pos + var_name.len() >= line.len() || !line.chars().nth(abs_pos + var_name.len()).map(|c| c.is_alphanumeric() || c == '_').unwrap_or(false);

        if before_ok && after_ok {
            return Some(abs_pos);
        }
        search_start = abs_pos + 1;
    }

    None
}

/// Format a label for a state change from the timeline.
pub fn format_change_label(change: &StateChange) -> String {
    let state_str: String = match &change.state {
        SetEntryState::Owned => {
            if change.mutable { "●●● owned mut".to_string() } else { "●●○ owned".to_string() }
        }
        SetEntryState::Shared { borrowed_by } => {
            if borrowed_by.is_empty() {
                "●●○ shared".to_string()
            } else {
                format!("●●○ shared (by {})", borrowed_by.join(", "))
            }
        }
        SetEntryState::Frozen { borrowed_by } => {
            format!("●○○ frozen (by {})", borrowed_by)
        }
        SetEntryState::SharedBorrow => "○●○ shared borrow".to_string(),
        SetEntryState::MutBorrow => "○●● mutable borrow".to_string(),
        SetEntryState::Moved { to } => {
            if let Some(target) = to {
                format!("move → {}; now invalid", target)
            } else {
                "moved; now invalid".to_string()
            }
        }
        SetEntryState::Dropped => "dropped".to_string(),
    };

    match &change.change_type {
        ChangeType::New => state_str,
        ChangeType::StateChanged { from } => {
            let prev_str = match from {
                SetEntryState::Owned => "owned",
                SetEntryState::Shared { .. } => "shared",
                SetEntryState::Frozen { .. } => "frozen",
                SetEntryState::SharedBorrow => "borrow",
                SetEntryState::MutBorrow => "&mut",
                SetEntryState::Moved { .. } => "moved",
                SetEntryState::Dropped => "dropped",
            };
            format!("{} (was {})", state_str, prev_str)
        }
    }
}

/// Extract function name from a line like `fn foo() {` or `fn bar(x: i32) -> bool {`
pub fn extract_function_name(line: &str) -> Option<String> {
    let trimmed = line.trim();
    if !trimmed.starts_with("fn ") {
        return None;
    }
    let after_fn = &trimmed[3..];
    let name_end = after_fn.find(|c: char| c == '(' || c == '<' || c.is_whitespace())?;
    Some(after_fn[..name_end].to_string())
}
