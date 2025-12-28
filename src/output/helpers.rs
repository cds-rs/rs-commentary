//! Shared helper functions for renderers.

use crate::analysis::{SetEntry, SetEntryState};
use crate::util::{ChangeType, StateChange, StateTimeline};

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
        SetEntryState::Shared => "●●○",
        SetEntryState::Frozen => "●○○",
        SetEntryState::SharedBorrow => "○●○",
        SetEntryState::MutBorrow => "○●●",
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
        SetEntryState::Shared => format!("{} ●●○", entry.name),
        SetEntryState::Frozen => format!("{} ●○○", entry.name),
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
    let state_str = match &change.state {
        SetEntryState::Owned => {
            if change.mutable { "●●● owned mut" } else { "●●○ owned" }
        }
        SetEntryState::Shared => "●●○ shared (borrowed)",
        SetEntryState::Frozen => "●○○ frozen (&mut active)",
        SetEntryState::SharedBorrow => "○●○ shared borrow",
        SetEntryState::MutBorrow => "○●● mutable borrow",
        SetEntryState::Dropped => "dropped",
    };

    match &change.change_type {
        ChangeType::New => state_str.to_string(),
        ChangeType::StateChanged { from } => {
            let prev_str = match from {
                SetEntryState::Owned => "owned",
                SetEntryState::Shared => "shared",
                SetEntryState::Frozen => "frozen",
                SetEntryState::SharedBorrow => "borrow",
                SetEntryState::MutBorrow => "&mut",
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
