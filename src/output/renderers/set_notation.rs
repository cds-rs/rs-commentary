//! Set notation renderer - displays ownership sets like `main{mut x, r(&x)}`.
//!
//! Matches the notation used in NOTES/rules.md for teaching ownership concepts.

use crate::analysis::{SetEntry, SetEntryState};
use crate::output::context::RenderContext;
use crate::output::helpers::{extract_function_name, pad_to_column};
use crate::output::traits::RichTextRenderer;

/// Set notation renderer - displays ownership sets like `main{mut x, r(&x)}`.
pub struct SetNotationRenderer;

impl RichTextRenderer for SetNotationRenderer {
    fn render(&self, ctx: &mut RenderContext) -> String {
        let mut output = String::new();
        let mut current_scope = String::from("main");
        let set_col = ctx.config.comment_column;

        for (line_num, line) in ctx.lines.iter().enumerate() {
            let line_num = line_num as u32;

            // Detect function declarations to update scope name
            if let Some(fn_name) = extract_function_name(line) {
                current_scope = fn_name;
            }

            // Check for NLL drops before this line
            let pending_drops = ctx.timeline.get_pending_drops(line_num);
            if !pending_drops.is_empty() && !line.trim().is_empty() {
                // Show the restored state after drops
                if let Some(set) = ctx.sets_by_line.get(&line_num) {
                    let entries = ctx.filter_entries(set);
                    let drop_names: Vec<&str> =
                        pending_drops.iter().map(|d| d.name.as_str()).collect();
                    let set_str = format_set_notation(&current_scope, &entries, &drop_names);
                    let drop_note: Vec<String> =
                        pending_drops.iter().map(|d| format!("{}†", d.name)).collect();
                    let padding = " ".repeat(set_col);
                    output.push_str(&format!(
                        "{}// {} ← {} (NLL)\n",
                        padding,
                        set_str,
                        drop_note.join(", ")
                    ));
                }
                for drop in &pending_drops {
                    ctx.timeline.mark_drop_shown(&drop.name);
                }
            }

            output.push_str(line);

            // Add set notation comment
            if let Some(set) = ctx.sets_by_line.get(&line_num) {
                let entries = ctx.filter_entries(set);
                let set_str = format_set_notation(&current_scope, &entries, &[]);
                let padding = pad_to_column(line.len(), set_col);
                output.push_str(&" ".repeat(padding));
                output.push_str(&format!("// {}", set_str));
            } else if line.trim().is_empty() || line.trim().starts_with("//") {
                // Keep empty/comment lines as-is
            } else if line.trim() == "}" {
                // Scope end
                let padding = pad_to_column(line.len(), set_col);
                output.push_str(&" ".repeat(padding));
                output.push_str(&format!("// {}{{}}", current_scope));
            }

            output.push('\n');
        }

        output
    }
}

/// Format entries as set notation: `scope{mut x, r(&y)}`
pub fn format_set_notation(scope: &str, entries: &[&SetEntry], exclude: &[&str]) -> String {
    let parts: Vec<String> = entries
        .iter()
        .filter(|e| !exclude.contains(&e.name.as_str()))
        .filter(|e| !matches!(e.state, SetEntryState::Dropped))
        .map(|e| format_set_entry(e))
        .collect();

    if parts.is_empty() {
        format!("{}{{}}", scope)
    } else {
        format!("{}{{{}}}", scope, parts.join(", "))
    }
}

/// Format a single entry for set notation.
fn format_set_entry(entry: &SetEntry) -> String {
    match &entry.state {
        SetEntryState::Owned => {
            if entry.mutable {
                format!("mut {}", entry.name)
            } else {
                entry.name.clone()
            }
        }
        SetEntryState::Shared => format!("shr {}", entry.name),
        SetEntryState::Frozen => format!("frz {}", entry.name),
        SetEntryState::SharedBorrow => {
            if let Some(from) = &entry.borrows_from {
                format!("{}(&{})", entry.name, from)
            } else {
                format!("{}(&?)", entry.name)
            }
        }
        SetEntryState::MutBorrow => {
            if let Some(from) = &entry.borrows_from {
                format!("{}(&mut {})", entry.name, from)
            } else {
                format!("{}(&mut ?)", entry.name)
            }
        }
        SetEntryState::Dropped => format!("{}†", entry.name),
    }
}
