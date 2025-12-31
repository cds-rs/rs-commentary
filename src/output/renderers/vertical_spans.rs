//! Vertical spans renderer - shows borrow lifetimes with vertical brackets.
//!
//! ```text
//!     let mut x = vec![1, 2];         x ●●●
//!                                       ╭─── r
//!     let r = &x;                     x ●●○ │  r ○●○
//!     println!("{}", r);              x ●●○ │  r ○●○
//!                                       ╰─── r†
//!     x.push(3);                      x ●●●
//! ```

use crate::analysis::{SetEntry, SetEntryState};
use crate::output::context::RenderContext;
use crate::output::helpers::{format_capability_dots, pad_to_column};
use crate::output::traits::RichTextRenderer;

/// Vertical spans renderer - shows borrow extents with box-drawing characters.
pub struct VerticalSpansRenderer;

impl RichTextRenderer for VerticalSpansRenderer {
    fn render(&self, ctx: &mut RenderContext) -> String {
        // Track active borrows and their start lines
        let mut active_borrows: Vec<ActiveBorrow> = Vec::new();
        let mut output = String::new();
        let state_col = ctx.config.comment_column;

        for (line_num, line) in ctx.lines.iter().enumerate() {
            let line_num = line_num as u32;

            // Check for borrows that end before this line (NLL drops)
            let pending_drops = ctx.timeline.get_pending_drops(line_num);
            // Filter to only borrows that are currently active
            let ending_borrows: Vec<String> = pending_drops
                .iter()
                .filter(|d| active_borrows.iter().any(|ab| ab.name == d.name))
                .map(|d| d.name.clone())
                .collect();

            // Render borrow-end line if needed
            if !ending_borrows.is_empty() && !line.trim().is_empty() {
                let padding = " ".repeat(state_col);
                for borrow_name in &ending_borrows {
                    if let Some(pos) = active_borrows.iter().position(|b| &b.name == borrow_name) {
                        let ab = &active_borrows[pos];
                        output.push_str(&format!(
                            "{}  {}╰─── {}†\n",
                            padding,
                            " ".repeat(ab.column * 5),
                            borrow_name
                        ));
                        active_borrows.remove(pos);
                    }
                }
                for drop in &pending_drops {
                    ctx.timeline.mark_drop_shown(&drop.name);
                }
            }

            // Check for new borrows on this line
            let changes = ctx.timeline.get_changes(line_num);
            let new_borrows: Vec<_> = changes
                .iter()
                .filter(|c| {
                    matches!(
                        c.state,
                        SetEntryState::SharedBorrow | SetEntryState::MutBorrow
                    )
                })
                .collect();

            // Render borrow-start line if needed
            for new_borrow in &new_borrows {
                let col = active_borrows.len();
                let padding = " ".repeat(state_col);
                output.push_str(&format!(
                    "{}  {}╭─── {}\n",
                    padding,
                    " ".repeat(col * 5),
                    new_borrow.name
                ));
                active_borrows.push(ActiveBorrow {
                    name: new_borrow.name.clone(),
                    column: col,
                });
            }

            // Render source line with state
            output.push_str(line);

            if let Some(set) = ctx.sets_by_line.get(&line_num) {
                let entries = ctx.filter_entries(set);
                if !entries.is_empty() {
                    let state_str = format_vertical_state(&entries);
                    let spans_str = format_active_spans(&active_borrows);
                    let padding = pad_to_column(line.len(), state_col);
                    output.push_str(&" ".repeat(padding));
                    output.push_str(&state_str);
                    if !spans_str.is_empty() {
                        output.push_str(&spans_str);
                    }
                }
            }

            output.push('\n');
        }

        // Final drops at scope end
        if let Some(last_state) = ctx
            .timeline
            .get(ctx.lines.len().saturating_sub(1) as u32)
        {
            let final_drops: Vec<&String> = last_state
                .live
                .iter()
                .filter(|v| !ctx.timeline.is_drop_shown(v))
                .collect();
            if !final_drops.is_empty() {
                let padding = " ".repeat(state_col);
                for var in final_drops {
                    output.push_str(&format!("{}{} †\n", padding, var));
                }
            }
        }

        output
    }
}

struct ActiveBorrow {
    name: String,
    column: usize,
}

fn format_vertical_state(entries: &[&SetEntry]) -> String {
    entries
        .iter()
        .filter(|e| !matches!(e.state, SetEntryState::Dropped))
        .map(|e| format!("{} {}", e.name, format_capability_dots(e)))
        .collect::<Vec<_>>()
        .join("  ")
}

fn format_active_spans(borrows: &[ActiveBorrow]) -> String {
    if borrows.is_empty() {
        return String::new();
    }
    let mut s = String::from("  ");
    for (i, _) in borrows.iter().enumerate() {
        if i > 0 {
            s.push_str("    ");
        }
        s.push('│');
    }
    s
}
