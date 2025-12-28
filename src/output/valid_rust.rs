//! Renderers that produce syntactically valid Rust output.
//!
//! These renderers use `//` comments for annotations, so output can be compiled.

use super::context::RenderContext;
use super::helpers::{format_entries_columnar, format_entries_compact, format_entries_inline, pad_to_column};
use super::traits::ValidRustRenderer;

/// Inline renderer - comments at end of each line.
pub struct InlineRenderer;

impl ValidRustRenderer for InlineRenderer {
    fn render(&self, ctx: &mut RenderContext) -> String {
        let mut output = String::new();

        for (line_num, line) in ctx.lines.iter().enumerate() {
            output.push_str(line);

            if let Some(set) = ctx.sets_by_line.get(&(line_num as u32)) {
                let entries = ctx.filter_entries(set);
                if !entries.is_empty() {
                    let comment = format_entries_inline(&entries);
                    let padding = pad_to_column(line.len(), ctx.config.comment_column);
                    output.push_str(&" ".repeat(padding));
                    output.push_str(&comment);
                }
            }

            output.push('\n');
        }

        output
    }
}

/// Columnar renderer - fixed columns with NLL transition lines.
pub struct ColumnarRenderer;

impl ValidRustRenderer for ColumnarRenderer {
    fn render(&self, ctx: &mut RenderContext) -> String {
        let col_width = ctx.all_vars.iter().map(|v| v.len()).max().unwrap_or(1) + 6;

        let mut output = String::new();

        for (line_num, line) in ctx.lines.iter().enumerate() {
            let line_num = line_num as u32;

            // Check for NLL drops using timeline
            let pending_drops = ctx.timeline.get_pending_drops(line_num);
            if !pending_drops.is_empty() && !line.trim().is_empty() {
                let padding = " ".repeat(ctx.config.comment_column);
                let mut cols: Vec<String> = Vec::new();
                for var in &ctx.all_vars {
                    let col = if pending_drops.iter().any(|d| d.name == *var) {
                        format!("{}†", var)
                    } else {
                        String::new()
                    };
                    cols.push(format!("{:<width$}", col, width = col_width));
                }
                output.push_str(&format!("{}// {}\n", padding, cols.join(" ")));
                for drop in &pending_drops {
                    ctx.timeline.mark_drop_shown(&drop.name);
                }
            }

            output.push_str(line);

            // Add columnar comment
            if let Some(set) = ctx.sets_by_line.get(&line_num) {
                let entries = ctx.filter_entries(set);
                if !entries.is_empty() {
                    let comment =
                        format_entries_columnar(&entries, &ctx.all_vars, col_width, &ctx.timeline);
                    let padding = pad_to_column(line.len(), ctx.config.comment_column);
                    output.push_str(&" ".repeat(padding));
                    output.push_str(&comment);
                }
            }

            output.push('\n');
        }

        output
    }
}

/// Grouped renderer - horizontal rules with blank lines.
pub struct GroupedRenderer;

impl ValidRustRenderer for GroupedRenderer {
    fn render(&self, ctx: &mut RenderContext) -> String {
        let rule_width = 45;

        let mut output = String::new();

        for (line_num, line) in ctx.lines.iter().enumerate() {
            let line_num = line_num as u32;

            // Check for drops (show before the line with ═ rule)
            let pending_drops = ctx.timeline.get_pending_drops(line_num);
            if !pending_drops.is_empty() && !line.trim().is_empty() {
                let drop_str: Vec<String> = pending_drops.iter().map(|d| format!("{}†", d.name)).collect();
                output.push_str(&format!(
                    "    // {}  {}\n\n",
                    "═".repeat(rule_width),
                    drop_str.join("  ")
                ));
                for drop in &pending_drops {
                    ctx.timeline.mark_drop_shown(&drop.name);
                }
            }

            output.push_str(line);
            output.push('\n');

            // Check if state changed - if so, add annotation after line
            let changes = ctx.timeline.get_changes(line_num);
            if !changes.is_empty() {
                if let Some(set) = ctx.sets_by_line.get(&line_num) {
                    let entries = ctx.filter_entries(set);
                    let state_str = format_entries_compact(&entries, &ctx.timeline);
                    if !state_str.is_empty() {
                        output.push_str(&format!(
                            "    // {}  {}\n\n",
                            "─".repeat(rule_width),
                            state_str
                        ));
                    }
                }
            }
        }

        // Final drops at end - get all remaining live variables
        if let Some(last_state) = ctx.timeline.get(ctx.lines.len().saturating_sub(1) as u32) {
            let final_drops: Vec<String> = last_state
                .live
                .iter()
                .filter(|v| !ctx.timeline.is_drop_shown(v))
                .map(|v| format!("{}†", v))
                .collect();
            if !final_drops.is_empty() {
                output.push_str(&format!(
                    "// {}  {}\n",
                    "═".repeat(rule_width),
                    final_drops.join("  ")
                ));
            }
        }

        output
    }
}
