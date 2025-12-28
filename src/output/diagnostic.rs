//! Diagnostic renderer - rustc-style with line numbers and underlines.

use super::context::RenderContext;
use super::helpers::{find_var_position, format_change_label};
use super::traits::RichTextRenderer;

/// Diagnostic renderer - rustc-style with line numbers and underline annotations.
pub struct DiagnosticRenderer;

impl RichTextRenderer for DiagnosticRenderer {
    fn render(&self, ctx: &mut RenderContext) -> String {
        let mut output = String::new();
        let line_num_width = ctx.lines.len().to_string().len().max(2);

        // Header
        output.push_str(&format!("{:>width$} ╭─\n", "", width = line_num_width));

        for (line_num, line) in ctx.lines.iter().enumerate() {
            let line_num_u32 = line_num as u32;
            let display_num = line_num + 1;

            // Print the source line with line number
            output.push_str(&format!(
                "{:>width$} │ {}\n",
                display_num,
                line,
                width = line_num_width
            ));

            // Get state changes from timeline
            let changes = ctx.timeline.get_changes(line_num_u32);

            // Get pending drops for the NEXT line (drops happen after this line)
            let pending_drops = ctx.timeline.get_pending_drops(line_num_u32 + 1);

            // Collect annotations for this line: (var_name, position, label, has_drop)
            let mut annotations: Vec<(String, usize, String, bool)> = Vec::new();

            for change in &changes {
                if let Some(pos) = find_var_position(line, &change.name) {
                    let label = format_change_label(change);
                    // Check if this variable has a pending drop
                    let has_drop = pending_drops.iter().any(|d| d.name == change.name);
                    annotations.push((change.name.clone(), pos, label, has_drop));
                }
            }

            // Render underline annotations
            if !annotations.is_empty() {
                // Sort by position (rightmost first for rendering order)
                annotations.sort_by_key(|(_, pos, _, _)| std::cmp::Reverse(*pos));

                // Build underline line with ─ under each variable
                let max_pos = line.chars().count();
                let mut underline_chars: Vec<char> = vec![' '; max_pos];

                for (name, pos, _, _) in &annotations {
                    let var_len = name.chars().count();
                    for i in 0..var_len {
                        if pos + i < underline_chars.len() {
                            underline_chars[pos + i] = '─';
                        }
                    }
                }

                let underline: String = underline_chars.iter().collect();
                output.push_str(&format!(
                    "{:>width$} │ {}\n",
                    "",
                    underline.trim_end(),
                    width = line_num_width
                ));

                // Print labels from right to left
                // For each variable: show its state, then its drop note if applicable
                for (idx, (name, pos, label, has_drop)) in annotations.iter().enumerate() {
                    let is_last = idx == annotations.len() - 1;

                    // Build the connector line for the state annotation
                    let mut connector_chars: Vec<char> = vec![' '; max_pos];

                    // Add | for all annotations to the left of this one
                    if !is_last {
                        for (_, other_pos, _, _) in annotations.iter().skip(idx + 1) {
                            if *other_pos < connector_chars.len() {
                                connector_chars[*other_pos] = '|';
                            }
                        }
                    }

                    // Add └─ at current position
                    if *pos < connector_chars.len() {
                        connector_chars[*pos] = '└';
                    }

                    let connector: String = connector_chars.iter().collect();
                    let connector_trimmed = connector.trim_end();

                    // Print the state annotation
                    output.push_str(&format!(
                        "{:>width$} │ {}─ {}: {}\n",
                        "",
                        connector_trimmed,
                        name,
                        label,
                        width = line_num_width
                    ));

                    // If this variable has a drop, print it on the next line
                    if *has_drop {
                        // Build connector for drop note (same position, but use └─ for drop)
                        let mut drop_connector_chars: Vec<char> = vec![' '; max_pos];

                        // Add | for annotations to the left
                        if !is_last {
                            for (_, other_pos, _, _) in annotations.iter().skip(idx + 1) {
                                if *other_pos < drop_connector_chars.len() {
                                    drop_connector_chars[*other_pos] = '|';
                                }
                            }
                        }

                        // Add └─ at current position for the drop
                        if *pos < drop_connector_chars.len() {
                            drop_connector_chars[*pos] = '└';
                        }

                        let drop_connector: String = drop_connector_chars.iter().collect();
                        let drop_connector_trimmed = drop_connector.trim_end();

                        output.push_str(&format!(
                            "{:>width$} │ {}─ note: `{}` dropped (last use)\n",
                            "",
                            drop_connector_trimmed,
                            name,
                            width = line_num_width
                        ));

                        ctx.timeline.mark_drop_shown(name);
                    }

                    // Add blank connector line between variables (except after the last one)
                    if !is_last {
                        let mut blank_connector_chars: Vec<char> = vec![' '; max_pos];
                        for (_, other_pos, _, _) in annotations.iter().skip(idx + 1) {
                            if *other_pos < blank_connector_chars.len() {
                                blank_connector_chars[*other_pos] = '|';
                            }
                        }
                        let blank_connector: String = blank_connector_chars.iter().collect();
                        let blank_trimmed = blank_connector.trim_end();
                        if !blank_trimmed.is_empty() {
                            output.push_str(&format!(
                                "{:>width$} │ {}\n",
                                "",
                                blank_trimmed,
                                width = line_num_width
                            ));
                        }
                    }
                }
            }

            // Handle drops for variables not annotated on this line
            // (e.g., variables that appear but don't have state changes)
            let mut had_standalone_drops = false;
            for drop in &pending_drops {
                if !ctx.timeline.is_drop_shown(&drop.name) {
                    if let Some(pos) = find_var_position(line, &drop.name) {
                        let max_pos = line.chars().count();
                        let mut marker_chars: Vec<char> = vec![' '; max_pos];
                        let var_len = drop.name.chars().count();
                        for i in 0..var_len {
                            if pos + i < marker_chars.len() {
                                marker_chars[pos + i] = '^';
                            }
                        }
                        let marker: String = marker_chars.iter().collect();
                        output.push_str(&format!(
                            "{:>width$} │ {} note: `{}` dropped (last use)\n",
                            "",
                            marker.trim_end(),
                            drop.name,
                            width = line_num_width
                        ));
                        ctx.timeline.mark_drop_shown(&drop.name);
                        had_standalone_drops = true;
                    }
                }
            }

            // Add blank line after annotation block for readability
            if !annotations.is_empty() || had_standalone_drops {
                output.push_str(&format!("{:>width$} │\n", "", width = line_num_width));
            }
        }

        // Footer with final drops
        let final_drops: Vec<String> = if let Some(last_state) = ctx.timeline.get(ctx.lines.len().saturating_sub(1) as u32) {
            last_state
                .live
                .iter()
                .filter(|v| !ctx.timeline.is_drop_shown(v))
                .map(|v| format!("{}†", v))
                .collect()
        } else {
            Vec::new()
        };

        if !final_drops.is_empty() {
            output.push_str(&format!("{:>width$} │\n", "", width = line_num_width));
            output.push_str(&format!(
                "{:>width$} ╰─ scope end: {}\n",
                "",
                final_drops.join(", "),
                width = line_num_width
            ));
        } else {
            output.push_str(&format!("{:>width$} ╰─\n", "", width = line_num_width));
        }

        output
    }
}
