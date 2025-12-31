//! Diagnostic renderer - rustc-style with line numbers and underlines.

use crate::output::context::RenderContext;
use crate::output::helpers::get_line_annotations;
use crate::output::traits::RichTextRenderer;

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

            // Get unified annotations (state changes + drops, sorted right-to-left)
            let annotations = get_line_annotations(line, &changes, &pending_drops, &ctx.timeline);

            // Render underline annotations
            if !annotations.is_empty() {
                // Build underline line with ─ under each variable
                let max_pos = line.chars().count();
                let mut underline_chars: Vec<char> = vec![' '; max_pos];

                for ann in &annotations {
                    let var_len = ann.name.chars().count();
                    for i in 0..var_len {
                        if ann.position + i < underline_chars.len() {
                            underline_chars[ann.position + i] = '─';
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
                for (idx, ann) in annotations.iter().enumerate() {
                    let is_last = idx == annotations.len() - 1;

                    // Build the connector line
                    let mut connector_chars: Vec<char> = vec![' '; max_pos];

                    // Add | for all annotations to the left of this one
                    if !is_last {
                        for other in annotations.iter().skip(idx + 1) {
                            if other.position < connector_chars.len() {
                                connector_chars[other.position] = '|';
                            }
                        }
                    }

                    // Add └─ at current position
                    if ann.position < connector_chars.len() {
                        connector_chars[ann.position] = '└';
                    }

                    let connector: String = connector_chars.iter().collect();
                    let connector_trimmed = connector.trim_end();

                    // Print the state annotation (if not drop-only)
                    if let Some(ref label) = ann.state_label {
                        output.push_str(&format!(
                            "{:>width$} │ {}─ {}: {}\n",
                            "",
                            connector_trimmed,
                            ann.name,
                            label,
                            width = line_num_width
                        ));
                    }

                    // Print drop note if applicable
                    if ann.has_drop {
                        // For drop-only annotations, reuse the same connector
                        // For state+drop, build a new connector
                        let drop_connector = if ann.state_label.is_some() {
                            let mut drop_connector_chars: Vec<char> = vec![' '; max_pos];
                            if !is_last {
                                for other in annotations.iter().skip(idx + 1) {
                                    if other.position < drop_connector_chars.len() {
                                        drop_connector_chars[other.position] = '|';
                                    }
                                }
                            }
                            if ann.position < drop_connector_chars.len() {
                                drop_connector_chars[ann.position] = '└';
                            }
                            drop_connector_chars.iter().collect::<String>()
                        } else {
                            connector.clone()
                        };
                        let drop_connector_trimmed = drop_connector.trim_end();

                        output.push_str(&format!(
                            "{:>width$} │ {}─ note: `{}` dropped (last use)\n",
                            "",
                            drop_connector_trimmed,
                            ann.name,
                            width = line_num_width
                        ));

                        ctx.timeline.mark_drop_shown(&ann.name);
                    }

                    // Add blank connector line between variables (except after the last one)
                    if !is_last {
                        let mut blank_connector_chars: Vec<char> = vec![' '; max_pos];
                        for other in annotations.iter().skip(idx + 1) {
                            if other.position < blank_connector_chars.len() {
                                blank_connector_chars[other.position] = '|';
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

                // Add blank line after annotation block for readability
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
