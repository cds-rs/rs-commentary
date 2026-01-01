//! Diagnostic renderer - rustc-style with line numbers and underlines.

use crate::output::context::RenderContext;
use crate::output::helpers::get_line_annotations;
use crate::output::traits::RichTextRenderer;
use crate::util::InvalidationReason;

/// Diagnostic renderer - rustc-style with line numbers and underline annotations.
pub struct DiagnosticRenderer;

/// Format a message based on the invalidation reason.
fn format_invalidation_message(name: &str, reason: &InvalidationReason) -> String {
    match reason {
        InvalidationReason::Moved { to } => {
            if let Some(target) = to {
                format!("note: `{}` moved to `{}`", name, target)
            } else {
                format!("note: `{}` moved", name)
            }
        }
        InvalidationReason::BorrowEnd => {
            format!("note: `{}` borrow ends here", name)
        }
        InvalidationReason::ScopeExit => {
            format!("note: `{}` last used here (dropped at scope exit)", name)
        }
    }
}

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

            // Get copy events for this line
            let copy_events = ctx.get_copy_events(line_num_u32);

            // Get unified annotations (state changes + drops, sorted right-to-left)
            let mut annotations = get_line_annotations(line, &changes, &pending_drops, &ctx.timeline);

            // Add copy event annotations
            for copy in copy_events {
                if let Some(pos) = crate::output::helpers::find_var_position(line, &copy.from) {
                    annotations.push(crate::output::helpers::LineAnnotation {
                        name: copy.from.clone(),
                        position: pos,
                        state_label: Some(format!("copied → {} (Copy); {} still valid", copy.to, copy.from)),
                        has_drop: false,
                        drop_reason: None,
                        is_borrow: false, // Copy types are owned, not borrows
                    });
                }
            }

            // Re-sort by position (rightmost first)
            annotations.sort_by_key(|a| std::cmp::Reverse(a.position));

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
                        // Override label with semantic info when available.
                        // This fixes for loop variables like `for x in vec.iter_mut()`
                        // where AST analysis shows "owned" but the type is actually &mut T.
                        // We pass line_num_u32 to disambiguate same-named variables in different scopes.
                        use crate::analysis::BindingKind;
                        let display_label = match ctx.get_binding_kind(&ann.name, line_num_u32) {
                            Some(BindingKind::MutRef) => "○●● mutable borrow".to_string(),
                            Some(BindingKind::SharedRef) => "○●○ shared borrow".to_string(),
                            _ => label.clone(),
                        };
                        output.push_str(&format!(
                            "{:>width$} │ {}─ {}: {}\n",
                            "",
                            connector_trimmed,
                            ann.name,
                            display_label,
                            width = line_num_width
                        ));
                    }

                    // Print drop note if applicable
                    // Skip drop note if:
                    // 1. State label already describes a move (avoid redundancy)
                    // 2. State label shows a copy (Copy types don't have meaningful drops)
                    // 3. Variable is a Copy type with ScopeExit reason (no interesting destructor)
                    let is_move_already_shown = ann.state_label.as_ref()
                        .map(|l| l.starts_with("move") || l.contains("→"))
                        .unwrap_or(false);
                    let is_copy_type = ann.state_label.as_ref()
                        .map(|l| l.contains("(Copy)") || l.contains("copied"))
                        .unwrap_or(false)
                        || ctx.is_copy_type(&ann.name);
                    let is_copy_scope_exit = is_copy_type
                        && matches!(ann.drop_reason, Some(crate::util::InvalidationReason::ScopeExit));

                    if ann.has_drop && !is_move_already_shown && !is_copy_scope_exit {
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

                        // Determine if this is a borrow/reference type.
                        // Prefer semantic info (from rust-analyzer type inference) over AST-based detection.
                        // This correctly handles for loop variables like `for x in vec.iter_mut()` where
                        // `x` is `&mut T` but AST analysis doesn't know this.
                        // We pass line_num_u32 to disambiguate same-named variables in different scopes.
                        let is_borrow = match ctx.get_binding_kind(&ann.name, line_num_u32) {
                            Some(kind) => kind.is_borrow(),
                            None => ann.is_borrow,
                        };

                        let message = if is_borrow {
                            // References don't "drop" - their borrows end
                            format!("note: `{}` borrow ends here", ann.name)
                        } else if let Some(ref reason) = ann.drop_reason {
                            format_invalidation_message(&ann.name, reason)
                        } else {
                            format!("note: `{}` dropped (last use)", ann.name)
                        };
                        output.push_str(&format!(
                            "{:>width$} │ {}─ {}\n",
                            "",
                            drop_connector_trimmed,
                            message,
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
