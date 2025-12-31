//! Validated renderer - combines ownership state with rust-analyzer diagnostics.
//!
//! This renderer uses rust-analyzer's semantic analysis to show actual compiler
//! errors inline with ownership state annotations. Invalid Rust is clearly marked.

use crate::analysis::{DiagnosticSeverity, RaDiagnostic};
use crate::output::context::RenderContext;
use crate::output::helpers::extract_function_name;
use ra_ap_syntax::TextSize;
use std::collections::HashMap;
use super::set_notation::format_set_notation;

/// Validated renderer - shows ownership state alongside rust-analyzer errors.
pub struct ValidatedRenderer;

/// Context for validated rendering - includes diagnostics.
pub struct ValidatedRenderContext<'a> {
    pub base: RenderContext<'a>,
    pub diagnostics: Vec<RaDiagnostic>,
    /// Maps line numbers to diagnostics on that line.
    pub diags_by_line: HashMap<u32, Vec<&'a RaDiagnostic>>,
}

impl<'a> ValidatedRenderContext<'a> {
    pub fn new(
        base: RenderContext<'a>,
        diagnostics: &'a [RaDiagnostic],
        source: &str,
    ) -> Self {
        // Map diagnostics to line numbers
        let mut diags_by_line: HashMap<u32, Vec<&RaDiagnostic>> = HashMap::new();
        for diag in diagnostics {
            let line = offset_to_line(source, diag.range.start());
            diags_by_line.entry(line).or_default().push(diag);
        }

        Self {
            base,
            diagnostics: diagnostics.to_vec(),
            diags_by_line,
        }
    }
}

/// Convert a byte offset to a line number (0-indexed).
fn offset_to_line(source: &str, offset: TextSize) -> u32 {
    let offset = u32::from(offset) as usize;
    source[..offset.min(source.len())]
        .chars()
        .filter(|&c| c == '\n')
        .count() as u32
}

impl ValidatedRenderer {
    pub fn render(ctx: &mut ValidatedRenderContext) -> String {
        let mut output = String::new();
        let line_num_width = ctx.base.lines.len().to_string().len().max(2);
        let mut current_scope = String::from("main");

        // Header showing analysis status
        let error_count = ctx
            .diagnostics
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Error)
            .count();
        let warning_count = ctx
            .diagnostics
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Warning)
            .count();

        if error_count > 0 || warning_count > 0 {
            output.push_str(&format!(
                "╭─ rust-analyzer: {} error(s), {} warning(s)\n│\n",
                error_count, warning_count
            ));
        } else {
            output.push_str("╭─ rust-analyzer: ✓ no errors\n│\n");
        }

        for (line_num, line) in ctx.base.lines.iter().enumerate() {
            let line_num_u32 = line_num as u32;
            let display_num = line_num + 1;

            // Detect function declarations
            if let Some(fn_name) = extract_function_name(line) {
                current_scope = fn_name;
            }

            // Check for NLL drops
            let pending_drops = ctx.base.timeline.get_pending_drops(line_num_u32);
            if !pending_drops.is_empty() && !line.trim().is_empty() {
                let drop_names: Vec<String> =
                    pending_drops.iter().map(|d| format!("{}†", d.name)).collect();
                output.push_str(&format!(
                    "{:>width$} │   ↳ {} (NLL drop)\n",
                    "",
                    drop_names.join(", "),
                    width = line_num_width
                ));
                for drop in &pending_drops {
                    ctx.base.timeline.mark_drop_shown(&drop.name);
                }
            }

            // Print source line
            output.push_str(&format!(
                "{:>width$} │ {}\n",
                display_num,
                line,
                width = line_num_width
            ));

            // Show ownership state
            if let Some(set) = ctx.base.sets_by_line.get(&line_num_u32) {
                let entries = ctx.base.filter_entries(set);
                if !entries.is_empty() {
                    let set_str = format_set_notation(&current_scope, &entries, &[]);
                    output.push_str(&format!(
                        "{:>width$} │   └─ {}\n",
                        "",
                        set_str,
                        width = line_num_width
                    ));
                }
            }

            // Show diagnostics for this line
            if let Some(diags) = ctx.diags_by_line.get(&line_num_u32) {
                for diag in diags {
                    let severity_icon = match diag.severity {
                        DiagnosticSeverity::Error => "✗",
                        DiagnosticSeverity::Warning => "⚠",
                        DiagnosticSeverity::Info => "ℹ",
                        DiagnosticSeverity::Hint => "•",
                    };
                    let severity_label = match diag.severity {
                        DiagnosticSeverity::Error => "error",
                        DiagnosticSeverity::Warning => "warning",
                        DiagnosticSeverity::Info => "info",
                        DiagnosticSeverity::Hint => "hint",
                    };
                    output.push_str(&format!(
                        "{:>width$} │   {} {}: {}\n",
                        "",
                        severity_icon,
                        severity_label,
                        diag.message,
                        width = line_num_width
                    ));
                    if !diag.code.is_empty() {
                        output.push_str(&format!(
                            "{:>width$} │     ╰─ [{}]\n",
                            "",
                            diag.code,
                            width = line_num_width
                        ));
                    }
                }
            }
        }

        // Footer
        output.push_str(&format!("{:>width$} │\n", "", width = line_num_width));
        output.push_str(&format!("{:>width$} ╰─\n", "", width = line_num_width));

        output
    }
}
