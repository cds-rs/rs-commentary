//! Pluggable renderer framework for ownership annotations.
//!
//! Provides a trait-based system for different output styles,
//! making it easy to experiment with new visualizations.

use crate::analysis::{
    DiagnosticSeverity, OwnershipAnalyzer, OwnershipSet, RaDiagnostic, SemanticAnalyzer,
    SemanticResult, SetAnnotation, SetEntry, SetEntryState,
};
use crate::util::{ChangeType, StateTimeline};
use ra_ap_syntax::TextSize;
use std::collections::HashMap;
use std::path::Path;

/// Renderer output category - determines syntactic validity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RenderCategory {
    /// Output is syntactically valid Rust (uses `//` comments only).
    /// Can be compiled, run through rustfmt, etc.
    ValidRust,
    /// Output uses rich typography (box drawing, diagrams).
    /// More visually powerful but not valid Rust syntax.
    RichText,
}

/// Available rendering styles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RenderStyle {
    // ─────────────────────────────────────────────────────────────────────
    // Valid Rust renderers (output can be compiled)
    // ─────────────────────────────────────────────────────────────────────
    /// Inline comments at end of each line: `// x ●●○`
    #[default]
    Inline,
    /// Fixed columns for each variable with NLL transitions
    Columnar,
    /// Grouped transitions with horizontal rules and blank lines
    Grouped,

    // ─────────────────────────────────────────────────────────────────────
    // Rich text renderers (more typographical freedom)
    // ─────────────────────────────────────────────────────────────────────
    /// Rustc-style diagnostic format with line numbers and underlines
    Diagnostic,
    /// Set notation: `main{mut x, r(&x)}` - matches tutorial style
    SetNotation,
    /// Vertical borrow spans with box-drawing brackets
    VerticalSpans,
    /// Interactive HTML with Aquascope-inspired features
    Html,
    /// Validated output with rust-analyzer errors integrated
    Validated,
}

impl RenderStyle {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "inline" => Some(Self::Inline),
            "columnar" | "columns" => Some(Self::Columnar),
            "grouped" | "transitions" => Some(Self::Grouped),
            "diagnostic" | "diag" | "rustc" => Some(Self::Diagnostic),
            "set" | "set-notation" | "sets" => Some(Self::SetNotation),
            "vertical" | "spans" | "vertical-spans" => Some(Self::VerticalSpans),
            "html" => Some(Self::Html),
            "validated" | "semantic" | "ra" => Some(Self::Validated),
            _ => None,
        }
    }

    /// Returns the output category for this render style.
    pub fn category(&self) -> RenderCategory {
        match self {
            // Valid Rust - can be compiled
            Self::Inline | Self::Columnar | Self::Grouped => RenderCategory::ValidRust,
            // Rich text - more typographical freedom
            Self::Diagnostic
            | Self::SetNotation
            | Self::VerticalSpans
            | Self::Html
            | Self::Validated => RenderCategory::RichText,
        }
    }

    /// Returns true if output is syntactically valid Rust.
    pub fn is_valid_rust(&self) -> bool {
        self.category() == RenderCategory::ValidRust
    }

    /// Returns true if this style requires semantic analysis (rust-analyzer).
    pub fn requires_semantic(&self) -> bool {
        matches!(self, Self::Validated)
    }

    pub fn all() -> &'static [(&'static str, &'static str, RenderCategory)] {
        &[
            ("inline", "Comments at end of line", RenderCategory::ValidRust),
            ("columnar", "Fixed columns with NLL transitions", RenderCategory::ValidRust),
            ("grouped", "Horizontal rules with blank lines", RenderCategory::ValidRust),
            ("diagnostic", "Rustc-style with line numbers and underlines", RenderCategory::RichText),
            ("set-notation", "Set notation: main{mut x, r(&x)}", RenderCategory::RichText),
            ("vertical-spans", "Vertical borrow spans with brackets", RenderCategory::RichText),
            ("html", "Interactive HTML visualization", RenderCategory::RichText),
            ("validated", "Ownership state + rust-analyzer errors", RenderCategory::RichText),
        ]
    }
}

/// Configuration options for rendering.
#[derive(Debug, Clone, Default)]
pub struct RenderConfig {
    /// Filter out Copy types (bool, i32, etc.)
    pub filter_copy_types: bool,
    /// Comment column position
    pub comment_column: usize,
    /// Show verbose explanations
    pub verbose: bool,
}

impl RenderConfig {
    pub fn new() -> Self {
        Self {
            filter_copy_types: true,
            comment_column: 58,
            verbose: false,
        }
    }

    pub fn with_filter_copy(mut self, filter: bool) -> Self {
        self.filter_copy_types = filter;
        self
    }

    pub fn with_comment_column(mut self, col: usize) -> Self {
        self.comment_column = col;
        self
    }
}

/// Context passed to renderers with analyzed data.
pub struct RenderContext<'a> {
    pub source: &'a str,
    pub lines: Vec<&'a str>,
    pub sets_by_line: HashMap<u32, &'a OwnershipSet>,
    pub all_vars: Vec<String>,
    pub config: RenderConfig,
    /// Pre-built timeline with all state transitions recorded.
    pub timeline: StateTimeline,
}

impl<'a> RenderContext<'a> {
    pub fn new(
        source: &'a str,
        set_annotations: &'a [SetAnnotation],
        config: RenderConfig,
    ) -> Self {
        let lines: Vec<&str> = source.lines().collect();

        // Group sets by line
        let mut sets_by_line: HashMap<u32, &OwnershipSet> = HashMap::new();
        for set_ann in set_annotations {
            sets_by_line.insert(set_ann.line, &set_ann.set);
        }

        // Collect all variables in declaration order
        let mut all_vars: Vec<String> = Vec::new();
        for set_ann in set_annotations {
            for entry in &set_ann.set.entries {
                if !all_vars.contains(&entry.name) {
                    // Filter Copy types if configured
                    if config.filter_copy_types && is_likely_copy_type(&entry.name, entry) {
                        continue;
                    }
                    all_vars.push(entry.name.clone());
                }
            }
        }

        // Build the timeline by recording each line's state
        let mut timeline = StateTimeline::new();
        for (line_num, line) in lines.iter().enumerate() {
            if let Some(set) = sets_by_line.get(&(line_num as u32)) {
                let entries: Vec<&SetEntry> = set
                    .entries
                    .iter()
                    .filter(|e| {
                        if config.filter_copy_types && is_likely_copy_type(&e.name, e) {
                            return false;
                        }
                        true
                    })
                    .collect();
                timeline.record(line_num as u32, &entries, line);
            } else {
                // Record empty state to detect drops
                timeline.record(line_num as u32, &[], line);
            }
        }

        Self {
            source,
            lines,
            sets_by_line,
            all_vars,
            config,
            timeline,
        }
    }

    /// Filter a set's entries based on config.
    pub fn filter_entries<'b>(&self, set: &'b OwnershipSet) -> Vec<&'b SetEntry> {
        set.entries
            .iter()
            .filter(|e| {
                if self.config.filter_copy_types && is_likely_copy_type(&e.name, e) {
                    return false;
                }
                true
            })
            .collect()
    }

    /// Set semantic last-use data for accurate NLL drop detection.
    ///
    /// Takes a map from variable name to the line where it was last used.
    /// When set, this overrides heuristic drop detection.
    pub fn set_semantic_last_uses(&mut self, last_uses: std::collections::HashMap<String, u32>) {
        self.timeline.set_semantic_last_uses(last_uses);
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Renderer traits - separated by output category
// ─────────────────────────────────────────────────────────────────────────────

/// Trait for renderers that produce valid Rust output.
///
/// Implementations must ensure output can be compiled by rustc.
/// This typically means annotations are placed inside `//` comments.
pub trait ValidRustRenderer {
    /// Render source with annotations as valid Rust.
    fn render(&self, ctx: &mut RenderContext) -> String;
}

/// Trait for renderers that produce rich text output.
///
/// Implementations have full typographical freedom - box drawing,
/// diagrams, custom layouts. Output is for display only, not compilation.
pub trait RichTextRenderer {
    /// Render source with rich text annotations.
    fn render(&self, ctx: &mut RenderContext) -> String;
}

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

// ─────────────────────────────────────────────────────────────────────────────
// SetNotation Renderer - matches tutorial style: main{mut x, r(&x)}
// ─────────────────────────────────────────────────────────────────────────────

/// Set notation renderer - displays ownership sets like `main{mut x, r(&x)}`.
///
/// Matches the notation used in NOTES/rules.md for teaching ownership concepts.
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
fn format_set_notation(scope: &str, entries: &[&SetEntry], exclude: &[&str]) -> String {
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

/// Extract function name from a line like `fn foo() {` or `fn bar(x: i32) -> bool {`
fn extract_function_name(line: &str) -> Option<String> {
    let trimmed = line.trim();
    if !trimmed.starts_with("fn ") {
        return None;
    }
    let after_fn = &trimmed[3..];
    let name_end = after_fn.find(|c: char| c == '(' || c == '<' || c.is_whitespace())?;
    Some(after_fn[..name_end].to_string())
}

// ─────────────────────────────────────────────────────────────────────────────
// VerticalSpans Renderer - shows borrow lifetimes with vertical brackets
// ─────────────────────────────────────────────────────────────────────────────

/// Vertical spans renderer - shows borrow extents with box-drawing characters.
///
/// ```text
///     let mut x = vec![1, 2];         x ●●●
///                                       ╭─── r
///     let r = &x;                     x ●●○ │  r ○●○
///     println!("{}", r);              x ●●○ │  r ○●○
///                                       ╰─── r†
///     x.push(3);                      x ●●●
/// ```
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
            let new_borrows: Vec<&crate::util::StateChange> = changes
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

fn format_capability_dots(entry: &SetEntry) -> &'static str {
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

// ─────────────────────────────────────────────────────────────────────────────
// HTML Renderer - interactive visualization inspired by Aquascope
// ─────────────────────────────────────────────────────────────────────────────

/// HTML renderer - produces interactive HTML visualization.
///
/// Inspired by Aquascope, this renderer generates standalone HTML with:
/// - Syntax-highlighted source code
/// - Hover tooltips showing ownership state
/// - Visual indicators for borrows and moves
/// - Color-coded capability indicators
pub struct HtmlRenderer;

impl RichTextRenderer for HtmlRenderer {
    fn render(&self, ctx: &mut RenderContext) -> String {
        let mut lines_html = String::new();

        for (line_num, line) in ctx.lines.iter().enumerate() {
            let line_num = line_num as u32;
            let escaped_line = html_escape(line);

            // Get state for this line
            let state_html = if let Some(set) = ctx.sets_by_line.get(&line_num) {
                let entries = ctx.filter_entries(set);
                format_html_state(&entries)
            } else {
                String::new()
            };

            // Get any drops for this line
            let drops = ctx.timeline.get_pending_drops(line_num);
            let drops_html = if !drops.is_empty() {
                let drop_spans: Vec<String> = drops
                    .iter()
                    .map(|d| {
                        format!(
                            r#"<span class="drop" title="{} dropped (NLL)">{}†</span>"#,
                            d.name, d.name
                        )
                    })
                    .collect();
                format!(r#"<div class="nll-drop">{}</div>"#, drop_spans.join(" "))
            } else {
                String::new()
            };

            lines_html.push_str(&format!(
                r#"<div class="line" data-line="{}">
  <span class="line-num">{}</span>
  <code class="source">{}</code>
  <span class="state">{}</span>
  {}
</div>
"#,
                line_num + 1,
                line_num + 1,
                escaped_line,
                state_html,
                drops_html
            ));
        }

        format!(
            r#"<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Ownership Visualization</title>
  <style>
{CSS_STYLES}
  </style>
</head>
<body>
  <div class="container">
    <div class="code-view">
{lines_html}
    </div>
    <div class="legend">
      <h3>Legend</h3>
      <div class="legend-item"><span class="dot owned-mut"></span> Owned (mutable)</div>
      <div class="legend-item"><span class="dot owned"></span> Owned (immutable)</div>
      <div class="legend-item"><span class="dot shared"></span> Shared / Frozen</div>
      <div class="legend-item"><span class="dot borrow"></span> Borrow active</div>
      <div class="legend-item"><span class="dot dropped"></span> Dropped</div>
    </div>
  </div>
  <script>
{JS_SCRIPT}
  </script>
</body>
</html>"#,
            lines_html = lines_html,
            CSS_STYLES = CSS_STYLES,
            JS_SCRIPT = JS_SCRIPT
        )
    }
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn format_html_state(entries: &[&SetEntry]) -> String {
    entries
        .iter()
        .filter(|e| !matches!(e.state, SetEntryState::Dropped))
        .map(|e| {
            let (class, title) = match &e.state {
                SetEntryState::Owned => {
                    if e.mutable {
                        ("owned-mut", "Owned (mutable) - full access")
                    } else {
                        ("owned", "Owned (immutable) - read only")
                    }
                }
                SetEntryState::Shared => ("shared", "Shared - read only while borrowed"),
                SetEntryState::Frozen => ("frozen", "Frozen - no access while &mut active"),
                SetEntryState::SharedBorrow => ("borrow", "Shared borrow (&T)"),
                SetEntryState::MutBorrow => ("borrow-mut", "Mutable borrow (&mut T)"),
                SetEntryState::Dropped => ("dropped", "Dropped"),
            };
            let borrow_info = if let Some(from) = &e.borrows_from {
                format!(" (from {})", from)
            } else {
                String::new()
            };
            format!(
                r#"<span class="var {}" title="{}{}">{} {}</span>"#,
                class,
                title,
                borrow_info,
                e.name,
                format_capability_dots(e)
            )
        })
        .collect::<Vec<_>>()
        .join(" ")
}

const CSS_STYLES: &str = r#"
    :root {
      --bg: #1e1e2e;
      --fg: #cdd6f4;
      --line-num: #6c7086;
      --owned-mut: #a6e3a1;
      --owned: #94e2d5;
      --shared: #f9e2af;
      --frozen: #fab387;
      --borrow: #89b4fa;
      --borrow-mut: #cba6f7;
      --dropped: #f38ba8;
    }
    body {
      background: var(--bg);
      color: var(--fg);
      font-family: 'JetBrains Mono', 'Fira Code', monospace;
      margin: 0;
      padding: 20px;
    }
    .container {
      display: flex;
      gap: 40px;
      max-width: 1200px;
      margin: 0 auto;
    }
    .code-view {
      flex: 1;
      background: #181825;
      border-radius: 8px;
      padding: 16px;
      overflow-x: auto;
    }
    .line {
      display: flex;
      align-items: baseline;
      gap: 16px;
      padding: 2px 0;
      min-height: 1.5em;
    }
    .line:hover {
      background: rgba(255,255,255,0.05);
    }
    .line-num {
      color: var(--line-num);
      min-width: 2em;
      text-align: right;
      user-select: none;
    }
    .source {
      flex: 1;
      white-space: pre;
    }
    .state {
      display: flex;
      gap: 12px;
      font-size: 0.9em;
    }
    .var {
      padding: 2px 6px;
      border-radius: 4px;
      cursor: help;
    }
    .var.owned-mut { background: rgba(166,227,161,0.2); color: var(--owned-mut); }
    .var.owned { background: rgba(148,226,213,0.2); color: var(--owned); }
    .var.shared { background: rgba(249,226,175,0.2); color: var(--shared); }
    .var.frozen { background: rgba(250,179,135,0.2); color: var(--frozen); }
    .var.borrow { background: rgba(137,180,250,0.2); color: var(--borrow); }
    .var.borrow-mut { background: rgba(203,166,247,0.2); color: var(--borrow-mut); }
    .nll-drop {
      margin-left: 8px;
    }
    .drop {
      color: var(--dropped);
      font-size: 0.85em;
      cursor: help;
    }
    .legend {
      background: #181825;
      border-radius: 8px;
      padding: 16px;
      min-width: 200px;
    }
    .legend h3 {
      margin: 0 0 12px 0;
      font-size: 1em;
    }
    .legend-item {
      display: flex;
      align-items: center;
      gap: 8px;
      padding: 4px 0;
      font-size: 0.9em;
    }
    .dot {
      width: 12px;
      height: 12px;
      border-radius: 50%;
    }
    .dot.owned-mut { background: var(--owned-mut); }
    .dot.owned { background: var(--owned); }
    .dot.shared { background: var(--shared); }
    .dot.borrow { background: var(--borrow); }
    .dot.dropped { background: var(--dropped); }
"#;

const JS_SCRIPT: &str = r#"
    // Add interactivity - highlight related variables on hover
    document.querySelectorAll('.var').forEach(el => {
      el.addEventListener('mouseenter', () => {
        const varName = el.textContent.split(' ')[0];
        document.querySelectorAll('.var').forEach(other => {
          if (other.textContent.startsWith(varName + ' ')) {
            other.style.outline = '2px solid currentColor';
          }
        });
      });
      el.addEventListener('mouseleave', () => {
        document.querySelectorAll('.var').forEach(other => {
          other.style.outline = 'none';
        });
      });
    });
"#;

// ─────────────────────────────────────────────────────────────────────────────
// Validated Renderer - combines ownership state with rust-analyzer diagnostics
// ─────────────────────────────────────────────────────────────────────────────

/// Validated renderer - shows ownership state alongside rust-analyzer errors.
///
/// This renderer uses rust-analyzer's semantic analysis to show actual compiler
/// errors inline with ownership state annotations. Invalid Rust is clearly marked.
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

/// Find the position of a variable name in a line of code.
/// Prioritizes declaration patterns over usage.
fn find_var_position(line: &str, var_name: &str) -> Option<usize> {
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
fn format_change_label(change: &crate::util::StateChange) -> String {
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

// ─────────────────────────────────────────────────────────────────────────────
// Helper functions
// ─────────────────────────────────────────────────────────────────────────────

fn pad_to_column(current: usize, target: usize) -> usize {
    if current < target {
        target - current
    } else {
        2
    }
}

fn is_likely_copy_type(_name: &str, _entry: &SetEntry) -> bool {
    // TODO: Implement proper type inference to detect Copy types
    // For now, don't filter anything - show all variables
    false
}

fn format_entry_short(entry: &SetEntry) -> String {
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

fn format_entries_inline(entries: &[&SetEntry]) -> String {
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

fn format_entries_columnar(
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

fn format_entries_compact(entries: &[&SetEntry], timeline: &StateTimeline) -> String {
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

// ─────────────────────────────────────────────────────────────────────────────
// Public API
// ─────────────────────────────────────────────────────────────────────────────

/// Render source with the specified style.
///
/// Returns annotated source code. Check `style.category()` to determine
/// whether the output is valid Rust or rich text.
///
/// Note: For `RenderStyle::Validated`, use `render_source_semantic` instead
/// which provides rust-analyzer diagnostics.
pub fn render_source(source: &str, style: RenderStyle, config: RenderConfig) -> String {
    let mut analyzer = OwnershipAnalyzer::new();

    let Ok(_) = analyzer.analyze(source) else {
        return source.to_string();
    };

    let set_annotations = analyzer.set_annotations();
    let mut ctx = RenderContext::new(source, set_annotations, config);

    match style {
        // ValidRustRenderer implementations
        RenderStyle::Inline => ValidRustRenderer::render(&InlineRenderer, &mut ctx),
        RenderStyle::Columnar => ValidRustRenderer::render(&ColumnarRenderer, &mut ctx),
        RenderStyle::Grouped => ValidRustRenderer::render(&GroupedRenderer, &mut ctx),
        // RichTextRenderer implementations
        RenderStyle::Diagnostic => RichTextRenderer::render(&DiagnosticRenderer, &mut ctx),
        RenderStyle::SetNotation => RichTextRenderer::render(&SetNotationRenderer, &mut ctx),
        RenderStyle::VerticalSpans => RichTextRenderer::render(&VerticalSpansRenderer, &mut ctx),
        RenderStyle::Html => RichTextRenderer::render(&HtmlRenderer, &mut ctx),
        // Validated requires semantic analysis - fallback to SetNotation
        RenderStyle::Validated => {
            // Without file path, we can't load rust-analyzer
            // Fall back to SetNotation with a warning
            let mut output = String::from(
                "╭─ rust-analyzer: ⚠ semantic analysis requires file path\n│  use render_source_semantic() instead\n│\n",
            );
            output.push_str(&RichTextRenderer::render(&SetNotationRenderer, &mut ctx));
            output
        }
    }
}

/// Render source with semantic analysis from rust-analyzer.
///
/// This loads the cargo workspace and provides actual compiler diagnostics
/// alongside ownership state annotations. Uses accurate NLL drop detection
/// based on rust-analyzer's reference analysis.
///
/// Returns `None` if the file is not part of a cargo project or loading fails.
pub fn render_source_semantic(
    file_path: &Path,
    source: &str,
    style: RenderStyle,
    config: RenderConfig,
) -> Option<String> {
    // Load semantic analysis
    let analyzer = match SemanticAnalyzer::load(file_path) {
        SemanticResult::Available(a) => a,
        SemanticResult::NotCargoProject => return None,
        SemanticResult::LoadFailed => return None,
    };

    let file_id = analyzer.file_id(file_path)?;
    let diagnostics = analyzer.diagnostics(file_id);

    // Get semantic last-use data for accurate NLL drops
    // Uses drop_line which accounts for loop boundaries
    let last_uses = analyzer.find_all_last_uses(file_id, source);
    let semantic_drops: std::collections::HashMap<String, u32> = last_uses
        .values()
        .map(|info| (info.name.clone(), info.drop_line))
        .collect();

    // Run ownership analysis
    let mut ownership_analyzer = OwnershipAnalyzer::new();
    let Ok(_) = ownership_analyzer.analyze(source) else {
        return None;
    };

    let set_annotations = ownership_analyzer.set_annotations();
    let mut base_ctx = RenderContext::new(source, set_annotations, config.clone());
    base_ctx.set_semantic_last_uses(semantic_drops.clone());

    // For Validated style, use the special renderer
    if matches!(style, RenderStyle::Validated) {
        let mut validated_ctx = ValidatedRenderContext::new(base_ctx, &diagnostics, source);
        return Some(ValidatedRenderer::render(&mut validated_ctx));
    }

    // For other styles, use the regular renderer with semantic drops
    let mut ctx = RenderContext::new(source, ownership_analyzer.set_annotations(), config);
    ctx.set_semantic_last_uses(semantic_drops);

    Some(match style {
        RenderStyle::Inline => ValidRustRenderer::render(&InlineRenderer, &mut ctx),
        RenderStyle::Columnar => ValidRustRenderer::render(&ColumnarRenderer, &mut ctx),
        RenderStyle::Grouped => ValidRustRenderer::render(&GroupedRenderer, &mut ctx),
        RenderStyle::Diagnostic => RichTextRenderer::render(&DiagnosticRenderer, &mut ctx),
        RenderStyle::SetNotation => RichTextRenderer::render(&SetNotationRenderer, &mut ctx),
        RenderStyle::VerticalSpans => RichTextRenderer::render(&VerticalSpansRenderer, &mut ctx),
        RenderStyle::Html => RichTextRenderer::render(&HtmlRenderer, &mut ctx),
        RenderStyle::Validated => unreachable!(),
    })
}
