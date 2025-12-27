//! Pluggable renderer framework for ownership annotations.
//!
//! Provides a trait-based system for different output styles,
//! making it easy to experiment with new visualizations.

use crate::analysis::{OwnershipAnalyzer, OwnershipSet, SetAnnotation, SetEntry, SetEntryState};
use crate::util::{ChangeType, StateTimeline};
use std::collections::HashMap;

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
}

impl RenderStyle {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "inline" => Some(Self::Inline),
            "columnar" | "columns" => Some(Self::Columnar),
            "grouped" | "transitions" => Some(Self::Grouped),
            "diagnostic" | "diag" | "rustc" => Some(Self::Diagnostic),
            _ => None,
        }
    }

    /// Returns the output category for this render style.
    pub fn category(&self) -> RenderCategory {
        match self {
            // Valid Rust - can be compiled
            Self::Inline | Self::Columnar | Self::Grouped => RenderCategory::ValidRust,
            // Rich text - more typographical freedom
            Self::Diagnostic => RenderCategory::RichText,
        }
    }

    /// Returns true if output is syntactically valid Rust.
    pub fn is_valid_rust(&self) -> bool {
        self.category() == RenderCategory::ValidRust
    }

    pub fn all() -> &'static [(&'static str, &'static str, RenderCategory)] {
        &[
            ("inline", "Comments at end of line", RenderCategory::ValidRust),
            ("columnar", "Fixed columns with NLL transitions", RenderCategory::ValidRust),
            ("grouped", "Horizontal rules with blank lines", RenderCategory::ValidRust),
            ("diagnostic", "Rustc-style with line numbers and underlines", RenderCategory::RichText),
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
        let mut prev_line: Option<&str> = None;

        // Header
        output.push_str(&format!("{:>width$} ╭─\n", "", width = line_num_width));

        for (line_num, line) in ctx.lines.iter().enumerate() {
            let line_num_u32 = line_num as u32;
            let display_num = line_num + 1;

            // Get pending drops from timeline (shown before the line)
            let pending_drops = ctx.timeline.get_pending_drops(line_num_u32);

            // Show NLL drop annotation pointing to where var appeared on PREVIOUS line
            if !pending_drops.is_empty() && !line.trim().is_empty() {
                if let Some(prev) = prev_line {
                    for drop in &pending_drops {
                        if let Some(pos) = find_var_position(prev, &drop.name) {
                            // Build a line with ^ at the variable position
                            let max_pos = prev.chars().count();
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
                        }
                        ctx.timeline.mark_drop_shown(&drop.name);
                    }
                }
            }

            // Print the source line with line number
            output.push_str(&format!(
                "{:>width$} │ {}\n",
                display_num,
                line,
                width = line_num_width
            ));

            // Get state changes from timeline
            let changes = ctx.timeline.get_changes(line_num_u32);

            // Collect annotations for this line
            let mut annotations: Vec<(String, usize, String)> = Vec::new(); // (var_name, position, label)

            for change in &changes {
                // Find position of variable in the line
                if let Some(pos) = find_var_position(line, &change.name) {
                    let label = format_change_label(change);
                    annotations.push((change.name.clone(), pos, label));
                }
            }

            // Render underline annotations
            if !annotations.is_empty() {
                // Sort by position (rightmost first for rendering order)
                annotations.sort_by_key(|(_, pos, _)| std::cmp::Reverse(*pos));

                // Build underline line with ─ under each variable
                let max_pos = line.chars().count();
                let mut underline_chars: Vec<char> = vec![' '; max_pos];

                for (name, pos, _) in &annotations {
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
                // For all but the last (leftmost), we need to show | connectors
                for (idx, (name, pos, label)) in annotations.iter().enumerate() {
                    let is_last = idx == annotations.len() - 1;

                    // Build the connector line
                    let mut connector_chars: Vec<char> = vec![' '; max_pos];

                    // Add | for all annotations to the left of this one (which are later in the sorted list)
                    if !is_last {
                        for (_, other_pos, _) in annotations.iter().skip(idx + 1) {
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

                    output.push_str(&format!(
                        "{:>width$} │ {}─ {}: {}\n",
                        "",
                        connector_trimmed,
                        name,
                        label,
                        width = line_num_width
                    ));
                }
            }

            prev_line = Some(line);
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
    }
}
