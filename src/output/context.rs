//! Render context - holds analyzed data for renderers.

use crate::analysis::{BindingKind, CopyEvent, OwnershipSet, SetAnnotation, SetEntry, SetEntryState};
use crate::util::{StateTimeline, StateTransition, VarSnapshot};
use super::types::RenderConfig;
use std::collections::{HashMap, HashSet};

// ============================================================================
// Unified Line Content - Single source of truth for all renderers
// ============================================================================

/// Processed transfer event with computed "still valid" status.
#[derive(Debug, Clone)]
pub struct ProcessedCopy {
    /// Variable being transferred.
    pub from: String,
    /// Target (function name or variable).
    pub to: String,
    /// Kind of transfer (Copy, Move, SharedBorrow, MutBorrow).
    pub kind: crate::analysis::TransferKind,
    /// Whether the source variable is still used after this line.
    pub still_valid: bool,
    /// Whether this is the first time we're showing "still valid" for this var.
    pub show_still_valid: bool,
}

/// Unified line content - all pedagogically relevant info for a line.
/// This is the single source of truth that all renderers consume.
#[derive(Debug, Clone)]
pub struct LineContent {
    /// Line number (0-indexed).
    pub line_num: u32,
    /// Source text for this line.
    pub line_text: String,
    /// Variables newly introduced on this line.
    pub new_vars: Vec<VarSnapshot>,
    /// State transitions on this line.
    pub transitions: Vec<(String, StateTransition)>,
    /// Variables dropped on this line.
    pub drops: Vec<VarSnapshot>,
    /// Processed copy events with "still valid" info.
    pub copy_events: Vec<ProcessedCopy>,
}

impl LineContent {
    /// Returns true if this line has pedagogical value worth showing.
    pub fn is_interesting(&self) -> bool {
        // Skip empty lines
        if self.line_text.trim().is_empty() {
            return false;
        }

        // Skip lines that are just closing braces with no ownership events
        let trimmed = self.line_text.trim();
        if trimmed == "}" || trimmed == "};" {
            return !self.drops.is_empty();
        }

        // Interesting if any ownership event happens
        !self.new_vars.is_empty()
            || !self.transitions.is_empty()
            || !self.drops.is_empty()
            || !self.copy_events.is_empty()
    }
}

/// Tracks state across lines to avoid duplicate annotations.
#[derive(Debug, Default)]
pub struct AnnotationTracker {
    /// Variables for which we've already shown "still valid".
    shown_still_valid: HashSet<String>,
    /// Variables for which we've already shown introduction.
    shown_introductions: HashSet<String>,
}

impl AnnotationTracker {
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if we should show "still valid" for a variable, and mark it as shown.
    /// Returns true if this is the first time we're showing "still valid" for this var.
    pub fn should_show_still_valid(&mut self, name: &str) -> bool {
        if self.shown_still_valid.contains(name) {
            false
        } else {
            self.shown_still_valid.insert(name.to_string());
            true
        }
    }

    /// Check if we should show introduction for a variable, and mark it as shown.
    /// Returns true if this is the first time we're showing this introduction.
    pub fn should_show_introduction(&mut self, name: &str) -> bool {
        if self.shown_introductions.contains(name) {
            false
        } else {
            self.shown_introductions.insert(name.to_string());
            true
        }
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
    /// Use `timeline.boundary_state(line)` to get rich diff information.
    pub timeline: StateTimeline,
    /// Semantic Copy type info: maps variable name to whether it implements Copy.
    semantic_copy_types: HashMap<String, bool>,
    /// Semantic binding kinds: maps (name, decl_line) to type classification.
    /// Keyed by declaration line to disambiguate same-named variables in different scopes.
    semantic_binding_kinds: HashMap<(String, u32), BindingKind>,
    /// Semantic drop lines for NLL tracking.
    semantic_drop_lines: HashMap<(String, u32), u32>,
    /// Synthetic copy events: line -> copies on that line.
    copy_events_by_line: HashMap<u32, Vec<CopyEvent>>,
}

impl<'a> RenderContext<'a> {
    pub fn new(
        source: &'a str,
        set_annotations: &'a [SetAnnotation],
        config: RenderConfig,
    ) -> Self {
        Self::new_with_semantic(source, set_annotations, config, HashMap::new())
    }

    /// Create a render context with semantic Copy type information.
    pub fn new_with_semantic(
        source: &'a str,
        set_annotations: &'a [SetAnnotation],
        config: RenderConfig,
        copy_types: HashMap<String, bool>,
    ) -> Self {
        let lines: Vec<&str> = source.lines().collect();

        // Group sets by line
        let mut sets_by_line: HashMap<u32, &OwnershipSet> = HashMap::new();
        for set_ann in set_annotations {
            sets_by_line.insert(set_ann.line, &set_ann.set);
        }

        // Helper to check if a variable is Copy
        let is_copy = |name: &str| -> bool {
            copy_types.get(name).copied().unwrap_or(false)
        };

        // Collect all variables in declaration order
        let mut all_vars: Vec<String> = Vec::new();
        for set_ann in set_annotations {
            for entry in &set_ann.set.entries {
                if !all_vars.contains(&entry.name) {
                    if config.filter_copy_types && is_copy(&entry.name)
                        && !matches!(entry.state, SetEntryState::SharedBorrow | SetEntryState::MutBorrow) {
                        continue;
                    }
                    all_vars.push(entry.name.clone());
                }
            }
        }

        // Build the timeline
        let mut timeline = StateTimeline::new();
        for (line_num, line) in lines.iter().enumerate() {
            if let Some(set) = sets_by_line.get(&(line_num as u32)) {
                let entries: Vec<&SetEntry> = set
                    .entries
                    .iter()
                    .filter(|e| {
                        if matches!(e.state, SetEntryState::SharedBorrow | SetEntryState::MutBorrow) {
                            return true;
                        }
                        if config.filter_copy_types && is_copy(&e.name) {
                            return false;
                        }
                        true
                    })
                    .collect();
                timeline.record(line_num as u32, &entries, line);
            } else {
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
            semantic_copy_types: copy_types,
            semantic_binding_kinds: HashMap::new(),
            semantic_drop_lines: HashMap::new(),
            copy_events_by_line: HashMap::new(),
        }
    }

    /// Check if a variable is a Copy type.
    pub fn is_copy_type(&self, name: &str) -> bool {
        self.semantic_copy_types.get(name).copied().unwrap_or(false)
    }

    /// Filter a set's entries based on config.
    pub fn filter_entries<'b>(&self, set: &'b OwnershipSet) -> Vec<&'b SetEntry> {
        set.entries
            .iter()
            .filter(|e| {
                if self.config.filter_copy_types && self.is_copy_type(&e.name) {
                    return false;
                }
                true
            })
            .collect()
    }

    /// Set semantic drop lines for accurate NLL drop detection.
    /// Keys are (name, decl_line) to disambiguate variables with the same name.
    pub fn set_semantic_drop_lines(&mut self, drop_lines: HashMap<(String, u32), u32>) {
        // Store for future use
        self.semantic_drop_lines = drop_lines.clone();

        // Convert to name-only for timeline (may have collisions, but okay for now)
        // Timeline expects last_use line, so subtract 1 from drop_line
        let last_uses: HashMap<String, u32> = drop_lines
            .iter()
            .map(|((name, _), &drop_line)| (name.clone(), drop_line.saturating_sub(1)))
            .collect();
        self.timeline.set_semantic_last_uses(last_uses);
    }

    /// Set semantic Copy type info for accurate filtering.
    pub fn set_semantic_copy_types(&mut self, copy_types: HashMap<String, bool>) {
        self.semantic_copy_types = copy_types;
    }

    /// Set semantic binding kinds for accurate messaging.
    ///
    /// This enables proper "borrow ends" vs "dropped" messaging based on
    /// whether a binding is a reference type.
    pub fn set_binding_kinds(&mut self, kinds: HashMap<(String, u32), BindingKind>) {
        self.semantic_binding_kinds = kinds;
    }

    /// Get the semantic binding kind for a variable at a specific line.
    ///
    /// Uses the line number to disambiguate variables with the same name
    /// in different scopes (e.g., a local `cache` vs a parameter `cache`).
    ///
    /// Returns `None` if no semantic info is available (AST-only analysis).
    pub fn get_binding_kind(&self, name: &str, line: u32) -> Option<BindingKind> {
        // First try exact match on (name, line)
        if let Some(kind) = self.semantic_binding_kinds.get(&(name.to_string(), line)) {
            return Some(*kind);
        }

        // For uses after declaration, find the binding declared at or before this line
        // by looking for the closest decl_line <= line
        let mut best_match: Option<(u32, BindingKind)> = None;
        for ((n, decl_line), kind) in &self.semantic_binding_kinds {
            if n == name && *decl_line <= line {
                if best_match.map_or(true, |(best_line, _)| *decl_line > best_line) {
                    best_match = Some((*decl_line, *kind));
                }
            }
        }
        best_match.map(|(_, kind)| kind)
    }

    /// Set copy events from the analyzer.
    pub fn set_copy_events(&mut self, events: &[CopyEvent]) {
        for event in events {
            self.copy_events_by_line
                .entry(event.line)
                .or_default()
                .push(event.clone());
        }
    }

    /// Get copy events for a specific line.
    pub fn get_copy_events(&self, line: u32) -> &[CopyEvent] {
        self.copy_events_by_line
            .get(&line)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Check if a variable is dead (no more uses) after a given line.
    ///
    /// Returns true if the variable's drop_line is at or before `after_line + 1`,
    /// meaning there are no uses of the variable after `after_line`.
    pub fn is_dead_after(&self, name: &str, after_line: u32) -> bool {
        // Find the best matching declaration for this name
        // (the one with decl_line <= after_line and closest to after_line)
        let mut best_drop_line: Option<u32> = None;
        let mut best_decl_line: u32 = 0;

        for ((n, decl_line), drop_line) in &self.semantic_drop_lines {
            if n == name && *decl_line <= after_line {
                if best_drop_line.is_none() || *decl_line > best_decl_line {
                    best_drop_line = Some(*drop_line);
                    best_decl_line = *decl_line;
                }
            }
        }

        // If drop_line <= after_line + 1, the variable is dead after after_line
        best_drop_line.map_or(false, |drop_line| drop_line <= after_line + 1)
    }

    /// Get state changes for a line, filtering out variables that have copy events.
    ///
    /// At call sites, the copy annotation ("copied → fn") is more informative than
    /// the state annotation ("●●● owned mut"), so we suppress the state when both exist.
    pub fn get_filtered_changes(&self, line: u32) -> Vec<crate::util::StateChange> {
        let changes = self.timeline.get_changes(line);
        let copy_events = self.get_copy_events(line);

        if copy_events.is_empty() {
            return changes;
        }

        // Collect names of variables with copy events
        let copy_var_names: HashSet<&str> =
            copy_events.iter().map(|c| c.from.as_str()).collect();

        // Filter out state changes for those variables
        changes
            .into_iter()
            .filter(|change| !copy_var_names.contains(change.name.as_str()))
            .collect()
    }

    /// Get unified line content for a line.
    ///
    /// This is the single source of truth for all renderers. It combines:
    /// - New variable introductions (deduplicated via tracker)
    /// - State transitions
    /// - Drops
    /// - Copy events with "still valid" info (deduplicated via tracker)
    pub fn get_line_content(&self, line: u32, tracker: &mut AnnotationTracker) -> LineContent {
        let line_text = self.lines.get(line as usize).copied().unwrap_or("").to_string();
        let boundary = self.timeline.boundary_state(line);

        // Process copy events with deduplication
        let raw_copies = self.get_copy_events(line);
        let copy_events: Vec<ProcessedCopy> = raw_copies
            .iter()
            .filter(|c| !c.is_scalar_in_call) // Skip low-value scalar copies
            .map(|c| {
                let still_valid = !self.is_dead_after(&c.from, line);
                let show_still_valid = if still_valid && !tracker.shown_still_valid.contains(&c.from) {
                    tracker.shown_still_valid.insert(c.from.clone());
                    true
                } else {
                    false
                };
                ProcessedCopy {
                    from: c.from.clone(),
                    to: c.to.clone(),
                    kind: c.kind,
                    still_valid,
                    show_still_valid,
                }
            })
            .collect();

        if let Some(state) = boundary {
            // Filter new vars to avoid duplicate introductions
            let new_vars: Vec<VarSnapshot> = state
                .new_vars
                .into_iter()
                .filter(|v| {
                    if tracker.shown_introductions.contains(&v.name) {
                        false
                    } else {
                        tracker.shown_introductions.insert(v.name.clone());
                        true
                    }
                })
                .collect();

            LineContent {
                line_num: line,
                line_text,
                new_vars,
                transitions: state.delta_vars.into_iter().collect(),
                drops: state.dropped_vars,
                copy_events,
            }
        } else {
            LineContent {
                line_num: line,
                line_text,
                new_vars: Vec::new(),
                transitions: Vec::new(),
                drops: Vec::new(),
                copy_events,
            }
        }
    }
}
