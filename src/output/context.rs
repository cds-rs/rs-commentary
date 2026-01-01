//! Render context - holds analyzed data for renderers.

use crate::analysis::{BindingKind, CopyEvent, OwnershipSet, SetAnnotation, SetEntry, SetEntryState};
use crate::util::StateTimeline;
use super::types::RenderConfig;
use std::collections::HashMap;

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
}
