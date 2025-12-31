//! Render context - holds analyzed data for renderers.

use crate::analysis::{OwnershipSet, SetAnnotation, SetEntry, SetEntryState};
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
    /// Semantic drop lines for NLL tracking.
    semantic_drop_lines: HashMap<(String, u32), u32>,
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
            semantic_drop_lines: HashMap::new(),
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
}
