//! State timeline for ownership tracking.
//!
//! Provides a time-traveling state machine that records ownership state
//! at each line, allowing queries at any point in the timeline.

use std::collections::{HashMap, HashSet};

use crate::analysis::{SetEntry, SetEntryState};

/// State snapshot at a specific line.
#[derive(Debug, Clone, Default)]
pub struct LineState {
    /// Variables live at this line.
    pub live: HashSet<String>,
    /// State of each variable: (state, mutable).
    pub states: HashMap<String, (SetEntryState, bool)>,
    /// Variables that were dropped at this line.
    pub dropped_here: Vec<String>,
    /// Borrow relationships: borrow_name -> source_name (e.g., "y" -> "x" for `let y = &x`).
    pub borrows_from: HashMap<String, String>,
}

/// Information about a variable that was dropped.
#[derive(Debug, Clone)]
pub struct VariableDrop {
    /// Name of the dropped variable.
    pub name: String,
    /// Line where it was last seen.
    pub last_seen_line: u32,
    /// If this was a borrow, the source variable that is now freed.
    /// When a borrow ends, its source returns to owned state.
    pub frees_source: Option<String>,
}

/// A time-traveling state machine for ownership tracking.
///
/// Records state at each line and allows querying any point in history.
/// Supports function-scoped variable tracking - each function maintains
/// its own set of live variables, isolated from other functions.
#[derive(Debug, Default)]
pub struct StateTimeline {
    /// State snapshot at each line number.
    history: HashMap<u32, LineState>,
    /// Current line being processed.
    current_line: u32,
    /// Variables already shown as dropped (to avoid duplicates).
    shown_dropped: HashSet<String>,
    /// Scope boundaries (function starts) for reset points.
    scope_starts: Vec<u32>,
    /// Semantic last-use data: map from variable name to last-use line (0-indexed).
    /// When present, this overrides heuristic drop detection.
    semantic_last_uses: HashMap<String, u32>,
    /// Current function scope name.
    current_scope: String,
    /// Map from line number to scope name.
    line_to_scope: HashMap<u32, String>,
    /// Accumulated live variables per scope (carried forward within scope).
    scope_live_vars: HashMap<String, HashSet<String>>,
}

impl StateTimeline {
    /// Create a new empty timeline.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set semantic last-use data from rust-analyzer.
    ///
    /// When set, this overrides heuristic drop detection with accurate
    /// NLL drop points based on actual last-use analysis.
    pub fn set_semantic_last_uses(&mut self, last_uses: HashMap<String, u32>) {
        self.semantic_last_uses = last_uses;
    }

    /// Check if semantic drop data is available.
    pub fn has_semantic_drops(&self) -> bool {
        !self.semantic_last_uses.is_empty()
    }

    /// Get the line where a variable should be shown as dropped.
    ///
    /// Uses semantic last-use data from rust-analyzer when available.
    /// Returns `Some(line)` if the variable should be dropped after `line`,
    /// or `None` if no semantic data is available for this variable.
    pub fn get_semantic_drop_line(&self, name: &str) -> Option<u32> {
        self.semantic_last_uses.get(name).copied()
    }

    /// Check if a variable should be shown as dropped at this line.
    ///
    /// Uses semantic data when available: drop shows on line after last use.
    /// Returns `None` if no semantic data exists (caller should use heuristic).
    pub fn should_show_drop_at(&self, name: &str, line_num: u32) -> Option<bool> {
        self.semantic_last_uses.get(name).map(|&last_use_line| {
            // Drop annotation appears on the line AFTER last use
            last_use_line + 1 == line_num && !self.shown_dropped.contains(name)
        })
    }

    /// Check if a line is a function boundary that starts a new scope.
    pub fn is_function_boundary(line: &str) -> bool {
        let trimmed = line.trim();
        trimmed.starts_with("fn ")
            || trimmed.starts_with("pub fn ")
            || trimmed.starts_with("pub(crate) fn ")
            || trimmed.starts_with("async fn ")
            || trimmed.starts_with("pub async fn ")
            || trimmed.starts_with("const fn ")
            || trimmed.starts_with("pub const fn ")
            || trimmed.starts_with("unsafe fn ")
            || trimmed.starts_with("pub unsafe fn ")
    }

    /// Extract function name from a function declaration line.
    pub fn extract_function_name(line: &str) -> String {
        let trimmed = line.trim();
        // Find "fn " and extract the name after it
        if let Some(fn_pos) = trimmed.find("fn ") {
            let after_fn = &trimmed[fn_pos + 3..];
            // Function name ends at '(' or '<' (generics)
            let end = after_fn
                .find(|c: char| c == '(' || c == '<')
                .unwrap_or(after_fn.len());
            return after_fn[..end].trim().to_string();
        }
        "unknown".to_string()
    }

    /// Get the scope name for a line.
    pub fn get_scope(&self, line_num: u32) -> Option<&String> {
        self.line_to_scope.get(&line_num)
    }

    /// Record state for a line, computing transitions from previous line.
    /// Maintains function-scoped variable tracking.
    pub fn record(&mut self, line_num: u32, entries: &[&SetEntry], line_text: &str) {
        // Check for scope boundary - entering a new function
        if Self::is_function_boundary(line_text) {
            let func_name = Self::extract_function_name(line_text);
            self.current_scope = func_name.clone();
            self.scope_starts.push(line_num);
            self.shown_dropped.clear();
            // Initialize fresh variable set for this scope
            self.scope_live_vars.insert(func_name.clone(), HashSet::new());
        }

        // Record which scope this line belongs to
        self.line_to_scope.insert(line_num, self.current_scope.clone());

        // Get variables that should be live in this scope
        let scope_vars = self.scope_live_vars
            .get(&self.current_scope)
            .cloned()
            .unwrap_or_default();

        // Filter entries to only include variables from current scope
        // New variables are added to scope, existing ones are kept
        let mut new_scope_vars = scope_vars.clone();
        for entry in entries {
            if !matches!(entry.state, SetEntryState::Dropped) {
                new_scope_vars.insert(entry.name.clone());
            }
        }

        // Compute current live set - only variables in this scope
        let live: HashSet<String> = entries
            .iter()
            .filter(|e| !matches!(e.state, SetEntryState::Dropped))
            .filter(|e| new_scope_vars.contains(&e.name))
            .map(|e| e.name.clone())
            .collect();

        // Compute states - only for variables in this scope
        let states: HashMap<String, (SetEntryState, bool)> = entries
            .iter()
            .filter(|e| new_scope_vars.contains(&e.name))
            .map(|e| (e.name.clone(), (e.state.clone(), e.mutable)))
            .collect();

        // Extract borrow relationships
        let borrows_from: HashMap<String, String> = entries
            .iter()
            .filter(|e| new_scope_vars.contains(&e.name))
            .filter_map(|e| e.borrows_from.as_ref().map(|from| (e.name.clone(), from.clone())))
            .collect();

        // Detect drops within this scope
        let prev_state = self.get_in_scope(self.current_line, &self.current_scope);
        let dropped_here: Vec<String> = if let Some(prev) = prev_state {
            if !line_text.trim().starts_with('}') {
                prev.live
                    .iter()
                    .filter(|v| !live.contains(*v))
                    .filter(|v| prev.states.contains_key(*v))
                    .filter(|v| new_scope_vars.contains(*v))
                    .cloned()
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        // Remove dropped variables from scope tracking
        for dropped in &dropped_here {
            new_scope_vars.remove(dropped);
        }

        // Update scope's live variables
        self.scope_live_vars.insert(self.current_scope.clone(), new_scope_vars);

        self.history.insert(
            line_num,
            LineState {
                live,
                states,
                dropped_here,
                borrows_from,
            },
        );
        self.current_line = line_num;
    }

    /// Get state at a line, but only if it's in the specified scope.
    fn get_in_scope(&self, line_num: u32, scope: &str) -> Option<&LineState> {
        if self.line_to_scope.get(&line_num).map(|s| s.as_str()) == Some(scope) {
            self.history.get(&line_num)
        } else {
            None
        }
    }

    /// Get state at a specific line (time travel).
    pub fn get(&self, line_num: u32) -> Option<&LineState> {
        self.history.get(&line_num)
    }

    /// Get state at the previous line.
    pub fn prev(&self) -> Option<&LineState> {
        if self.current_line > 0 {
            self.get(self.current_line - 1)
        } else {
            None
        }
    }

    /// Get the current line number.
    pub fn current_line(&self) -> u32 {
        self.current_line
    }

    /// Get drops that occurred before this line.
    ///
    /// When semantic data is available, uses accurate NLL drop points.
    /// Otherwise falls back to heuristic state-transition detection.
    /// Only returns drops for variables in the current line's scope.
    ///
    /// Returns `VariableDrop` with `frees_source` populated when a borrow ends,
    /// indicating that the source variable returns to owned state.
    pub fn get_pending_drops(&self, line_num: u32) -> Vec<VariableDrop> {
        let mut drops = Vec::new();

        // Get current line's scope
        let current_scope = self.line_to_scope.get(&line_num).cloned().unwrap_or_default();

        // Get current line state to look up borrow relationships
        let current_state = self.get(line_num);

        // Get variables that are in this scope
        let scope_vars = self.scope_live_vars
            .get(&current_scope)
            .cloned()
            .unwrap_or_default();

        // Check semantic drops first - only for variables in current scope
        for (name, &last_use_line) in &self.semantic_last_uses {
            // Only process if variable is in current scope
            if !scope_vars.contains(name) && !current_state.map(|s| s.states.contains_key(name)).unwrap_or(false) {
                continue;
            }

            if let Some(true) = self.should_show_drop_at(name, line_num) {
                // Check if this was a borrow - look up in current state's borrow map
                let frees_source = current_state
                    .and_then(|s| s.borrows_from.get(name).cloned());

                drops.push(VariableDrop {
                    name: name.clone(),
                    last_seen_line: last_use_line,
                    frees_source,
                });
            }
        }

        // If we have semantic data, use only that
        if !self.semantic_last_uses.is_empty() {
            return drops;
        }

        // Fallback to heuristic detection
        // Variables in dropped_here were detected as dropped during record()
        // They may no longer be in scope_vars (removed after drop), so don't filter by scope
        self.get(line_num)
            .map(|state| {
                state
                    .dropped_here
                    .iter()
                    .filter(|name| !self.shown_dropped.contains(*name))
                    .map(|name| {
                        let frees_source = state.borrows_from.get(name).cloned();
                        VariableDrop {
                            name: name.clone(),
                            last_seen_line: line_num.saturating_sub(1),
                            frees_source,
                        }
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Mark a variable as shown dropped (to avoid duplicate annotations).
    pub fn mark_drop_shown(&mut self, name: &str) {
        self.shown_dropped.insert(name.to_string());
    }

    /// Check if a drop has already been shown.
    pub fn is_drop_shown(&self, name: &str) -> bool {
        self.shown_dropped.contains(name)
    }

    /// Get the effective state for a line, accounting for NLL borrow endings.
    ///
    /// When a borrow ends (detected via semantic drop), the source variable
    /// transitions back to owned state. This method returns the corrected states.
    /// Only returns variables that are still live in the current scope.
    ///
    /// Returns a map of variable name to (effective_state, mutable, is_freed_from_borrow).
    pub fn get_effective_state(&self, line_num: u32) -> HashMap<String, (SetEntryState, bool, bool)> {
        let mut result = HashMap::new();

        let Some(state) = self.get(line_num) else {
            return result;
        };

        // Get current scope and its live variables
        let current_scope = self.line_to_scope.get(&line_num).cloned().unwrap_or_default();
        let scope_vars = self.scope_live_vars
            .get(&current_scope)
            .cloned()
            .unwrap_or_default();

        // Get drops for this line to find freed sources
        let drops = self.get_pending_drops(line_num);
        let freed_sources: HashSet<String> = drops
            .iter()
            .filter_map(|d| d.frees_source.clone())
            .collect();
        let drop_names: HashSet<&str> = drops.iter().map(|d| d.name.as_str()).collect();

        for (name, (entry_state, mutable)) in &state.states {
            // Only include variables that are still live in this scope
            if !scope_vars.contains(name) {
                continue;
            }
            // Skip dropped entries
            if matches!(entry_state, SetEntryState::Dropped) {
                continue;
            }
            // Skip borrows that are ending (they'll be in drops)
            if drop_names.contains(name.as_str()) {
                continue;
            }

            // Check if this variable is freed from a borrow
            if freed_sources.contains(name) {
                // Variable returns to owned state
                result.insert(name.clone(), (SetEntryState::Owned, *mutable, true));
            } else {
                result.insert(name.clone(), (entry_state.clone(), *mutable, false));
            }
        }

        result
    }

    /// Get entries that changed state from the previous line.
    pub fn get_changes(&self, line_num: u32) -> Vec<StateChange> {
        let current = match self.get(line_num) {
            Some(s) => s,
            None => return Vec::new(),
        };

        // Find the previous line with state in the same scope
        let prev = self.find_prev_in_scope(line_num);

        let mut changes = Vec::new();

        for (name, (state, mutable)) in &current.states {
            if self.shown_dropped.contains(name) && matches!(state, SetEntryState::Dropped) {
                continue;
            }

            let prev_state = prev.and_then(|p| p.states.get(name));

            let change_type = match prev_state {
                None => ChangeType::New,
                Some((old_state, _)) if old_state != state => ChangeType::StateChanged {
                    from: old_state.clone(),
                },
                _ => continue, // No change
            };

            changes.push(StateChange {
                name: name.clone(),
                state: state.clone(),
                mutable: *mutable,
                change_type,
            });
        }

        changes
    }

    /// Find the previous line that has state in the current scope.
    fn find_prev_in_scope(&self, line_num: u32) -> Option<&LineState> {
        if line_num == 0 {
            return None;
        }

        // Check if we crossed a scope boundary
        let current_scope_start = self
            .scope_starts
            .iter()
            .filter(|&&start| start <= line_num)
            .max()
            .copied()
            .unwrap_or(0);

        // Look for the most recent previous line in the same scope
        for prev_line in (current_scope_start..line_num).rev() {
            if let Some(state) = self.get(prev_line) {
                return Some(state);
            }
        }

        None
    }

    /// Get the scope start line for a given line.
    pub fn scope_start_for(&self, line_num: u32) -> u32 {
        self.scope_starts
            .iter()
            .filter(|&&start| start <= line_num)
            .max()
            .copied()
            .unwrap_or(0)
    }

    /// Rewind to a previous line (for re-processing).
    pub fn rewind_to(&mut self, line_num: u32) {
        // Remove all state after this line
        self.history.retain(|&line, _| line <= line_num);
        self.current_line = line_num;

        // Reset shown_dropped to scope boundary
        let scope_start = self.scope_start_for(line_num);

        // Collect dropped names first to avoid borrow conflict
        let dropped_names: Vec<String> = (scope_start..=line_num)
            .filter_map(|line| self.history.get(&line))
            .flat_map(|state| state.dropped_here.iter().cloned())
            .collect();

        self.shown_dropped.clear();
        for name in dropped_names {
            self.shown_dropped.insert(name);
        }
    }

    /// Get all lines in the timeline.
    pub fn lines(&self) -> Vec<u32> {
        let mut lines: Vec<u32> = self.history.keys().copied().collect();
        lines.sort();
        lines
    }

    /// Clear all state (for reuse).
    pub fn clear(&mut self) {
        self.history.clear();
        self.current_line = 0;
        self.shown_dropped.clear();
        self.scope_starts.clear();
    }
}

/// A detected state change for a variable.
#[derive(Debug, Clone)]
pub struct StateChange {
    /// Variable name.
    pub name: String,
    /// Current state.
    pub state: SetEntryState,
    /// Whether the variable is mutable.
    pub mutable: bool,
    /// Type of change.
    pub change_type: ChangeType,
}

/// Type of state change.
#[derive(Debug, Clone)]
pub enum ChangeType {
    /// New variable introduced.
    New,
    /// State changed from a previous state.
    StateChanged { from: SetEntryState },
}


#[cfg(test)]
mod tests {
    use super::*;

    fn make_entry(name: &str, state: SetEntryState) -> SetEntry {
        SetEntry {
            name: name.to_string(),
            mutable: false,
            state,
            borrows_from: None,
        }
    }

    #[test]
    fn test_record_and_get() {
        let mut timeline = StateTimeline::new();

        let x = make_entry("x", SetEntryState::Owned);
        let entries: Vec<&SetEntry> = vec![&x];
        timeline.record(1, &entries, "    let x = String::new();");

        let state = timeline.get(1).unwrap();
        assert!(state.live.contains("x"));
        assert_eq!(state.states.get("x").unwrap().0, SetEntryState::Owned);
    }

    #[test]
    fn test_detect_drops() {
        let mut timeline = StateTimeline::new();

        // Line 1: x is alive
        let x = make_entry("x", SetEntryState::Owned);
        timeline.record(1, &vec![&x], "    let x = String::new();");

        // Line 2: x is gone
        timeline.record(2, &vec![], "    next_statement();");

        let drops = timeline.get_pending_drops(2);
        assert_eq!(drops.len(), 1);
        assert_eq!(drops[0].name, "x");
    }

    #[test]
    fn test_no_drops_at_closing_brace() {
        let mut timeline = StateTimeline::new();

        let x = make_entry("x", SetEntryState::Owned);
        timeline.record(1, &vec![&x], "    let x = String::new();");
        timeline.record(2, &vec![], "}");

        let drops = timeline.get_pending_drops(2);
        assert!(drops.is_empty());
    }

    #[test]
    fn test_state_changes() {
        let mut timeline = StateTimeline::new();

        // Line 1: x is owned
        let x1 = make_entry("x", SetEntryState::Owned);
        timeline.record(1, &vec![&x1], "    let x = String::new();");

        // Line 2: x is shared
        let x2 = make_entry("x", SetEntryState::Shared);
        timeline.record(2, &vec![&x2], "    let r = &x;");

        let changes = timeline.get_changes(2);
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].name, "x");
        assert!(matches!(changes[0].change_type, ChangeType::StateChanged { .. }));
    }

    #[test]
    fn test_function_boundary_resets_scope() {
        let mut timeline = StateTimeline::new();

        // First function
        timeline.record(1, &vec![], "fn foo() {");
        let x = make_entry("x", SetEntryState::Owned);
        timeline.record(2, &vec![&x], "    let x = 1;");

        // Second function - x should not carry over
        timeline.record(5, &vec![], "fn bar() {");
        let y = make_entry("y", SetEntryState::Owned);
        timeline.record(6, &vec![&y], "    let y = 2;");

        // Changes at line 6 should only show y as new, not reference x
        let changes = timeline.get_changes(6);
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].name, "y");
    }

    #[test]
    fn test_time_travel() {
        let mut timeline = StateTimeline::new();

        let x1 = make_entry("x", SetEntryState::Owned);
        timeline.record(1, &vec![&x1], "    let x = String::new();");

        let x2 = make_entry("x", SetEntryState::Shared);
        let r = make_entry("r", SetEntryState::SharedBorrow);
        timeline.record(2, &vec![&x2, &r], "    let r = &x;");

        // Time travel back to line 1
        let state1 = timeline.get(1).unwrap();
        assert!(state1.live.contains("x"));
        assert!(!state1.live.contains("r"));

        // Check line 2
        let state2 = timeline.get(2).unwrap();
        assert!(state2.live.contains("x"));
        assert!(state2.live.contains("r"));
    }

    #[test]
    fn test_rewind() {
        let mut timeline = StateTimeline::new();

        let x = make_entry("x", SetEntryState::Owned);
        timeline.record(1, &vec![&x], "    let x = 1;");
        timeline.record(2, &vec![], "    drop(x);");
        timeline.record(3, &vec![], "    other();");

        // Rewind to line 1
        timeline.rewind_to(1);

        assert_eq!(timeline.current_line(), 1);
        assert!(timeline.get(2).is_none());
        assert!(timeline.get(3).is_none());
    }

    #[test]
    fn test_function_boundary() {
        assert!(StateTimeline::is_function_boundary("fn main() {"));
        assert!(StateTimeline::is_function_boundary("pub fn foo() {"));
        assert!(StateTimeline::is_function_boundary("    pub async fn bar() {"));
        assert!(!StateTimeline::is_function_boundary("let x = 1;"));
    }
}
