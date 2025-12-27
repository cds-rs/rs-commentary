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
}

/// Information about a variable that was dropped.
#[derive(Debug, Clone)]
pub struct VariableDrop {
    /// Name of the dropped variable.
    pub name: String,
    /// Line where it was last seen.
    pub last_seen_line: u32,
}

/// A time-traveling state machine for ownership tracking.
///
/// Records state at each line and allows querying any point in history.
/// Supports rewinding to previous states for comparison or re-rendering.
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
}

impl StateTimeline {
    /// Create a new empty timeline.
    pub fn new() -> Self {
        Self::default()
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

    /// Record state for a line, computing transitions from previous line.
    pub fn record(&mut self, line_num: u32, entries: &[&SetEntry], line_text: &str) {
        // Check for scope boundary
        if Self::is_function_boundary(line_text) {
            self.scope_starts.push(line_num);
            self.shown_dropped.clear();
        }

        let prev_state = self.get(self.current_line);

        // Compute current live set
        let live: HashSet<String> = entries
            .iter()
            .filter(|e| !matches!(e.state, SetEntryState::Dropped))
            .map(|e| e.name.clone())
            .collect();

        // Compute states
        let states: HashMap<String, (SetEntryState, bool)> = entries
            .iter()
            .map(|e| (e.name.clone(), (e.state.clone(), e.mutable)))
            .collect();

        // Detect drops (variables in prev but not in current)
        let dropped_here: Vec<String> = if let Some(prev) = prev_state {
            if !line_text.trim().starts_with('}') {
                prev.live
                    .iter()
                    .filter(|v| !live.contains(*v))
                    .filter(|v| prev.states.contains_key(*v)) // Only annotated vars
                    .cloned()
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        self.history.insert(
            line_num,
            LineState {
                live,
                states,
                dropped_here,
            },
        );
        self.current_line = line_num;
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
    pub fn get_pending_drops(&self, line_num: u32) -> Vec<VariableDrop> {
        self.get(line_num)
            .map(|state| {
                state
                    .dropped_here
                    .iter()
                    .filter(|name| !self.shown_dropped.contains(*name))
                    .map(|name| VariableDrop {
                        name: name.clone(),
                        last_seen_line: line_num.saturating_sub(1),
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
