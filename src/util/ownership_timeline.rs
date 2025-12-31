//! Ownership timeline - explicit state transitions at line boundaries.
//!
//! This module provides an intermediate data structure that makes ownership
//! state transitions first-class citizens, enabling easier testing and
//! clearer reasoning about state changes.
//!
//! ## Model
//!
//! Each line has two boundaries:
//! - `Pre`: State BEFORE the line executes
//! - `Post`: State AFTER the line executes
//!
//! State changes are captured as:
//! - `new_vars`: Variables introduced at this boundary
//! - `delta_vars`: Existing variables whose state changed (with reason)
//! - `dropped_vars`: Variables dropped at this boundary

use std::collections::HashMap;
use crate::analysis::{SetAnnotation, SetEntryState};

/// Pre or Post a line boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Boundary {
    /// State BEFORE the line executes.
    Pre,
    /// State AFTER the line executes.
    Post,
}

/// Why a state transition occurred.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TransitionReason {
    /// A shared borrow was taken: Owned -> Shared
    SharedBorrowTaken { borrow_name: String },
    /// A mutable borrow was taken: Owned -> Frozen
    MutBorrowTaken { borrow_name: String },
    /// A borrow ended (NLL): Shared/Frozen -> Owned
    BorrowEnded { borrow_name: String },
    /// Value was moved
    Moved,
    /// Explicit drop() call
    ExplicitDrop,
    /// Scope exit (automatic drop)
    ScopeExit,
    /// State changed for other/unknown reason
    Other,
}

/// A state transition with from/to states and reason.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StateTransition {
    pub from: SetEntryState,
    pub to: SetEntryState,
    pub reason: TransitionReason,
}

/// Variable snapshot at a boundary.
#[derive(Debug, Clone, PartialEq)]
pub struct VarSnapshot {
    pub name: String,
    pub state: SetEntryState,
    pub mutable: bool,
    pub intro_line: u32,
    pub borrows_from: Option<String>,
}

/// State at a specific line boundary.
#[derive(Debug, Clone)]
pub struct BoundaryState {
    /// The line number this state applies to.
    pub line: u32,
    /// Pre or Post this line.
    pub boundary: Boundary,
    /// Function scope name.
    pub scope: String,

    /// Variables newly introduced at this boundary.
    pub new_vars: Vec<VarSnapshot>,

    /// Existing variables whose state changed at this boundary.
    pub delta_vars: HashMap<String, StateTransition>,

    /// Variables dropped at this boundary.
    pub dropped_vars: Vec<VarSnapshot>,

    /// Complete variable state at this boundary (for rendering).
    pub variables: Vec<VarSnapshot>,
}

impl BoundaryState {
    /// Check if a variable was introduced at this boundary.
    pub fn is_new(&self, name: &str) -> bool {
        self.new_vars.iter().any(|v| v.name == name)
    }

    /// Check if a variable was dropped at this boundary.
    pub fn is_dropped(&self, name: &str) -> bool {
        self.dropped_vars.iter().any(|v| v.name == name)
    }

    /// Check if a variable's state changed at this boundary.
    pub fn has_transition(&self, name: &str) -> bool {
        self.delta_vars.contains_key(name)
    }

    /// Get a variable's snapshot by name.
    pub fn get_var(&self, name: &str) -> Option<&VarSnapshot> {
        self.variables.iter().find(|v| v.name == name)
    }
}

/// Per-function timeline.
#[derive(Debug, Clone)]
pub struct FunctionTimeline {
    /// Function name (scope).
    pub name: String,
    /// Start line of the function.
    pub start_line: u32,
    /// End line of the function (closing brace).
    pub end_line: u32,
    /// Ordered list of boundary states.
    pub states: Vec<BoundaryState>,
}

impl FunctionTimeline {
    /// Get the state at a specific boundary.
    pub fn at(&self, line: u32, boundary: Boundary) -> Option<&BoundaryState> {
        self.states.iter().find(|s| s.line == line && s.boundary == boundary)
    }

    /// Get transitions that happened on a specific line (Pre -> Post delta).
    pub fn transitions_on(&self, line: u32) -> Option<&HashMap<String, StateTransition>> {
        self.at(line, Boundary::Post).map(|s| &s.delta_vars)
    }

    /// Get variables introduced on a line.
    pub fn introduced_on(&self, line: u32) -> Vec<&VarSnapshot> {
        self.at(line, Boundary::Post)
            .map(|s| s.new_vars.iter().collect())
            .unwrap_or_default()
    }

    /// Get variables dropped on a line.
    pub fn dropped_on(&self, line: u32) -> Vec<&VarSnapshot> {
        self.at(line, Boundary::Post)
            .map(|s| s.dropped_vars.iter().collect())
            .unwrap_or_default()
    }

    /// Iterate through boundaries in order.
    pub fn iter(&self) -> impl Iterator<Item = &BoundaryState> {
        self.states.iter()
    }

    /// Get all lines in this function.
    pub fn lines(&self) -> impl Iterator<Item = u32> {
        self.start_line..=self.end_line
    }
}

/// File-level ownership timeline.
#[derive(Debug, Clone, Default)]
pub struct OwnershipTimeline {
    /// Per-function timelines.
    pub functions: Vec<FunctionTimeline>,
}

impl OwnershipTimeline {
    /// Create an empty timeline.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the function timeline containing a line.
    pub fn function_at(&self, line: u32) -> Option<&FunctionTimeline> {
        self.functions.iter().find(|f| line >= f.start_line && line <= f.end_line)
    }

    /// Get state at a specific line/boundary.
    pub fn at(&self, line: u32, boundary: Boundary) -> Option<&BoundaryState> {
        self.function_at(line).and_then(|f| f.at(line, boundary))
    }

    /// Iterate through all functions.
    pub fn iter_functions(&self) -> impl Iterator<Item = &FunctionTimeline> {
        self.functions.iter()
    }
}

// ============================================================================
// Builder
// ============================================================================

/// Builder for constructing OwnershipTimeline from analysis output.
pub struct OwnershipTimelineBuilder {
    functions: Vec<FunctionTimeline>,
    current_function: Option<FunctionTimelineBuilder>,
}

struct FunctionTimelineBuilder {
    name: String,
    start_line: u32,
    states: Vec<BoundaryState>,
    /// Current variable state (carried forward between lines).
    current_vars: HashMap<String, VarSnapshot>,
}

impl OwnershipTimelineBuilder {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            current_function: None,
        }
    }

    /// Start a new function scope.
    pub fn enter_function(&mut self, name: &str, line: u32) {
        // Finalize previous function if any
        if let Some(builder) = self.current_function.take() {
            self.functions.push(builder.build());
        }

        self.current_function = Some(FunctionTimelineBuilder {
            name: name.to_string(),
            start_line: line,
            states: Vec::new(),
            current_vars: HashMap::new(),
        });
    }

    /// Record state at a boundary.
    pub fn record_boundary(
        &mut self,
        line: u32,
        boundary: Boundary,
        scope: &str,
        new_vars: Vec<VarSnapshot>,
        delta_vars: HashMap<String, StateTransition>,
        dropped_vars: Vec<VarSnapshot>,
    ) {
        let Some(ref mut fb) = self.current_function else {
            return;
        };

        // Apply changes to current_vars
        for var in &new_vars {
            fb.current_vars.insert(var.name.clone(), var.clone());
        }

        for (name, transition) in &delta_vars {
            if let Some(var) = fb.current_vars.get_mut(name) {
                var.state = transition.to.clone();
            }
        }

        for var in &dropped_vars {
            fb.current_vars.remove(&var.name);
        }

        // Build complete variable list
        let variables: Vec<VarSnapshot> = fb.current_vars.values().cloned().collect();

        fb.states.push(BoundaryState {
            line,
            boundary,
            scope: scope.to_string(),
            new_vars,
            delta_vars,
            dropped_vars,
            variables,
        });
    }

    /// Build the final timeline.
    pub fn build(mut self) -> OwnershipTimeline {
        if let Some(builder) = self.current_function.take() {
            self.functions.push(builder.build());
        }
        OwnershipTimeline { functions: self.functions }
    }
}

impl Default for OwnershipTimelineBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl FunctionTimelineBuilder {
    fn build(self) -> FunctionTimeline {
        let end_line = self.states.last().map(|s| s.line).unwrap_or(self.start_line);
        FunctionTimeline {
            name: self.name,
            start_line: self.start_line,
            end_line,
            states: self.states,
        }
    }
}

// ============================================================================
// Construction from SetAnnotation
// ============================================================================

impl OwnershipTimeline {
    /// Build timeline from ownership analysis annotations.
    pub fn from_annotations(
        annotations: &[SetAnnotation],
        semantic_drops: &HashMap<(String, u32), u32>,
        source: &str,
    ) -> Self {
        let mut builder = OwnershipTimelineBuilder::new();
        let lines: Vec<&str> = source.lines().collect();

        // Track previous state for diff computation
        let mut prev_vars: HashMap<String, VarSnapshot> = HashMap::new();
        let mut current_scope = String::new();

        for ann in annotations {
            let line = ann.line;
            let line_text = lines.get(line as usize).map(|s| *s).unwrap_or("");

            // Detect function entry
            if is_function_start(line_text) {
                let fn_name = extract_fn_name(line_text);
                builder.enter_function(&fn_name, line);
                current_scope = fn_name;
                prev_vars.clear();
            }

            // Convert entries to VarSnapshots
            let current_vars: HashMap<String, VarSnapshot> = ann.set.entries.iter()
                .map(|e| {
                    let snap = VarSnapshot {
                        name: e.name.clone(),
                        state: e.state.clone(),
                        mutable: e.mutable,
                        intro_line: find_intro_line(&e.name, &prev_vars, line),
                        borrows_from: e.borrows_from.clone(),
                    };
                    (e.name.clone(), snap)
                })
                .collect();

            // Compute diffs
            let (new_vars, delta_vars, dropped_vars) = compute_diffs(
                &prev_vars,
                &current_vars,
                semantic_drops,
                line,
            );

            // Record Pre boundary (state before this line = previous Post)
            builder.record_boundary(
                line,
                Boundary::Pre,
                &current_scope,
                Vec::new(),
                HashMap::new(),
                Vec::new(),
            );

            // Record Post boundary with changes
            builder.record_boundary(
                line,
                Boundary::Post,
                &current_scope,
                new_vars,
                delta_vars,
                dropped_vars,
            );

            prev_vars = current_vars;
        }

        builder.build()
    }
}

/// Check if a line is a function definition start.
fn is_function_start(line: &str) -> bool {
    let trimmed = line.trim();
    (trimmed.starts_with("fn ") || trimmed.starts_with("pub fn ") ||
     trimmed.starts_with("async fn ") || trimmed.starts_with("pub async fn "))
        && trimmed.contains('(')
}

/// Extract function name from a function definition line.
fn extract_fn_name(line: &str) -> String {
    let trimmed = line.trim();
    // Skip "pub ", "async ", etc.
    let after_fn = trimmed
        .trim_start_matches("pub ")
        .trim_start_matches("async ")
        .trim_start_matches("fn ");

    // Extract name up to '(' or '<'
    after_fn
        .split(|c| c == '(' || c == '<')
        .next()
        .unwrap_or("unknown")
        .trim()
        .to_string()
}

/// Find the intro line for a variable (first occurrence or current line).
fn find_intro_line(name: &str, prev_vars: &HashMap<String, VarSnapshot>, current_line: u32) -> u32 {
    prev_vars.get(name).map(|v| v.intro_line).unwrap_or(current_line)
}

/// Compute diffs between previous and current variable states.
fn compute_diffs(
    prev_vars: &HashMap<String, VarSnapshot>,
    current_vars: &HashMap<String, VarSnapshot>,
    semantic_drops: &HashMap<(String, u32), u32>,
    line: u32,
) -> (Vec<VarSnapshot>, HashMap<String, StateTransition>, Vec<VarSnapshot>) {
    let mut new_vars = Vec::new();
    let mut delta_vars = HashMap::new();
    let mut dropped_vars = Vec::new();

    // Find new variables (in current but not in prev)
    for (name, snap) in current_vars {
        if !prev_vars.contains_key(name) {
            new_vars.push(snap.clone());
        }
    }

    // Find state changes (in both, but different state)
    for (name, curr_snap) in current_vars {
        if let Some(prev_snap) = prev_vars.get(name) {
            if prev_snap.state != curr_snap.state {
                let reason = infer_transition_reason(prev_snap, curr_snap, current_vars);
                delta_vars.insert(name.clone(), StateTransition {
                    from: prev_snap.state.clone(),
                    to: curr_snap.state.clone(),
                    reason,
                });
            }
        }
    }

    // Find dropped variables (semantic drops at this line, or in prev but not in current)
    for (name, prev_snap) in prev_vars {
        let key = (name.clone(), prev_snap.intro_line);
        let is_semantic_drop = semantic_drops.get(&key).map(|&drop_line| drop_line == line).unwrap_or(false);
        let not_in_current = !current_vars.contains_key(name);

        if is_semantic_drop || not_in_current {
            dropped_vars.push(prev_snap.clone());
        }
    }

    (new_vars, delta_vars, dropped_vars)
}

/// Infer why a state transition occurred.
fn infer_transition_reason(
    prev: &VarSnapshot,
    curr: &VarSnapshot,
    all_vars: &HashMap<String, VarSnapshot>,
) -> TransitionReason {
    use SetEntryState::*;

    match (&prev.state, &curr.state) {
        // Owned/Shared -> Shared: borrow taken
        (Owned, Shared) | (Shared, Shared) => {
            // Find the new borrow that references this variable
            if let Some(borrow_name) = find_new_borrow_of(&prev.name, all_vars) {
                TransitionReason::SharedBorrowTaken { borrow_name }
            } else {
                TransitionReason::Other
            }
        }
        // Owned -> Frozen: mutable borrow taken
        (Owned, Frozen) => {
            if let Some(borrow_name) = find_new_borrow_of(&prev.name, all_vars) {
                TransitionReason::MutBorrowTaken { borrow_name }
            } else {
                TransitionReason::Other
            }
        }
        // Shared/Frozen -> Owned: borrow ended
        (Shared, Owned) | (Frozen, Owned) => {
            TransitionReason::BorrowEnded { borrow_name: String::new() }
        }
        // Anything -> Dropped
        (_, Dropped) => TransitionReason::ExplicitDrop,
        _ => TransitionReason::Other,
    }
}

/// Find a variable that borrows from the given source.
fn find_new_borrow_of(source: &str, all_vars: &HashMap<String, VarSnapshot>) -> Option<String> {
    for (name, snap) in all_vars {
        if snap.borrows_from.as_deref() == Some(source) {
            return Some(name.clone());
        }
    }
    None
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::{OwnershipSet, SetEntry};

    fn make_annotation(line: u32, scope: &str, entries: Vec<(&str, SetEntryState, Option<&str>)>) -> SetAnnotation {
        SetAnnotation {
            line,
            set: OwnershipSet {
                scope_name: scope.to_string(),
                line,
                entries: entries.into_iter().map(|(name, state, borrows)| SetEntry {
                    name: name.to_string(),
                    state,
                    mutable: false,
                    borrows_from: borrows.map(|s| s.to_string()),
                }).collect(),
            },
        }
    }

    #[test]
    fn test_new_var_detection() {
        let annotations = vec![
            make_annotation(0, "main", vec![]),
            make_annotation(1, "main", vec![("x", SetEntryState::Owned, None)]),
        ];

        let timeline = OwnershipTimeline::from_annotations(&annotations, &HashMap::new(), "fn main() {\nlet x = 1;\n}");

        let post = timeline.at(1, Boundary::Post).unwrap();
        assert_eq!(post.new_vars.len(), 1);
        assert_eq!(post.new_vars[0].name, "x");
        assert_eq!(post.new_vars[0].state, SetEntryState::Owned);
    }

    #[test]
    fn test_borrow_transition() {
        let annotations = vec![
            make_annotation(0, "main", vec![]),
            make_annotation(1, "main", vec![("x", SetEntryState::Owned, None)]),
            make_annotation(2, "main", vec![
                ("x", SetEntryState::Shared, None),
                ("r", SetEntryState::SharedBorrow, Some("x")),
            ]),
        ];

        let timeline = OwnershipTimeline::from_annotations(&annotations, &HashMap::new(), "fn main() {\nlet x = 1;\nlet r = &x;\n}");

        let post = timeline.at(2, Boundary::Post).unwrap();

        // r should be new
        assert!(post.is_new("r"));

        // x should have transitioned Owned -> Shared
        assert!(post.has_transition("x"));
        let transition = &post.delta_vars["x"];
        assert_eq!(transition.from, SetEntryState::Owned);
        assert_eq!(transition.to, SetEntryState::Shared);
        assert!(matches!(transition.reason, TransitionReason::SharedBorrowTaken { .. }));
    }

    #[test]
    fn test_semantic_drop() {
        let annotations = vec![
            make_annotation(0, "main", vec![]),
            make_annotation(1, "main", vec![("x", SetEntryState::Owned, None)]),
            make_annotation(2, "main", vec![("x", SetEntryState::Owned, None)]),
        ];

        let mut semantic_drops = HashMap::new();
        semantic_drops.insert(("x".to_string(), 1), 2); // x drops at line 2

        let timeline = OwnershipTimeline::from_annotations(&annotations, &semantic_drops, "fn main() {\nlet x = 1;\nuse(x);\n}");

        let post = timeline.at(2, Boundary::Post).unwrap();
        assert!(post.is_dropped("x"));
    }

    #[test]
    fn test_function_timeline_queries() {
        let annotations = vec![
            make_annotation(0, "main", vec![]),
            make_annotation(1, "main", vec![("x", SetEntryState::Owned, None)]),
        ];

        let timeline = OwnershipTimeline::from_annotations(&annotations, &HashMap::new(), "fn main() {\nlet x = 1;\n}");

        let func = timeline.function_at(1).unwrap();
        assert_eq!(func.name, "main");

        let introduced = func.introduced_on(1);
        assert_eq!(introduced.len(), 1);
        assert_eq!(introduced[0].name, "x");
    }
}
