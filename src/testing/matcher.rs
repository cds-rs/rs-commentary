//! Matching logic for comparing actual ownership states with expectations.

use crate::analysis::SetEntryState;
use super::expectation::ExpectedState;

/// Result of matching an expectation against actual state.
#[derive(Debug, Clone)]
pub enum MatchResult {
    /// Expectation matched
    Match,
    /// State mismatch
    StateMismatch {
        expected: ExpectedState,
        actual: SetEntryState,
    },
    /// Variable expected to be live but wasn't found
    VariableNotFound {
        var: String,
    },
    /// Variable expected to NOT be live but was found
    UnexpectedVariable {
        var: String,
        actual: SetEntryState,
    },
}

impl MatchResult {
    /// Returns true if this is a match.
    pub fn is_match(&self) -> bool {
        matches!(self, MatchResult::Match)
    }
}

/// Check if an actual SetEntryState matches an ExpectedState.
pub fn states_match(expected: &ExpectedState, actual: &SetEntryState, mutable: bool) -> bool {
    match (expected, actual) {
        // Owned states - check mutability
        (ExpectedState::OwnedMut, SetEntryState::Owned) => mutable,
        (ExpectedState::Owned, SetEntryState::Owned) => !mutable,

        // Downgraded states
        (ExpectedState::Shared, SetEntryState::Shared { .. }) => true,
        (ExpectedState::Frozen, SetEntryState::Frozen { .. }) => true,

        // Reference states
        (ExpectedState::RefShared, SetEntryState::SharedBorrow) => true,
        (ExpectedState::RefMut, SetEntryState::MutBorrow) => true,

        // Terminal states
        (ExpectedState::Moved, SetEntryState::Moved { .. }) => true,
        (ExpectedState::Dropped, SetEntryState::Dropped) => true,

        // No match
        _ => false,
    }
}

/// Check if a state represents "not live" (moved or dropped).
///
/// Used for `!x` assertions: a variable that is moved or dropped
/// is not usable and counts as "not live".
pub fn is_not_live(state: &SetEntryState) -> bool {
    matches!(state, SetEntryState::Moved { .. } | SetEntryState::Dropped)
}

/// Convert a SetEntryState to its canonical expectation name for error messages.
pub fn state_to_name(state: &SetEntryState, mutable: bool) -> &'static str {
    match state {
        SetEntryState::Owned => {
            if mutable {
                "owned_mut"
            } else {
                "owned"
            }
        }
        SetEntryState::Shared { .. } => "shared",
        SetEntryState::Frozen { .. } => "frozen",
        SetEntryState::SharedBorrow => "ref_shared",
        SetEntryState::MutBorrow => "ref_mut",
        SetEntryState::Moved { .. } => "moved",
        SetEntryState::Dropped => "dropped",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_owned_mut_match() {
        assert!(states_match(&ExpectedState::OwnedMut, &SetEntryState::Owned, true));
        assert!(!states_match(&ExpectedState::OwnedMut, &SetEntryState::Owned, false));
    }

    #[test]
    fn test_owned_immut_match() {
        assert!(states_match(&ExpectedState::Owned, &SetEntryState::Owned, false));
        assert!(!states_match(&ExpectedState::Owned, &SetEntryState::Owned, true));
    }

    #[test]
    fn test_shared_match() {
        assert!(states_match(
            &ExpectedState::Shared,
            &SetEntryState::Shared {
                borrowed_by: vec!["r".to_string()]
            },
            true
        ));
    }

    #[test]
    fn test_frozen_match() {
        assert!(states_match(
            &ExpectedState::Frozen,
            &SetEntryState::Frozen {
                borrowed_by: "r".to_string()
            },
            true
        ));
    }

    #[test]
    fn test_ref_shared_match() {
        assert!(states_match(&ExpectedState::RefShared, &SetEntryState::SharedBorrow, false));
    }

    #[test]
    fn test_ref_mut_match() {
        assert!(states_match(&ExpectedState::RefMut, &SetEntryState::MutBorrow, false));
    }

    #[test]
    fn test_moved_match() {
        assert!(states_match(
            &ExpectedState::Moved,
            &SetEntryState::Moved { to: Some("y".to_string()) },
            false
        ));
    }

    #[test]
    fn test_dropped_match() {
        assert!(states_match(&ExpectedState::Dropped, &SetEntryState::Dropped, false));
    }

    #[test]
    fn test_state_mismatch() {
        assert!(!states_match(&ExpectedState::OwnedMut, &SetEntryState::SharedBorrow, true));
        assert!(!states_match(&ExpectedState::Shared, &SetEntryState::Owned, true));
    }
}
