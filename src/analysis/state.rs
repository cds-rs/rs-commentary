//! Core data types for ownership state tracking.
//!
//! This module implements an explicit state machine for tracking Rust ownership.
//! State transitions are centralized in [`BindingState::transition`] to ensure
//! consistency and enable validation of legal transitions.

use ra_ap_syntax::TextRange;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Unique identifier for a binding (variable) in the analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BindingId(pub u32);

// ============================================================================
// State Machine: Events and Transitions
// ============================================================================

/// Events that trigger ownership state transitions.
///
/// These represent the semantic actions in Rust code that affect ownership.
#[derive(Debug, Clone)]
pub enum OwnershipEvent {
    /// A shared borrow (&T) is taken from this binding
    SharedBorrow { by: BindingId },

    /// A mutable borrow (&mut T) is taken from this binding
    MutBorrow { by: BindingId },

    /// A borrow of this binding has ended (NLL)
    BorrowEnd { borrow_id: BindingId },

    /// All borrows have ended, restore to owned
    AllBorrowsEnd,

    /// The value is moved to another binding
    Move { to: Option<BindingId> },

    /// The value is copied (Copy types only)
    Copy,

    /// The scope containing this binding has ended
    ScopeExit,

    /// The binding is reborrowed (e.g., &mut passed to function)
    Reborrow { to: BindingId },
}

/// Error when an invalid state transition is attempted.
#[derive(Debug, Error)]
pub enum TransitionError {
    #[error("Invalid transition from {from:?} via {event:?}")]
    Invalid {
        from: BindingState,
        event: OwnershipEvent,
    },

    #[error("Cannot borrow: binding is already {state:?}")]
    CannotBorrow { state: BindingState },

    #[error("Cannot move: binding is {state:?}")]
    CannotMove { state: BindingState },
}

/// Unique identifier for a scope (block, function, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ScopeId(pub u32);

/// Ownership capabilities: O=ownership, R=read, W=write.
///
/// These represent what operations are valid on a binding:
/// - O (owned): Can transfer ownership (move or drop)
/// - R (read): Can read the value
/// - W (write): Can mutate the value
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Capabilities {
    /// Can transfer ownership (move the value out)
    pub owned: bool,
    /// Can read the value
    pub read: bool,
    /// Can mutate the value
    pub write: bool,
}

impl Capabilities {
    /// Full ownership: O R W
    pub const OWNED: Self = Self {
        owned: true,
        read: true,
        write: true,
    };

    /// Immutable ownership: O R (no W because not declared mut)
    pub const OWNED_IMMUT: Self = Self {
        owned: true,
        read: true,
        write: false,
    };

    /// Shared borrow: R only
    pub const SHARED_BORROW: Self = Self {
        owned: false,
        read: true,
        write: false,
    };

    /// Mutable borrow: R W (not O)
    pub const MUT_BORROW: Self = Self {
        owned: false,
        read: true,
        write: true,
    };

    /// No access (moved or dropped)
    pub const NONE: Self = Self {
        owned: false,
        read: false,
        write: false,
    };
}

impl std::fmt::Display for Capabilities {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut caps = String::new();
        if self.owned {
            caps.push('O');
        }
        if self.read {
            caps.push('R');
        }
        if self.write {
            caps.push('W');
        }
        if caps.is_empty() {
            write!(f, "∅")
        } else {
            write!(f, "{}", caps)
        }
    }
}

/// Current state of a binding.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BindingState {
    /// Binding owns the value (O R W or O R depending on mutability)
    Owned { mutable: bool },

    /// Binding holds a shared reference (&T)
    SharedBorrow { from: BindingId },

    /// Binding holds a mutable reference (&mut T)
    MutBorrow { from: BindingId },

    /// Value has been moved to another binding
    Moved { to: Option<BindingId> },

    /// Some fields have been moved out (partial move)
    PartiallyMoved { moved_fields: Vec<String> },

    /// Value has been dropped (end of scope)
    Dropped,

    /// Binding is temporarily suspended due to reborrowing
    /// (e.g., &mut passed to a function call)
    Suspended { reborrowed_to: BindingId },

    /// Binding was copied (implements Copy trait)
    Copied,

    // === Temporary downgrade states (from lecture notation) ===

    /// Original binding is temporarily "shared" (shr) due to active &T borrows.
    /// The binding still owns the value but is downgraded to read-only.
    /// Capabilities: O R (loses W even if originally mut)
    Shared {
        original_mutable: bool,
        borrowed_by: Vec<BindingId>,
    },

    /// Original binding is temporarily "frozen" (frz) due to active &mut T borrow.
    /// The binding still owns the value but has no access until borrow ends.
    /// Capabilities: O only (can't read or write while frozen)
    Frozen {
        original_mutable: bool,
        borrowed_by: BindingId,
    },
}

/// Capabilities when a binding is frozen (still owns, but can't access)
impl Capabilities {
    pub const FROZEN: Self = Self {
        owned: true,  // still owns the value
        read: false,  // can't read while frozen
        write: false, // can't write while frozen
    };
}

impl BindingState {
    /// Get the capabilities for this state.
    pub fn capabilities(&self) -> Capabilities {
        match self {
            BindingState::Owned { mutable: true } => Capabilities::OWNED,
            BindingState::Owned { mutable: false } => Capabilities::OWNED_IMMUT,
            BindingState::SharedBorrow { .. } => Capabilities::SHARED_BORROW,
            BindingState::MutBorrow { .. } => Capabilities::MUT_BORROW,
            BindingState::Moved { .. } => Capabilities::NONE,
            BindingState::PartiallyMoved { .. } => Capabilities::NONE,
            BindingState::Dropped => Capabilities::NONE,
            BindingState::Suspended { .. } => Capabilities::NONE,
            BindingState::Copied => Capabilities::OWNED_IMMUT, // After copy, original unchanged
            // Temporary downgrade states
            BindingState::Shared { .. } => Capabilities::OWNED_IMMUT, // O R (loses W)
            BindingState::Frozen { .. } => Capabilities::FROZEN,      // O only (can't R or W)
        }
    }

    /// Apply a state transition event, returning the new state.
    ///
    /// This is the core of the ownership state machine. All state changes
    /// should go through this method to ensure valid transitions.
    ///
    /// # State Transition Table
    ///
    /// ```text
    /// From State    | Event           | To State
    /// --------------|-----------------|------------------
    /// Owned         | SharedBorrow    | Shared
    /// Owned         | MutBorrow       | Frozen
    /// Owned         | Move            | Moved
    /// Owned         | Copy            | Owned (unchanged)
    /// Owned         | ScopeExit       | Dropped
    /// Shared        | SharedBorrow    | Shared (add borrow)
    /// Shared        | BorrowEnd       | Shared or Owned
    /// Shared        | AllBorrowsEnd   | Owned
    /// Shared        | ScopeExit       | Dropped
    /// Frozen        | BorrowEnd       | Owned
    /// Frozen        | AllBorrowsEnd   | Owned
    /// Frozen        | ScopeExit       | Dropped
    /// SharedBorrow  | ScopeExit       | Dropped
    /// MutBorrow     | ScopeExit       | Dropped
    /// MutBorrow     | Reborrow        | Suspended
    /// ```
    pub fn transition(&self, event: OwnershipEvent) -> Result<BindingState, TransitionError> {
        match (self, &event) {
            // ================================================================
            // Owned state transitions
            // ================================================================
            (BindingState::Owned { mutable }, OwnershipEvent::SharedBorrow { by }) => {
                Ok(BindingState::Shared {
                    original_mutable: *mutable,
                    borrowed_by: vec![*by],
                })
            }

            (BindingState::Owned { mutable }, OwnershipEvent::MutBorrow { by }) => {
                Ok(BindingState::Frozen {
                    original_mutable: *mutable,
                    borrowed_by: *by,
                })
            }

            (BindingState::Owned { .. }, OwnershipEvent::Move { to }) => {
                Ok(BindingState::Moved { to: *to })
            }

            (BindingState::Owned { .. }, OwnershipEvent::Copy) => {
                // Copy doesn't change the source binding's state - it remains Owned
                // The copy annotation is tracked separately as a synthetic event
                Ok(self.clone())
            }

            (BindingState::Owned { .. }, OwnershipEvent::ScopeExit) => {
                Ok(BindingState::Dropped)
            }

            // ================================================================
            // Shared state transitions (borrowed by &T)
            // ================================================================
            (
                BindingState::Shared { original_mutable, borrowed_by },
                OwnershipEvent::SharedBorrow { by },
            ) => {
                // Can add more shared borrows
                let mut new_borrows = borrowed_by.clone();
                new_borrows.push(*by);
                Ok(BindingState::Shared {
                    original_mutable: *original_mutable,
                    borrowed_by: new_borrows,
                })
            }

            (
                BindingState::Shared { original_mutable, borrowed_by },
                OwnershipEvent::BorrowEnd { borrow_id },
            ) => {
                let new_borrows: Vec<_> = borrowed_by
                    .iter()
                    .filter(|&id| id != borrow_id)
                    .copied()
                    .collect();

                if new_borrows.is_empty() {
                    // All borrows ended, restore to owned
                    Ok(BindingState::Owned {
                        mutable: *original_mutable,
                    })
                } else {
                    Ok(BindingState::Shared {
                        original_mutable: *original_mutable,
                        borrowed_by: new_borrows,
                    })
                }
            }

            (BindingState::Shared { original_mutable, .. }, OwnershipEvent::AllBorrowsEnd) => {
                Ok(BindingState::Owned {
                    mutable: *original_mutable,
                })
            }

            (BindingState::Shared { .. }, OwnershipEvent::ScopeExit) => {
                Ok(BindingState::Dropped)
            }

            // ================================================================
            // Frozen state transitions (borrowed by &mut T)
            // ================================================================
            (BindingState::Frozen { original_mutable, .. }, OwnershipEvent::BorrowEnd { .. }) |
            (BindingState::Frozen { original_mutable, .. }, OwnershipEvent::AllBorrowsEnd) => {
                Ok(BindingState::Owned {
                    mutable: *original_mutable,
                })
            }

            (BindingState::Frozen { .. }, OwnershipEvent::ScopeExit) => {
                Ok(BindingState::Dropped)
            }

            // ================================================================
            // Borrow binding transitions
            // ================================================================
            (BindingState::SharedBorrow { .. }, OwnershipEvent::ScopeExit) => {
                Ok(BindingState::Dropped)
            }

            (BindingState::MutBorrow { .. }, OwnershipEvent::ScopeExit) => {
                Ok(BindingState::Dropped)
            }

            (BindingState::MutBorrow { from: _ }, OwnershipEvent::Reborrow { to }) => {
                Ok(BindingState::Suspended {
                    reborrowed_to: *to,
                })
            }

            // ================================================================
            // Suspended state transitions
            // ================================================================
            (BindingState::Suspended { .. }, OwnershipEvent::ScopeExit) => {
                Ok(BindingState::Dropped)
            }

            // ================================================================
            // Terminal states (Moved, Dropped) - no valid transitions
            // ================================================================
            (BindingState::Moved { .. }, OwnershipEvent::ScopeExit) => {
                // Already moved, scope exit is a no-op (or becomes Dropped)
                Ok(BindingState::Dropped)
            }

            (BindingState::Dropped, _) => {
                // Already dropped, ignore further events
                Ok(BindingState::Dropped)
            }

            // ================================================================
            // Invalid transitions
            // ================================================================
            _ => Err(TransitionError::Invalid {
                from: self.clone(),
                event,
            }),
        }
    }

    /// Check if this state allows taking a shared borrow.
    pub fn can_shared_borrow(&self) -> bool {
        matches!(
            self,
            BindingState::Owned { .. } | BindingState::Shared { .. }
        )
    }

    /// Check if this state allows taking a mutable borrow.
    pub fn can_mut_borrow(&self) -> bool {
        matches!(self, BindingState::Owned { mutable: true })
    }

    /// Check if this state allows moving the value.
    pub fn can_move(&self) -> bool {
        matches!(self, BindingState::Owned { .. })
    }

    /// Check if this binding is currently borrowed (Shared or Frozen).
    pub fn is_borrowed(&self) -> bool {
        matches!(
            self,
            BindingState::Shared { .. } | BindingState::Frozen { .. }
        )
    }

    /// Get the original mutability if this is a borrowed state.
    pub fn original_mutable(&self) -> Option<bool> {
        match self {
            BindingState::Shared { original_mutable, .. } => Some(*original_mutable),
            BindingState::Frozen { original_mutable, .. } => Some(*original_mutable),
            _ => None,
        }
    }
}

impl std::fmt::Display for BindingState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BindingState::Owned { mutable: true } => write!(f, "owned (mut)"),
            BindingState::Owned { mutable: false } => write!(f, "owned"),
            BindingState::SharedBorrow { .. } => write!(f, "&borrow"),
            BindingState::MutBorrow { .. } => write!(f, "&mut borrow"),
            BindingState::Moved { .. } => write!(f, "moved"),
            BindingState::PartiallyMoved { moved_fields } => {
                write!(f, "partially moved ({})", moved_fields.join(", "))
            }
            BindingState::Dropped => write!(f, "dropped"),
            BindingState::Suspended { .. } => write!(f, "suspended (reborrowed)"),
            BindingState::Copied => write!(f, "copied"),
            BindingState::Shared { .. } => write!(f, "shared (shr)"),
            BindingState::Frozen { .. } => write!(f, "frozen (frz)"),
        }
    }
}

impl BindingState {
    /// Get a detailed explanation of this state for hover display.
    pub fn hover_explanation(&self) -> String {
        match self {
            BindingState::Owned { mutable: true } => {
                "Full ownership with mutation rights. Can read, write, move, or drop.".to_string()
            }
            BindingState::Owned { mutable: false } => {
                "Immutable ownership. Can read, move, or drop, but cannot mutate.".to_string()
            }
            BindingState::SharedBorrow { .. } => {
                "Shared reference (&T). Read-only access to borrowed value.".to_string()
            }
            BindingState::MutBorrow { .. } => {
                "Mutable reference (&mut T). Exclusive read-write access to borrowed value."
                    .to_string()
            }
            BindingState::Moved { .. } => {
                "Value has been moved out. This binding can no longer be used.".to_string()
            }
            BindingState::PartiallyMoved { moved_fields } => {
                format!(
                    "Some fields moved out ({}). Remaining fields may still be accessible.",
                    moved_fields.join(", ")
                )
            }
            BindingState::Dropped => {
                "Value has been dropped (freed). Scope has ended.".to_string()
            }
            BindingState::Suspended { .. } => {
                "Access suspended due to active reborrow. Will resume when borrow ends.".to_string()
            }
            BindingState::Copied => {
                "Value was copied (Copy trait). Original binding unchanged.".to_string()
            }
            BindingState::Shared { borrowed_by, .. } => {
                format!(
                    "Temporarily shared (shr): {} active shared borrow(s). \
                     Read-only access; mutation blocked until borrows end.",
                    borrowed_by.len()
                )
            }
            BindingState::Frozen { .. } => {
                "Temporarily frozen (frz): active &mut borrow exists. \
                 No read or write access until the mutable borrow ends."
                    .to_string()
            }
        }
    }
}

/// Scope information for tracking variable lifetimes.
#[derive(Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub bindings: Vec<BindingId>,
}

/// An annotation for a specific source location.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Annotation {
    /// The text range this annotation applies to
    pub span: (u32, u32), // (start, end) as u32 for serialization
    /// Name of the binding
    pub binding: String,
    /// Current state of the binding
    pub state: BindingState,
    /// Human-readable explanation
    pub explanation: String,
}

impl Annotation {
    pub fn new(span: TextRange, binding: String, state: BindingState, explanation: String) -> Self {
        Self {
            span: (u32::from(span.start()), u32::from(span.end())),
            binding,
            state,
            explanation,
        }
    }

    /// Get the capabilities for this annotation's state.
    pub fn capabilities(&self) -> Capabilities {
        self.state.capabilities()
    }
}

/// Entry in an ownership set, representing a binding's current state.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetEntry {
    /// Name of the binding
    pub name: String,
    /// Whether it's mutable (for owned bindings)
    pub mutable: bool,
    /// Current state category
    pub state: SetEntryState,
    /// For borrows: name of the binding being borrowed
    pub borrows_from: Option<String>,
}

/// Simplified state for set notation display.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SetEntryState {
    /// Owned binding (shows as `x` or `mut x`)
    Owned,
    /// Shared/downgraded due to active &T borrows (shows as `shr x`)
    Shared { borrowed_by: Vec<String> },
    /// Frozen due to active &mut T borrow (shows as `frz x`)
    Frozen { borrowed_by: String },
    /// Shared borrow (shows as `r(&x)`)
    SharedBorrow,
    /// Mutable borrow (shows as `r(&mut x)`)
    MutBorrow,
    /// Moved to another binding (value lives on, source invalid)
    Moved { to: Option<String> },
    /// Dropped at scope exit (value deallocated)
    Dropped,
}

/// Ownership set snapshot at a program point.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OwnershipSet {
    /// Name of the enclosing function/scope
    pub scope_name: String,
    /// Entries in the set (in declaration order)
    pub entries: Vec<SetEntry>,
    /// Line number this set applies to
    pub line: u32,
}

impl OwnershipSet {
    pub fn new(scope_name: String, line: u32) -> Self {
        Self {
            scope_name,
            entries: Vec::new(),
            line,
        }
    }

    /// Add an owned binding to the set.
    pub fn add_owned(&mut self, name: String, mutable: bool) {
        self.entries.push(SetEntry {
            name,
            mutable,
            state: SetEntryState::Owned,
            borrows_from: None,
        });
    }

    /// Add a shared (shr) binding - original being borrowed by &T.
    pub fn add_shared(&mut self, name: String, mutable: bool, borrowed_by: Vec<String>) {
        self.entries.push(SetEntry {
            name,
            mutable,
            state: SetEntryState::Shared { borrowed_by },
            borrows_from: None,
        });
    }

    /// Add a frozen (frz) binding - original with active &mut.
    pub fn add_frozen(&mut self, name: String, mutable: bool, borrowed_by: String) {
        self.entries.push(SetEntry {
            name,
            mutable,
            state: SetEntryState::Frozen { borrowed_by },
            borrows_from: None,
        });
    }

    /// Add a shared borrow binding.
    pub fn add_shared_borrow(&mut self, name: String, borrows_from: String) {
        self.entries.push(SetEntry {
            name,
            mutable: false,
            state: SetEntryState::SharedBorrow,
            borrows_from: Some(borrows_from),
        });
    }

    /// Add a mutable borrow binding.
    pub fn add_mut_borrow(&mut self, name: String, borrows_from: String) {
        self.entries.push(SetEntry {
            name,
            mutable: false,
            state: SetEntryState::MutBorrow,
            borrows_from: Some(borrows_from),
        });
    }

    /// Add a moved binding (value transferred elsewhere).
    pub fn add_moved(&mut self, name: String, to: Option<String>) {
        self.entries.push(SetEntry {
            name,
            mutable: false,
            state: SetEntryState::Moved { to },
            borrows_from: None,
        });
    }

    /// Add a dropped binding.
    pub fn add_dropped(&mut self, name: String) {
        self.entries.push(SetEntry {
            name,
            mutable: false,
            state: SetEntryState::Dropped,
            borrows_from: None,
        });
    }

    /// Check if set is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl std::fmt::Display for OwnershipSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{{{}}}", self.scope_name, self.format_entries())
    }
}

impl OwnershipSet {
    /// Format entries for display.
    fn format_entries(&self) -> String {
        self.entries
            .iter()
            .map(|e| e.format())
            .collect::<Vec<_>>()
            .join(", ")
    }
}

impl SetEntry {
    /// Format a single entry for display.
    fn format(&self) -> String {
        match &self.state {
            SetEntryState::Owned => {
                if self.mutable {
                    format!("mut {}", self.name)
                } else {
                    self.name.clone()
                }
            }
            SetEntryState::Shared { borrowed_by } => {
                if borrowed_by.is_empty() {
                    format!("shr {}", self.name)
                } else {
                    format!("shr {} (by {})", self.name, borrowed_by.join(", "))
                }
            }
            SetEntryState::Frozen { borrowed_by } => {
                format!("frz {} (by {})", self.name, borrowed_by)
            }
            SetEntryState::SharedBorrow => {
                if let Some(from) = &self.borrows_from {
                    format!("{}(&{})", self.name, from)
                } else {
                    format!("{}(&?)", self.name)
                }
            }
            SetEntryState::MutBorrow => {
                if let Some(from) = &self.borrows_from {
                    format!("{}(&mut {})", self.name, from)
                } else {
                    format!("{}(&mut ?)", self.name)
                }
            }
            SetEntryState::Moved { to } => {
                if let Some(target) = to {
                    format!("{} → {}", self.name, target)
                } else {
                    format!("{} → _", self.name)
                }
            }
            SetEntryState::Dropped => format!("†{}", self.name),
        }
    }
}

/// Annotation for ownership set at a line.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetAnnotation {
    /// Line number (0-indexed)
    pub line: u32,
    /// The ownership set at this point
    pub set: OwnershipSet,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_capabilities_display() {
        assert_eq!(format!("{}", Capabilities::OWNED), "ORW");
        assert_eq!(format!("{}", Capabilities::OWNED_IMMUT), "OR");
        assert_eq!(format!("{}", Capabilities::SHARED_BORROW), "R");
        assert_eq!(format!("{}", Capabilities::MUT_BORROW), "RW");
        assert_eq!(format!("{}", Capabilities::NONE), "∅");
    }

    #[test]
    fn test_binding_state_capabilities() {
        assert_eq!(
            BindingState::Owned { mutable: true }.capabilities(),
            Capabilities::OWNED
        );
        assert_eq!(
            BindingState::Moved { to: None }.capabilities(),
            Capabilities::NONE
        );
    }

    #[test]
    fn test_ownership_set_display() {
        let mut set = OwnershipSet::new("main".to_string(), 0);
        assert_eq!(format!("{}", set), "main{}");

        set.add_owned("x".to_string(), true);
        assert_eq!(format!("{}", set), "main{mut x}");

        set.add_shared_borrow("r".to_string(), "x".to_string());
        // After adding borrow, x should become shr, but we test the add methods here
        assert_eq!(format!("{}", set), "main{mut x, r(&x)}");
    }

    #[test]
    fn test_ownership_set_shr_frz() {
        let mut set = OwnershipSet::new("main".to_string(), 0);
        set.add_shared("x".to_string(), true, vec!["r".to_string()]);
        set.add_shared_borrow("r".to_string(), "x".to_string());
        assert_eq!(format!("{}", set), "main{shr x (by r), r(&x)}");

        let mut set2 = OwnershipSet::new("main".to_string(), 0);
        set2.add_frozen("x".to_string(), true, "r".to_string());
        set2.add_mut_borrow("r".to_string(), "x".to_string());
        assert_eq!(format!("{}", set2), "main{frz x (by r), r(&mut x)}");
    }
}
