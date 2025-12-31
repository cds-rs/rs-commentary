//! Variable-centric state tracking for ownership visualization.
//!
//! This module provides a cleaner architecture where each variable tracks its own
//! state, scope, lifetime, and borrow relationships. The StateTable manages all
//! variables and provides queries for rendering.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::analysis::SetEntryState;

/// A tracked variable with its ownership state and relationships.
///
/// Variables are the central data structure - they know their own state,
/// when they were introduced/dropped, and their borrow relationships.
#[derive(Debug)]
pub struct Variable {
    /// Variable name.
    pub name: String,
    /// Current ownership state.
    state: Cell<SetEntryState>,
    /// Whether declared as mutable.
    pub mutable: bool,
    /// Function scope this variable belongs to.
    pub scope: String,
    /// Line where this variable was introduced.
    pub intro_line: u32,
    /// Line where this variable is dropped (None if still live at end of scope).
    drop_line: Cell<Option<u32>>,
    /// If this is a borrow, what variable it borrows from.
    borrows_from: RefCell<Option<Weak<Variable>>>,
    /// Name of the variable this borrows from (for display).
    pub borrows_from_name: RefCell<Option<String>>,
    /// Variables that borrow from this one.
    borrowed_by: RefCell<Vec<Weak<Variable>>>,
}

impl Variable {
    /// Create a new variable.
    pub fn new(name: String, state: SetEntryState, mutable: bool, scope: String, intro_line: u32) -> Self {
        Self {
            name,
            state: Cell::new(state),
            mutable,
            scope,
            intro_line,
            drop_line: Cell::new(None),
            borrows_from: RefCell::new(None),
            borrows_from_name: RefCell::new(None),
            borrowed_by: RefCell::new(Vec::new()),
        }
    }

    /// Get the current state.
    pub fn state(&self) -> SetEntryState {
        self.state.get()
    }

    /// Set the state.
    pub fn set_state(&self, state: SetEntryState) {
        self.state.set(state);
    }

    /// Get the drop line if set.
    pub fn drop_line(&self) -> Option<u32> {
        self.drop_line.get()
    }

    /// Set when this variable is dropped.
    pub fn set_drop_line(&self, line: u32) {
        self.drop_line.set(Some(line));
    }

    /// Check if this variable is live at a given line.
    pub fn is_live_at(&self, line: u32) -> bool {
        if line < self.intro_line {
            return false;
        }
        match self.drop_line.get() {
            Some(drop) => line < drop,
            None => true,
        }
    }

    /// Check if this variable is being dropped at a given line.
    pub fn is_dropped_at(&self, line: u32) -> bool {
        self.drop_line.get() == Some(line)
    }

    /// Check if this is a borrow (reference).
    pub fn is_borrow(&self) -> bool {
        matches!(self.state.get(), SetEntryState::SharedBorrow | SetEntryState::MutBorrow)
    }

    /// Set the borrow source.
    pub fn set_borrows_from(&self, source: &Rc<Variable>) {
        *self.borrows_from.borrow_mut() = Some(Rc::downgrade(source));
        *self.borrows_from_name.borrow_mut() = Some(source.name.clone());
        source.borrowed_by.borrow_mut().push(Rc::downgrade(&Rc::new(self.clone_shallow())));
    }

    /// Get the name of the variable this borrows from.
    pub fn get_borrows_from_name(&self) -> Option<String> {
        self.borrows_from_name.borrow().clone()
    }

    /// Check if this variable is currently borrowed.
    pub fn is_borrowed(&self) -> bool {
        !self.borrowed_by.borrow().is_empty()
    }

    /// Register that another variable borrows from this one.
    pub fn add_borrower(&self, borrower: Weak<Variable>) {
        self.borrowed_by.borrow_mut().push(borrower);
    }

    /// Remove a borrower (when the borrow ends).
    pub fn remove_borrower(&self, name: &str) {
        self.borrowed_by.borrow_mut().retain(|weak| {
            weak.upgrade().map(|v| v.name != name).unwrap_or(false)
        });
    }

    /// Shallow clone for weak reference (doesn't clone RefCell contents deeply).
    fn clone_shallow(&self) -> Self {
        Self {
            name: self.name.clone(),
            state: Cell::new(self.state.get()),
            mutable: self.mutable,
            scope: self.scope.clone(),
            intro_line: self.intro_line,
            drop_line: Cell::new(self.drop_line.get()),
            borrows_from: RefCell::new(None),
            borrows_from_name: RefCell::new(self.borrows_from_name.borrow().clone()),
            borrowed_by: RefCell::new(Vec::new()),
        }
    }
}

/// Information about a variable's state at a specific line.
#[derive(Debug, Clone)]
pub struct VariableSnapshot {
    pub name: String,
    pub state: SetEntryState,
    pub mutable: bool,
    pub borrows_from: Option<String>,
    pub is_new: bool,        // Just introduced on this line
    pub is_dropped: bool,    // Being dropped on this line
    pub is_freed: bool,      // Source freed from borrow on this line
}

/// Central state table managing all variables.
///
/// Provides scope-aware queries for variables and their states.
#[derive(Debug, Default)]
pub struct StateTable {
    /// All variables indexed by (scope, name).
    variables: HashMap<(String, String), Rc<Variable>>,
    /// Map from line number to scope name.
    line_scopes: HashMap<u32, String>,
    /// Current scope being processed.
    current_scope: String,
}

impl StateTable {
    /// Create a new empty state table.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the scope for a line.
    pub fn set_line_scope(&mut self, line: u32, scope: String) {
        self.line_scopes.insert(line, scope.clone());
        self.current_scope = scope;
    }

    /// Get the scope for a line.
    pub fn get_scope(&self, line: u32) -> Option<&String> {
        self.line_scopes.get(&line)
    }

    /// Enter a new function scope.
    pub fn enter_scope(&mut self, scope: String, line: u32) {
        self.current_scope = scope.clone();
        self.line_scopes.insert(line, scope);
    }

    /// Get or create a variable.
    ///
    /// Note: This does NOT update state if the variable already exists.
    /// Variables store their initial state; per-line state comes from ownership analysis.
    pub fn get_or_create(
        &mut self,
        name: &str,
        state: SetEntryState,
        mutable: bool,
        line: u32,
    ) -> Rc<Variable> {
        let key = (self.current_scope.clone(), name.to_string());

        if let Some(var) = self.variables.get(&key) {
            // Don't update state - Variable stores initial state only
            // Per-line state should come from ownership analysis sets
            Rc::clone(var)
        } else {
            // Create new variable with initial state
            let var = Rc::new(Variable::new(
                name.to_string(),
                state,
                mutable,
                self.current_scope.clone(),
                line,
            ));
            self.variables.insert(key, Rc::clone(&var));
            var
        }
    }

    /// Get a variable by scope and name.
    pub fn get(&self, scope: &str, name: &str) -> Option<Rc<Variable>> {
        self.variables.get(&(scope.to_string(), name.to_string())).cloned()
    }

    /// Set up a borrow relationship.
    pub fn set_borrow(&mut self, borrower_name: &str, source_name: &str, line: u32) {
        let scope = self.current_scope.clone();

        if let (Some(borrower), Some(source)) = (
            self.get(&scope, borrower_name),
            self.get(&scope, source_name),
        ) {
            *borrower.borrows_from.borrow_mut() = Some(Rc::downgrade(&source));
            *borrower.borrows_from_name.borrow_mut() = Some(source_name.to_string());
            source.add_borrower(Rc::downgrade(&borrower));

            // Update source state to shared/frozen
            match borrower.state() {
                SetEntryState::SharedBorrow => {
                    if !matches!(source.state(), SetEntryState::Frozen) {
                        source.set_state(SetEntryState::Shared);
                    }
                }
                SetEntryState::MutBorrow => {
                    source.set_state(SetEntryState::Frozen);
                }
                _ => {}
            }
        }
    }

    /// Set the drop line for a variable (from semantic analysis).
    pub fn set_drop_line(&mut self, scope: &str, name: &str, line: u32) {
        if let Some(var) = self.get(scope, name) {
            var.set_drop_line(line);
        }
    }

    /// Set semantic drop data from rust-analyzer.
    /// Maps variable name to the line AFTER its last use.
    pub fn set_semantic_drops(&mut self, drops: &HashMap<String, u32>) {
        for (name, &drop_line) in drops {
            // Try to find this variable in any scope
            for ((scope, var_name), var) in &self.variables {
                if var_name == name {
                    var.set_drop_line(drop_line);
                }
            }
        }
    }

    /// Get all live variables at a line in the line's scope.
    pub fn get_live_at(&self, line: u32) -> Vec<Rc<Variable>> {
        let Some(scope) = self.line_scopes.get(&line) else {
            return Vec::new();
        };

        self.variables
            .iter()
            .filter(|((s, _), _)| s == scope)
            .filter(|(_, var)| var.is_live_at(line))
            .map(|(_, var)| Rc::clone(var))
            .collect()
    }

    /// Get variables being dropped at a line.
    pub fn get_drops_at(&self, line: u32) -> Vec<Rc<Variable>> {
        let Some(scope) = self.line_scopes.get(&line) else {
            return Vec::new();
        };

        self.variables
            .iter()
            .filter(|((s, _), _)| s == scope)
            .filter(|(_, var)| var.is_dropped_at(line))
            .map(|(_, var)| Rc::clone(var))
            .collect()
    }

    /// Get a snapshot of variable states at a line for rendering.
    pub fn get_snapshot(&self, line: u32) -> Vec<VariableSnapshot> {
        let Some(scope) = self.line_scopes.get(&line) else {
            eprintln!("DEBUG get_snapshot({}): no scope found", line);
            return Vec::new();
        };

        eprintln!("DEBUG get_snapshot({}): scope={}", line, scope);
        let mut snapshots = Vec::new();

        // Get variables being dropped at this line
        let drops_at_line: Vec<_> = self.get_drops_at(line);
        let live_at_line: Vec<_> = self.get_live_at(line);

        eprintln!("DEBUG get_snapshot({}): live=[{}] drops=[{}]",
            line,
            live_at_line.iter().map(|v| format!("{}(intro={},drop={:?})", v.name, v.intro_line, v.drop_line())).collect::<Vec<_>>().join(", "),
            drops_at_line.iter().map(|v| format!("{}(intro={},drop={:?})", v.name, v.intro_line, v.drop_line())).collect::<Vec<_>>().join(", "));

        let drop_names: std::collections::HashSet<_> = drops_at_line.iter().map(|v| &v.name).collect();

        // Find sources being freed (borrowed_by a variable that's dropping)
        let mut freed_sources: std::collections::HashSet<String> = std::collections::HashSet::new();
        for dropped_var in &drops_at_line {
            if let Some(source_name) = dropped_var.get_borrows_from_name() {
                freed_sources.insert(source_name);
            }
        }

        // Live variables
        for var in &live_at_line {
            let is_new = var.intro_line == line;
            let is_freed = freed_sources.contains(&var.name);

            // Compute effective state
            let effective_state = if is_freed {
                // Borrow ended, return to owned
                SetEntryState::Owned
            } else {
                var.state()
            };

            eprintln!("DEBUG get_snapshot({}): var {} state={:?} effective={:?} is_new={} is_freed={}",
                line, var.name, var.state(), effective_state, is_new, is_freed);

            snapshots.push(VariableSnapshot {
                name: var.name.clone(),
                state: effective_state,
                mutable: var.mutable,
                borrows_from: if is_freed { None } else { var.get_borrows_from_name() },
                is_new,
                is_dropped: false,
                is_freed,
            });
        }

        // Dropped variables (show as dropped)
        for var in drops_at_line {
            let is_borrow = var.is_borrow();
            snapshots.push(VariableSnapshot {
                name: var.name.clone(),
                state: SetEntryState::Dropped,
                mutable: var.mutable,
                borrows_from: var.get_borrows_from_name(),
                is_new: false,
                is_dropped: true,
                is_freed: false,
            });
        }

        snapshots
    }

    /// Get the current scope.
    pub fn current_scope(&self) -> &str {
        &self.current_scope
    }
}
