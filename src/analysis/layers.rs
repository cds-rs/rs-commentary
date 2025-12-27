//! Layered analysis architecture.
//!
//! Layer 1: AST structure (always available)
//! Layer 2: Type queries (rust-analyzer)
//! Layer 3: Full semantics (future)

use ra_ap_syntax::TextSize;
use std::collections::HashMap;

use super::state::{BindingId, ScopeId};

/// Variable state in the state machine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarState {
    /// Declared but not initialized
    Uninit,

    /// Owns a value
    Owned { mutable: bool },

    /// Temporarily shared due to active &T borrows
    Shared {
        original_mutable: bool,
        borrow_count: usize,
    },

    /// Temporarily frozen due to active &mut T borrow
    Frozen { original_mutable: bool },

    /// Value moved out
    Moved,

    /// Value dropped (scope ended)
    Dropped,
}

impl VarState {
    pub fn is_accessible(&self) -> bool {
        matches!(self, VarState::Owned { .. } | VarState::Shared { .. })
    }

    pub fn is_mutable(&self) -> bool {
        matches!(self, VarState::Owned { mutable: true } | VarState::Shared { original_mutable: true, .. })
    }
}

/// Result of analyzing an expression - what Layer 1 can determine.
#[derive(Debug, Clone)]
pub enum AstEvent {
    /// New binding declared
    Declaration {
        id: BindingId,
        name: String,
        mutable: bool,
        has_initializer: bool,
        /// Type annotation if present
        type_annotation: Option<String>,
        /// Initializer expression text (for heuristics)
        initializer: Option<String>,
    },

    /// Shared borrow created: &x
    SharedBorrow {
        borrowed: BindingId,
        borrow_binding: Option<BindingId>,
    },

    /// Mutable borrow created: &mut x
    MutBorrow {
        borrowed: BindingId,
        borrow_binding: Option<BindingId>,
    },

    /// Possible move (Layer 2 can refine if it's actually Copy)
    PossibleMove {
        binding: BindingId,
        /// Context: function call, assignment, return, etc.
        context: MoveContext,
    },

    /// Scope entered
    ScopeEnter { scope: ScopeId },

    /// Scope exited - bindings may be dropped
    ScopeExit {
        scope: ScopeId,
        bindings: Vec<BindingId>,
    },

    /// Borrow ended (lexical: scope exit of borrow binding)
    BorrowEnd { borrow_binding: BindingId },

    /// Assignment to existing binding
    Assignment { binding: BindingId },
}

/// Context for a possible move.
#[derive(Debug, Clone)]
pub enum MoveContext {
    /// Passed to function: foo(x)
    FunctionArg {
        func_name: Option<String>,
        arg_index: usize,
    },
    /// Assigned to another binding: let y = x
    Assignment { target: String },
    /// Returned from function
    Return,
    /// Used in a macro
    Macro { macro_name: String },
}

/// What Layer 2 can tell us about a type/expression.
pub trait TypeOracle {
    /// Check if the type at this position is Copy.
    fn is_copy_at(&self, offset: TextSize) -> Option<bool>;

    /// Get function parameter type (is it &T, &mut T, or T?).
    fn param_mode(&self, call_offset: TextSize, arg_index: usize) -> Option<ParamMode>;

    /// Check if a type name is Copy (from annotation).
    fn is_type_copy(&self, type_name: &str) -> Option<bool>;
}

/// How a function parameter receives its argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamMode {
    /// By shared reference: &T
    SharedRef,
    /// By mutable reference: &mut T
    MutRef,
    /// By value (move or copy)
    ByValue,
}

/// Layer 1: Pure AST-based state machine.
pub struct AstStateMachine {
    /// Current state per binding
    states: HashMap<BindingId, VarState>,

    /// Binding metadata
    bindings: HashMap<BindingId, BindingMeta>,

    /// Active borrows: borrow_binding_id → borrowed_binding_id
    active_borrows: HashMap<BindingId, BindingId>,

    /// Bindings per scope
    scope_bindings: HashMap<ScopeId, Vec<BindingId>>,

    /// Scope stack
    scope_stack: Vec<ScopeId>,

    /// Next IDs
    next_binding_id: u32,
    next_scope_id: u32,
}

#[derive(Debug, Clone)]
pub struct BindingMeta {
    pub name: String,
    pub scope: ScopeId,
    pub is_copy: CopyStatus,
}

/// Whether a binding's type is Copy.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CopyStatus {
    /// Definitely Copy (primitive, reference, annotated)
    Yes,
    /// Definitely not Copy (known non-Copy type)
    No,
    /// Unknown - needs Layer 2 to determine
    Unknown,
}

impl AstStateMachine {
    pub fn new() -> Self {
        let root_scope = ScopeId(0);
        let mut scope_bindings = HashMap::new();
        scope_bindings.insert(root_scope, Vec::new());

        Self {
            states: HashMap::new(),
            bindings: HashMap::new(),
            active_borrows: HashMap::new(),
            scope_bindings,
            scope_stack: vec![root_scope],
            next_binding_id: 0,
            next_scope_id: 1,
        }
    }

    pub fn current_scope(&self) -> ScopeId {
        *self.scope_stack.last().unwrap_or(&ScopeId(0))
    }

    /// Process an AST event and update state.
    pub fn process(&mut self, event: AstEvent) {
        match event {
            AstEvent::Declaration {
                id,
                name,
                mutable,
                has_initializer,
                type_annotation,
                initializer,
            } => {
                let is_copy = self.infer_copy_status(type_annotation.as_deref(), initializer.as_deref());

                self.bindings.insert(id, BindingMeta {
                    name,
                    scope: self.current_scope(),
                    is_copy,
                });

                self.states.insert(id, if has_initializer {
                    VarState::Owned { mutable }
                } else {
                    VarState::Uninit
                });

                self.scope_bindings
                    .entry(self.current_scope())
                    .or_default()
                    .push(id);
            }

            AstEvent::SharedBorrow { borrowed, borrow_binding } => {
                // Downgrade borrowed binding to Shared
                if let Some(state) = self.states.get(&borrowed) {
                    match state {
                        VarState::Owned { mutable } => {
                            self.states.insert(borrowed, VarState::Shared {
                                original_mutable: *mutable,
                                borrow_count: 1,
                            });
                        }
                        VarState::Shared { original_mutable, borrow_count } => {
                            self.states.insert(borrowed, VarState::Shared {
                                original_mutable: *original_mutable,
                                borrow_count: borrow_count + 1,
                            });
                        }
                        _ => {}
                    }
                }

                // Track the borrow relationship
                if let Some(borrow_id) = borrow_binding {
                    self.active_borrows.insert(borrow_id, borrowed);
                }
            }

            AstEvent::MutBorrow { borrowed, borrow_binding } => {
                // Freeze the borrowed binding
                if let Some(state) = self.states.get(&borrowed) {
                    if let VarState::Owned { mutable } = state {
                        self.states.insert(borrowed, VarState::Frozen {
                            original_mutable: *mutable,
                        });
                    }
                }

                if let Some(borrow_id) = borrow_binding {
                    self.active_borrows.insert(borrow_id, borrowed);
                }
            }

            AstEvent::PossibleMove { binding, context: _ } => {
                // Layer 1: Check if we know it's Copy
                let is_copy = self.bindings.get(&binding)
                    .map(|m| m.is_copy.clone())
                    .unwrap_or(CopyStatus::Unknown);

                match is_copy {
                    CopyStatus::Yes => {
                        // Copy - no state change
                    }
                    CopyStatus::No | CopyStatus::Unknown => {
                        // Move (or assume move if unknown)
                        self.states.insert(binding, VarState::Moved);
                    }
                }
            }

            AstEvent::ScopeEnter { scope } => {
                self.scope_stack.push(scope);
                self.scope_bindings.entry(scope).or_default();
            }

            AstEvent::ScopeExit { scope, bindings } => {
                // End any borrows from this scope
                let borrows_to_end: Vec<_> = self.active_borrows
                    .iter()
                    .filter(|(borrow_id, _)| bindings.contains(borrow_id))
                    .map(|(b, _)| *b)
                    .collect();

                for borrow_id in borrows_to_end {
                    self.end_borrow(borrow_id);
                }

                // Drop all owned bindings in this scope
                for binding_id in &bindings {
                    if let Some(state) = self.states.get(binding_id) {
                        if matches!(state, VarState::Owned { .. }) {
                            self.states.insert(*binding_id, VarState::Dropped);
                        }
                    }
                }

                // Pop scope
                if self.scope_stack.last() == Some(&scope) {
                    self.scope_stack.pop();
                }
            }

            AstEvent::BorrowEnd { borrow_binding } => {
                self.end_borrow(borrow_binding);
            }

            AstEvent::Assignment { binding } => {
                // Reassignment restores to Owned
                if self.bindings.contains_key(&binding) {
                    let mutable = true; // If we can assign, it must be mut
                    self.states.insert(binding, VarState::Owned { mutable });
                }
            }
        }
    }

    fn end_borrow(&mut self, borrow_binding: BindingId) {
        if let Some(borrowed) = self.active_borrows.remove(&borrow_binding) {
            // Restore the borrowed binding
            if let Some(state) = self.states.get(&borrowed) {
                match state {
                    VarState::Shared { original_mutable, borrow_count } => {
                        if *borrow_count <= 1 {
                            self.states.insert(borrowed, VarState::Owned {
                                mutable: *original_mutable,
                            });
                        } else {
                            self.states.insert(borrowed, VarState::Shared {
                                original_mutable: *original_mutable,
                                borrow_count: borrow_count - 1,
                            });
                        }
                    }
                    VarState::Frozen { original_mutable } => {
                        self.states.insert(borrowed, VarState::Owned {
                            mutable: *original_mutable,
                        });
                    }
                    _ => {}
                }
            }
        }
    }

    /// Layer 1 heuristics for Copy inference.
    fn infer_copy_status(&self, type_annotation: Option<&str>, initializer: Option<&str>) -> CopyStatus {
        // Check type annotation first
        if let Some(ty) = type_annotation {
            if is_primitive_type(ty) || ty.starts_with('&') {
                return CopyStatus::Yes;
            }
            if is_known_non_copy(ty) {
                return CopyStatus::No;
            }
        }

        // Check initializer expression
        if let Some(init) = initializer {
            if is_non_copy_expr(init) {
                return CopyStatus::No;
            }
            // Literals of primitives
            if init.parse::<i64>().is_ok() || init.parse::<f64>().is_ok() {
                return CopyStatus::Yes;
            }
            if init == "true" || init == "false" {
                return CopyStatus::Yes;
            }
        }

        CopyStatus::Unknown
    }

    /// Get current state of a binding.
    pub fn get_state(&self, id: BindingId) -> Option<&VarState> {
        self.states.get(&id)
    }

    /// Get all binding states.
    pub fn all_states(&self) -> &HashMap<BindingId, VarState> {
        &self.states
    }

    /// Get binding metadata.
    pub fn get_binding(&self, id: BindingId) -> Option<&BindingMeta> {
        self.bindings.get(&id)
    }

    /// Look up binding by name in current scope chain.
    pub fn lookup(&self, name: &str) -> Option<BindingId> {
        for scope_id in self.scope_stack.iter().rev() {
            if let Some(bindings) = self.scope_bindings.get(scope_id) {
                for id in bindings.iter().rev() {
                    if let Some(meta) = self.bindings.get(id) {
                        if meta.name == name {
                            return Some(*id);
                        }
                    }
                }
            }
        }
        None
    }

    /// Allocate a new binding ID.
    pub fn new_binding_id(&mut self) -> BindingId {
        let id = BindingId(self.next_binding_id);
        self.next_binding_id += 1;
        id
    }

    /// Allocate a new scope ID.
    pub fn new_scope_id(&mut self) -> ScopeId {
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;
        id
    }

    /// Refine a binding's Copy status (called by Layer 2).
    pub fn refine_copy_status(&mut self, id: BindingId, is_copy: bool) {
        if let Some(meta) = self.bindings.get_mut(&id) {
            meta.is_copy = if is_copy { CopyStatus::Yes } else { CopyStatus::No };
        }

        // If we marked it as Moved but it's actually Copy, restore it
        if is_copy {
            if let Some(VarState::Moved) = self.states.get(&id) {
                // Find original mutability from history (approximate)
                let mutable = self.bindings.get(&id)
                    .map(|_| false) // Conservative
                    .unwrap_or(false);
                self.states.insert(id, VarState::Owned { mutable });
            }
        }
    }

    /// Refine: mark a "possible move" as actually a borrow.
    pub fn refine_as_borrow(&mut self, binding: BindingId, is_mut: bool) {
        // If we marked it as Moved, change to Shared/Frozen
        if let Some(VarState::Moved) = self.states.get(&binding) {
            if self.bindings.contains_key(&binding) {
                let original_mutable = true; // Approximate
                if is_mut {
                    self.states.insert(binding, VarState::Frozen { original_mutable });
                } else {
                    self.states.insert(binding, VarState::Shared {
                        original_mutable,
                        borrow_count: 1,
                    });
                }
            }
        }
    }
}

/// Check if type name is a primitive.
fn is_primitive_type(ty: &str) -> bool {
    matches!(ty,
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" |
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" |
        "f32" | "f64" | "bool" | "char" | "()"
    )
}

/// Check if type name is a known non-Copy type.
fn is_known_non_copy(ty: &str) -> bool {
    ty.starts_with("String")
        || ty.starts_with("Vec")
        || ty.starts_with("Box")
        || ty.starts_with("Rc")
        || ty.starts_with("Arc")
        || ty.starts_with("HashMap")
        || ty.starts_with("HashSet")
        || ty.starts_with("BTreeMap")
        || ty.starts_with("BTreeSet")
}

/// Check if expression produces a non-Copy value.
fn is_non_copy_expr(expr: &str) -> bool {
    expr.starts_with("vec![")
        || expr.starts_with("format!")
        || expr.starts_with("String::from")
        || expr.starts_with("String::new")
        || expr.contains(".to_string()")
        || expr.contains(".to_owned()")
        || expr.contains(".clone()")  // clone result may or may not be Copy
        || expr.contains("::new(")    // constructors often non-Copy
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_declaration_and_move() {
        let mut sm = AstStateMachine::new();

        let x = sm.new_binding_id();
        sm.process(AstEvent::Declaration {
            id: x,
            name: "x".into(),
            mutable: false,
            has_initializer: true,
            type_annotation: Some("Vec<i32>".into()),
            initializer: None,
        });

        assert!(matches!(sm.get_state(x), Some(VarState::Owned { mutable: false })));

        sm.process(AstEvent::PossibleMove {
            binding: x,
            context: MoveContext::Assignment { target: "y".into() },
        });

        assert!(matches!(sm.get_state(x), Some(VarState::Moved)));
    }

    #[test]
    fn test_shared_borrow() {
        let mut sm = AstStateMachine::new();

        let x = sm.new_binding_id();
        sm.process(AstEvent::Declaration {
            id: x,
            name: "x".into(),
            mutable: true,
            has_initializer: true,
            type_annotation: None,
            initializer: Some("vec![1, 2, 3]".into()),
        });

        let r = sm.new_binding_id();
        sm.process(AstEvent::Declaration {
            id: r,
            name: "r".into(),
            mutable: false,
            has_initializer: true,
            type_annotation: None,
            initializer: None,
        });

        sm.process(AstEvent::SharedBorrow {
            borrowed: x,
            borrow_binding: Some(r),
        });

        assert!(matches!(sm.get_state(x), Some(VarState::Shared { borrow_count: 1, .. })));

        // End the borrow
        sm.process(AstEvent::BorrowEnd { borrow_binding: r });

        assert!(matches!(sm.get_state(x), Some(VarState::Owned { mutable: true })));
    }

    #[test]
    fn test_copy_type_no_move() {
        let mut sm = AstStateMachine::new();

        let x = sm.new_binding_id();
        sm.process(AstEvent::Declaration {
            id: x,
            name: "x".into(),
            mutable: false,
            has_initializer: true,
            type_annotation: Some("i32".into()),
            initializer: None,
        });

        sm.process(AstEvent::PossibleMove {
            binding: x,
            context: MoveContext::Assignment { target: "y".into() },
        });

        // i32 is Copy, so no move
        assert!(matches!(sm.get_state(x), Some(VarState::Owned { .. })));
    }

    #[test]
    fn test_layer2_refinement() {
        let mut sm = AstStateMachine::new();

        let x = sm.new_binding_id();
        sm.process(AstEvent::Declaration {
            id: x,
            name: "x".into(),
            mutable: false,
            has_initializer: true,
            type_annotation: None,  // Unknown type
            initializer: Some("get_value()".into()),  // Unknown if Copy
        });

        sm.process(AstEvent::PossibleMove {
            binding: x,
            context: MoveContext::FunctionArg { func_name: Some("foo".into()), arg_index: 0 },
        });

        // Layer 1 assumes move for unknown
        assert!(matches!(sm.get_state(x), Some(VarState::Moved)));

        // Layer 2 tells us it's actually a borrow
        sm.refine_as_borrow(x, false);

        assert!(matches!(sm.get_state(x), Some(VarState::Shared { .. })));
    }
}
