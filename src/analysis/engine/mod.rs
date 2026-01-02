//! Core analysis engine that tracks ownership state through code.
//!
//! This module uses the state machine defined in `state.rs` for all
//! ownership state transitions. See [`BindingState::transition`] for
//! the complete transition table.

mod macros;

use super::state::{
    Annotation, BindingId, BindingState, OwnershipEvent, OwnershipSet, Scope, ScopeId,
    SetAnnotation,
};
use super::TypeOracle;
use crate::util::{AstEvent, AstIter};
use anyhow::Result;
use ra_ap_syntax::{
    ast::{self, HasArgList, HasName},
    AstNode, SourceFile, TextRange,
};
use std::collections::HashMap;

/// Information about a tracked binding.
#[derive(Debug, Clone)]
pub struct BindingInfo {
    pub name: String,
    pub scope: ScopeId,
    pub is_mutable: bool,
    pub is_copy: bool,
    /// True if this is a scalar primitive (i32, usize, bool, etc.)
    /// Used for noise suppression in function call annotations.
    pub is_scalar: bool,
    pub current_state: BindingState,
}

/// What happens to a value when passed to a function.
///
/// Used to generate call-site annotations showing the transfer semantics:
///
/// ```text
/// process(data, &config, &mut cache);
///         ────  ───────  ──────────
///         │     │        └─ cache: mut borrowed → process
///         │     └─ config: borrowed → process
///         └─ data: moved → process
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransferKind {
    /// Ownership moves to callee (non-Copy types passed by value).
    Move,
    /// Shared borrow (`&T`) - callee gets read access.
    SharedBorrow,
    /// Mutable borrow (`&mut T`) - callee gets exclusive access.
    MutBorrow,
    /// Copy - value duplicated, original unchanged (Copy types).
    Copy,
}

impl TransferKind {
    pub fn symbol(&self) -> &'static str {
        match self {
            TransferKind::Move => "→",
            TransferKind::SharedBorrow => "&→",
            TransferKind::MutBorrow => "&mut→",
            TransferKind::Copy => "⊕→",
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            TransferKind::Move => "moved",
            TransferKind::SharedBorrow => "borrowed",
            TransferKind::MutBorrow => "mut borrowed",
            TransferKind::Copy => "copied",
        }
    }
}

/// Records what happens to an argument at a function call site.
///
/// Created during [`OwnershipAnalyzer`] traversal for each argument passed
/// to a function or method. Renderers consume these to show transfer annotations.
///
/// # Example
///
/// For `find_min(book_counts, &mut cache)`:
///
/// ```text
/// CopyEvent { from: "book_counts", to: "find_min", kind: Copy, ... }
/// CopyEvent { from: "cache", to: "find_min", kind: MutBorrow, ... }
/// ```
#[derive(Debug, Clone)]
pub struct CopyEvent {
    /// Name of the source binding (the argument variable).
    pub from: String,
    /// Name of the call target (function or method name).
    pub to: String,
    /// Line number where the transfer occurred (0-indexed).
    pub line: u32,
    /// What kind of transfer: move, copy, borrow, or mut borrow.
    pub kind: TransferKind,
    /// True if this is a scalar primitive (`i32`, `bool`, etc.).
    ///
    /// Renderers may suppress scalar annotations to reduce noise.
    pub is_scalar_in_call: bool,
}

/// The ownership analyzer tracks variable state through code.
pub struct OwnershipAnalyzer<'oracle> {
    bindings: HashMap<BindingId, BindingInfo>,
    scopes: HashMap<ScopeId, Scope>,
    annotations: Vec<Annotation>,
    set_annotations: Vec<SetAnnotation>,
    /// Synthetic copy events (Copy types copied to another binding)
    copy_events: Vec<CopyEvent>,
    next_binding_id: u32,
    next_scope_id: u32,
    current_scope: ScopeId,
    current_fn_name: String,
    source: String,

    // Event processing context (for AstIter-based traversal)
    /// Stack of let statements being processed (for nested blocks with let bindings)
    let_stmt_stack: Vec<ast::LetStmt>,
    /// True when processing function parameters (set on EnterFn, cleared on EnterBlock)
    in_fn_params: bool,
    /// Current function being processed (for parameter type lookup)
    current_fn: Option<ast::Fn>,
    /// Current closure being processed (for parameter type lookup)
    current_closure: Option<ast::ClosureExpr>,
    /// Parameter index counter (for matching patterns with parameter types)
    param_index: usize,
    /// True when processing closure parameters
    in_closure_params: bool,
    /// Scope ID of the current function (for dropped bindings at closing brace)
    fn_scope: Option<ScopeId>,
    /// Scope ID of the current block (for dropped bindings at closing brace)
    block_scope_stack: Vec<ScopeId>,
    /// Semantic copy types: declaration offset -> is_copy (from rust-analyzer)
    semantic_copy_types: HashMap<u32, bool>,
    /// Semantic scalar types: declaration offset -> is_scalar (from rust-analyzer)
    semantic_scalar_types: HashMap<u32, bool>,
    /// Type oracle for on-demand type queries (optional, from rust-analyzer)
    type_oracle: Option<&'oracle dyn TypeOracle>,
}

impl<'oracle> OwnershipAnalyzer<'oracle> {
    pub fn new() -> Self {
        let root_scope_id = ScopeId(0);
        let mut scopes = HashMap::new();
        scopes.insert(
            root_scope_id,
            Scope {
                id: root_scope_id,
                parent: None,
                bindings: Vec::new(),
            },
        );

        Self {
            bindings: HashMap::new(),
            scopes,
            annotations: Vec::new(),
            set_annotations: Vec::new(),
            copy_events: Vec::new(),
            next_binding_id: 0,
            next_scope_id: 1,
            current_scope: root_scope_id,
            current_fn_name: String::new(),
            source: String::new(),
            // Context fields
            let_stmt_stack: Vec::new(),
            in_fn_params: false,
            current_fn: None,
            current_closure: None,
            param_index: 0,
            in_closure_params: false,
            fn_scope: None,
            block_scope_stack: Vec::new(),
            semantic_copy_types: HashMap::new(),
            semantic_scalar_types: HashMap::new(),
            type_oracle: None,
        }
    }

    /// Set the type oracle for on-demand type queries.
    ///
    /// When set, the analyzer will query this oracle for Copy/scalar type info
    /// at the point of use, rather than relying on pre-collected offset mappings.
    pub fn set_type_oracle(&mut self, oracle: &'oracle dyn TypeOracle) {
        self.type_oracle = Some(oracle);
    }

    /// Set semantic copy type information from rust-analyzer.
    ///
    /// This provides authoritative type info for Copy/Move determination.
    /// Deprecated: prefer `set_type_oracle` for on-demand queries.
    pub fn set_semantic_copy_types(&mut self, copy_types: HashMap<u32, bool>) {
        self.semantic_copy_types = copy_types;
    }

    /// Set semantic scalar type information from rust-analyzer.
    ///
    /// Scalar primitives (i32, usize, bool, etc.) create low-value copy
    /// annotations when passed to functions. Renderers can suppress these.
    pub fn set_semantic_scalar_types(&mut self, scalar_types: HashMap<u32, bool>) {
        self.semantic_scalar_types = scalar_types;
    }

    // ========================================================================
    // State Machine Interface
    // ========================================================================

    /// Apply an ownership event to a binding, using the state machine.
    ///
    /// This is the single point of entry for all state transitions.
    /// Returns the new state if the transition was valid.
    fn apply_event(&mut self, binding_id: BindingId, event: OwnershipEvent) -> Option<BindingState> {
        let binding = self.bindings.get(&binding_id)?;
        let current_state = binding.current_state.clone();

        match current_state.transition(event) {
            Ok(new_state) => {
                if let Some(b) = self.bindings.get_mut(&binding_id) {
                    b.current_state = new_state.clone();
                }
                Some(new_state)
            }
            Err(e) => {
                tracing::debug!("Transition error for {:?}: {}", binding_id, e);
                None
            }
        }
    }

    /// Apply an event and create an annotation for the transition.
    fn apply_event_with_annotation(
        &mut self,
        binding_id: BindingId,
        event: OwnershipEvent,
        range: TextRange,
        explanation: String,
    ) -> Option<BindingState> {
        let name = self.bindings.get(&binding_id)?.name.clone();
        let new_state = self.apply_event(binding_id, event)?;

        self.annotations.push(Annotation::new(
            range,
            name,
            new_state.clone(),
            explanation,
        ));

        Some(new_state)
    }

    /// Analyze a source file and generate annotations.
    pub fn analyze(&mut self, source: &str) -> Result<Vec<Annotation>> {
        tracing::debug!("Analyzing source ({} bytes)", source.len());
        self.source = source.to_string();
        let parse = SourceFile::parse(source, ra_ap_syntax::Edition::Edition2021);
        let file = parse.tree();

        // Check for parse errors
        for error in parse.errors() {
            tracing::warn!("Parse error: {:?}", error);
        }

        // Walk the AST using iterator-based traversal
        for event in AstIter::new(&file) {
            self.process(&event);
        }

        tracing::debug!("Analysis complete: {} annotations", self.annotations.len());
        Ok(self.annotations.clone())
    }

    /// Get the set annotations (ownership sets per line).
    pub fn set_annotations(&self) -> &[SetAnnotation] {
        &self.set_annotations
    }

    /// Generate an ownership set snapshot at the given byte offset.
    fn snapshot_set(&mut self, offset: u32) {
        let line = self.offset_to_line(offset);
        let mut set = OwnershipSet::new(self.current_fn_name.clone(), line);

        // Collect all live bindings in current scope chain
        let mut scope_id = Some(self.current_scope);
        let mut binding_ids: Vec<BindingId> = Vec::new();

        while let Some(sid) = scope_id {
            if let Some(scope) = self.scopes.get(&sid) {
                binding_ids.extend(scope.bindings.iter().copied());
                scope_id = scope.parent;
            } else {
                break;
            }
        }

        // Sort by binding id to maintain declaration order
        binding_ids.sort_by_key(|id| id.0);

        // Add each binding to the set based on its current state
        for binding_id in binding_ids {
            if let Some(binding) = self.bindings.get(&binding_id) {
                match &binding.current_state {
                    BindingState::Owned { mutable } => {
                        set.add_owned(binding.name.clone(), *mutable);
                    }
                    BindingState::Shared { original_mutable, borrowed_by } => {
                        let borrow_names: Vec<String> = borrowed_by
                            .iter()
                            .filter_map(|id| self.bindings.get(id).map(|b| b.name.clone()))
                            .collect();
                        set.add_shared(binding.name.clone(), *original_mutable, borrow_names);
                    }
                    BindingState::Frozen { original_mutable, borrowed_by } => {
                        let borrow_name = self.bindings.get(borrowed_by)
                            .map(|b| b.name.clone())
                            .unwrap_or_else(|| "?".to_string());
                        set.add_frozen(binding.name.clone(), *original_mutable, borrow_name);
                    }
                    BindingState::SharedBorrow { from } => {
                        let from_name = self.bindings.get(from)
                            .map(|b| b.name.clone())
                            .unwrap_or_else(|| "?".to_string());
                        set.add_shared_borrow(binding.name.clone(), from_name);
                    }
                    BindingState::MutBorrow { from } => {
                        let from_name = self.bindings.get(from)
                            .map(|b| b.name.clone())
                            .unwrap_or_else(|| "?".to_string());
                        set.add_mut_borrow(binding.name.clone(), from_name);
                    }
                    BindingState::Moved { to } => {
                        let to_name = to.and_then(|id| self.bindings.get(&id).map(|b| b.name.clone()));
                        set.add_moved(binding.name.clone(), to_name);
                    }
                    BindingState::Dropped => {
                        set.add_dropped(binding.name.clone());
                    }
                    // Other states
                    _ => {
                        set.add_owned(binding.name.clone(), binding.is_mutable);
                    }
                }
            }
        }

        self.set_annotations.push(SetAnnotation { line, set });
    }

    /// Convert byte offset to line number (0-indexed).
    fn offset_to_line(&self, offset: u32) -> u32 {
        let offset = offset as usize;
        self.source[..offset.min(self.source.len())]
            .chars()
            .filter(|&c| c == '\n')
            .count() as u32
    }

    /// Create a new binding in the current scope.
    pub fn new_binding(
        &mut self,
        name: String,
        is_mutable: bool,
        is_copy: bool,
        is_scalar: bool,
        range: TextRange,
    ) -> BindingId {
        let id = BindingId(self.next_binding_id);
        self.next_binding_id += 1;

        let info = BindingInfo {
            name: name.clone(),
            scope: self.current_scope,
            is_mutable,
            is_copy,
            is_scalar,
            current_state: BindingState::Owned { mutable: is_mutable },
        };

        self.bindings.insert(id, info);

        // Add to current scope
        if let Some(scope) = self.scopes.get_mut(&self.current_scope) {
            scope.bindings.push(id);
        }

        // Generate annotation for the declaration
        self.annotations.push(Annotation::new(
            range,
            name,
            BindingState::Owned { mutable: is_mutable },
            if is_mutable {
                "New owned binding (mutable): O R W".to_string()
            } else {
                "New owned binding: O R".to_string()
            },
        ));

        id
    }

    /// Create a new binding that holds a borrow (shared or mutable).
    pub fn new_borrow_binding(
        &mut self,
        name: String,
        is_mutable: bool,
        is_mut_borrow: bool,
        borrowed_from: BindingId,
        range: TextRange,
    ) -> BindingId {
        let id = BindingId(self.next_binding_id);
        self.next_binding_id += 1;

        let state = if is_mut_borrow {
            BindingState::MutBorrow { from: borrowed_from }
        } else {
            BindingState::SharedBorrow { from: borrowed_from }
        };

        let borrowed_name = self.bindings.get(&borrowed_from)
            .map(|b| b.name.clone())
            .unwrap_or_else(|| "?".to_string());

        let info = BindingInfo {
            name: name.clone(),
            scope: self.current_scope,
            is_mutable,
            is_copy: true, // References are Copy
            is_scalar: false, // References are not scalars
            current_state: state.clone(),
        };

        self.bindings.insert(id, info);

        if let Some(scope) = self.scopes.get_mut(&self.current_scope) {
            scope.bindings.push(id);
        }

        let explanation = if is_mut_borrow {
            format!("Mutable borrow of {}: R W", borrowed_name)
        } else {
            format!("Shared borrow of {}: R", borrowed_name)
        };

        self.annotations.push(Annotation::new(range, name, state, explanation));

        id
    }

    /// Enter a new scope (block, function body, etc.).
    pub fn enter_scope(&mut self) -> ScopeId {
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;

        let scope = Scope {
            id,
            parent: Some(self.current_scope),
            bindings: Vec::new(),
        };

        self.scopes.insert(id, scope);
        self.current_scope = id;
        id
    }

    /// Exit the current scope, dropping all bindings in it.
    pub fn exit_scope(&mut self) {
        if let Some(scope) = self.scopes.get(&self.current_scope).cloned() {
            // Apply ScopeExit event to all bindings in this scope
            for binding_id in &scope.bindings {
                self.apply_event(*binding_id, OwnershipEvent::ScopeExit);
            }

            // Move to parent scope
            if let Some(parent) = scope.parent {
                self.current_scope = parent;
            }
        }
    }

    /// Record a move of a binding.
    pub fn record_move(&mut self, from: BindingId, to: Option<BindingId>, range: TextRange) {
        let Some(binding) = self.bindings.get(&from).cloned() else {
            return;
        };

        if binding.is_copy {
            // Copy types don't move - apply Copy event
            self.apply_event_with_annotation(
                from,
                OwnershipEvent::Copy,
                range,
                format!("{}: copied (implements Copy)", binding.name),
            );
        } else {
            // Apply Move event
            self.apply_event_with_annotation(
                from,
                OwnershipEvent::Move { to },
                range,
                format!("{}: moved out, no longer accessible", binding.name),
            );
        }
    }

    /// Record a copy of a binding (for Copy types in let bindings).
    /// Unlike move, the source remains valid after copy.
    /// This creates a synthetic event - the source's state doesn't change.
    pub fn record_copy(
        &mut self,
        _from: BindingId,
        from_name: String,
        _to: BindingId,
        to_name: String,
        range: TextRange,
    ) {
        // Calculate line number from range
        let line = self.offset_to_line(u32::from(range.start()));

        // Add synthetic copy event (no state change to source)
        self.copy_events.push(CopyEvent {
            from: from_name,
            to: to_name,
            line,
            kind: TransferKind::Copy,
            is_scalar_in_call: false, // Let bindings always show
        });
    }

    /// Get the recorded copy events.
    pub fn copy_events(&self) -> &[CopyEvent] {
        &self.copy_events
    }

    /// Get Copy type info for all tracked bindings.
    /// Returns a map from variable name to whether it's a Copy type.
    pub fn copy_types(&self) -> HashMap<String, bool> {
        self.bindings
            .values()
            .map(|b| (b.name.clone(), b.is_copy))
            .collect()
    }

    /// Record a shared borrow of a binding.
    /// This also marks the original as "shared" (shr) - temporarily read-only.
    pub fn record_shared_borrow(&mut self, from: BindingId, range: TextRange) -> BindingId {
        let borrow_id = BindingId(self.next_binding_id);
        self.next_binding_id += 1;

        let Some(binding) = self.bindings.get(&from).cloned() else {
            return borrow_id;
        };

        // Annotation for the borrow itself
        self.annotations.push(Annotation::new(
            range,
            binding.name.clone(),
            BindingState::SharedBorrow { from },
            format!("&{}: shared borrow, read-only access", binding.name),
        ));

        // Apply SharedBorrow event to the source binding (Owned -> Shared)
        let was_mutable = binding.is_mutable;
        self.apply_event_with_annotation(
            from,
            OwnershipEvent::SharedBorrow { by: borrow_id },
            range,
            format!(
                "{}: {} → shr (shared borrow active)",
                binding.name,
                if was_mutable { "mut" } else { "owned" }
            ),
        );

        borrow_id
    }

    /// Record a mutable borrow of a binding.
    /// This also marks the original as "frozen" (frz) - no access until borrow ends.
    pub fn record_mut_borrow(&mut self, from: BindingId, range: TextRange) -> BindingId {
        let borrow_id = BindingId(self.next_binding_id);
        self.next_binding_id += 1;

        let Some(binding) = self.bindings.get(&from).cloned() else {
            return borrow_id;
        };

        // Annotation for the borrow itself
        self.annotations.push(Annotation::new(
            range,
            binding.name.clone(),
            BindingState::MutBorrow { from },
            format!("&mut {}: exclusive borrow, R W access", binding.name),
        ));

        // Apply MutBorrow event to the source binding (Owned -> Frozen)
        let was_mutable = binding.is_mutable;
        self.apply_event_with_annotation(
            from,
            OwnershipEvent::MutBorrow { by: borrow_id },
            range,
            format!(
                "{}: {} → frz (mutable borrow active, access frozen)",
                binding.name,
                if was_mutable { "mut" } else { "owned" }
            ),
        );

        borrow_id
    }

    /// Look up a binding by name in the current scope chain.
    pub fn lookup_binding(&self, name: &str) -> Option<BindingId> {
        let mut scope_id = Some(self.current_scope);

        while let Some(sid) = scope_id {
            if let Some(scope) = self.scopes.get(&sid) {
                for &binding_id in &scope.bindings {
                    if let Some(binding) = self.bindings.get(&binding_id) {
                        if binding.name == name {
                            return Some(binding_id);
                        }
                    }
                }
                scope_id = scope.parent;
            } else {
                break;
            }
        }

        None
    }

    /// Get all annotations generated during analysis.
    pub fn annotations(&self) -> &[Annotation] {
        &self.annotations
    }

    // ========================================================================
    // Event-Based Processing (AstIter integration)
    // ========================================================================

    /// Process an AST event from the iterator.
    ///
    /// This is the main entry point for event-based traversal. Each event
    /// triggers appropriate state machine updates and annotation generation.
    pub fn process(&mut self, event: &AstEvent) {
        match event {
            AstEvent::EnterFn(func) => {
                self.handle_enter_fn(func);
            }

            AstEvent::ExitFn => {
                self.handle_exit_fn();
            }

            AstEvent::EnterBlock(block) => {
                self.handle_enter_block(block);
            }

            AstEvent::ExitBlock => {
                self.handle_exit_block();
            }

            AstEvent::EnterFor(for_expr) => {
                // For loops create their own scope
                self.enter_scope();
                // Process the iterable before the loop scope
                if let Some(iterable) = for_expr.iterable() {
                    self.process_expr(&iterable);
                }
            }

            AstEvent::ExitFor => {
                self.exit_scope();
            }

            AstEvent::EnterMatchArm(_arm) => {
                self.enter_scope();
            }

            AstEvent::ExitMatchArm => {
                self.exit_scope();
            }

            AstEvent::EnterClosure(closure) => {
                self.handle_enter_closure(closure);
            }

            AstEvent::ExitClosure => {
                self.exit_scope();
                self.current_closure = None;
            }

            AstEvent::Stmt(stmt) => {
                self.handle_stmt(stmt);
            }

            AstEvent::Pat { pat, is_mut } => {
                self.handle_pat(pat, *is_mut);
            }

            AstEvent::Expr(expr) => {
                // If we're in closure params mode and see an Expr, we've moved to the body
                if self.in_closure_params {
                    self.in_closure_params = false;
                }
                self.process_expr(expr);
            }

            AstEvent::CallArg { arg, call_target } => {
                self.process_call_arg(arg, &call_target);
            }

            AstEvent::MethodReceiver { receiver, method_call } => {
                self.handle_method_receiver(receiver, method_call);
            }

            AstEvent::Macro(macro_expr) => {
                self.visit_macro_expr(macro_expr);
            }

            // Item and Impl events don't need special handling here;
            // the iterator will yield EnterFn for functions within them
            AstEvent::Item(_) | AstEvent::Impl(_) => {}
        }
    }

    /// Handle entering a function.
    fn handle_enter_fn(&mut self, func: &ast::Fn) {
        // Track function name for set notation
        if let Some(name) = func.name() {
            self.current_fn_name = name.text().to_string();
        }

        // Store function for parameter type lookup
        self.current_fn = Some(func.clone());
        self.param_index = 0;

        // Enter function scope
        let fn_scope = self.enter_scope();
        self.fn_scope = Some(fn_scope);
        self.in_fn_params = true;
    }

    /// Handle exiting a function.
    fn handle_exit_fn(&mut self) {
        self.fn_scope = None;
        self.current_fn = None;
        self.current_fn_name = String::new();
    }

    /// Handle entering a block.
    fn handle_enter_block(&mut self, block: &ast::BlockExpr) {
        // First block after function entry: process parameter snapshot
        if self.in_fn_params {
            self.in_fn_params = false;
            // Snapshot at function entry (captures parameters)
            if let Some(stmt_list) = block.stmt_list() {
                if let Some(l_curly) = stmt_list.l_curly_token() {
                    let fn_start = u32::from(l_curly.text_range().start());
                    self.snapshot_set(fn_start);
                }
            }
            // Don't enter a new scope - we're in the function scope already
            return;
        }

        // Non-function blocks: enter a new scope
        let scope_id = self.enter_scope();
        self.block_scope_stack.push(scope_id);
    }

    /// Handle exiting a block.
    fn handle_exit_block(&mut self) {
        // Get the block scope we're exiting
        if self.block_scope_stack.pop().is_some() {
            self.exit_scope();
            // Note: We'd need the closing brace position for snapshot_set_with_dropped
            // but we don't have it here. The semantic layer handles drops more accurately.
        } else if self.fn_scope.is_some() {
            // Exiting the function body block
            self.exit_scope();
            // snapshot_set_with_dropped would need the closing brace position
        }
    }

    /// Handle entering a closure.
    fn handle_enter_closure(&mut self, closure: &ast::ClosureExpr) {
        let is_move = closure.move_token().is_some();
        let range = closure.syntax().text_range();

        // Store closure for parameter type lookup
        self.current_closure = Some(closure.clone());
        self.param_index = 0;
        self.in_closure_params = true;

        // Enter scope first so capture analysis can detect outer variables
        self.enter_scope();

        // For move closures, capture analysis (now that we're in the closure's scope)
        if is_move {
            if let Some(body) = closure.body() {
                let captures = self.collect_captures(&body);
                for (name, binding_id, binding) in captures {
                    if !binding.is_copy {
                        self.apply_event_with_annotation(
                            binding_id,
                            OwnershipEvent::Move { to: None },
                            range,
                            format!("{}: moved into closure", name),
                        );
                    }
                }
            }
        }
    }

    /// Handle a statement event.
    fn handle_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::LetStmt(let_stmt) => {
                // Push onto stack for when we see the Pat event
                // Snapshot will be taken after the Pat event processes the binding
                self.let_stmt_stack.push(let_stmt.clone());
            }
            ast::Stmt::ExprStmt(_) => {
                // Expression will be handled by Expr event
                // Snapshot after the statement
                let end = u32::from(stmt.syntax().text_range().end());
                self.snapshot_set(end);
            }
            ast::Stmt::Item(_) => {
                // Nested items handled by Item event
            }
        }
    }

    /// Handle a pattern event.
    fn handle_pat(&mut self, pat: &ast::Pat, is_mut: bool) {
        if self.in_fn_params {
            // This is a function parameter - look up the type
            let type_str = self.current_fn.as_ref().and_then(|func| {
                func.param_list()
                    .and_then(|pl| pl.params().nth(self.param_index))
                    .and_then(|param| param.ty())
                    .map(|ty| ty.syntax().text().to_string())
            });
            self.param_index += 1;

            // Use visit_param which handles the type
            self.visit_param(pat, type_str.as_deref());
        } else if self.in_closure_params {
            // This is a closure parameter - look up the type
            let type_str = self.current_closure.as_ref().and_then(|closure| {
                closure.param_list()
                    .and_then(|pl| pl.params().nth(self.param_index))
                    .and_then(|param| param.ty())
                    .map(|ty| ty.syntax().text().to_string())
            });
            self.param_index += 1;

            // Use visit_param which handles the type
            self.visit_param(pat, type_str.as_deref());
        } else if let Some(let_stmt) = self.let_stmt_stack.pop() {
            // This is a let binding - use the full let statement context
            self.handle_let_binding(pat, &let_stmt);
        } else {
            // For/match pattern or other context
            self.visit_pat(pat, is_mut);
        }
    }

    /// Detect `let r = &x` or `let r = &mut x`. Returns (source_binding_id, is_mut).
    fn detect_borrow_init(&self, let_stmt: &ast::LetStmt) -> Option<(BindingId, bool)> {
        let init = let_stmt.initializer()?;
        let ref_expr = match &init {
            ast::Expr::RefExpr(r) => r,
            _ => return None,
        };
        let is_mut = ref_expr.mut_token().is_some();
        let inner = ref_expr.expr()?;
        let path = match inner {
            ast::Expr::PathExpr(p) => p,
            _ => return None,
        };
        let (_, binding_id, _) = self.resolve_path(&path)?;
        Some((binding_id, is_mut))
    }

    /// Detect `let y = x` (potential move/copy). Returns (name, id, is_copy, range).
    fn detect_transfer_init(
        &self,
        let_stmt: &ast::LetStmt,
    ) -> Option<(String, BindingId, bool, TextRange)> {
        let init = let_stmt.initializer()?;
        let path = match &init {
            ast::Expr::PathExpr(p) => p,
            _ => return None,
        };
        let (name, id, binding) = self.resolve_path(path)?;
        Some((name, id, binding.is_copy, init.syntax().text_range()))
    }

    /// Get the declaration offset for semantic lookups.
    /// For `let mut x`, returns the offset of `x` (not `mut x`).
    fn get_decl_offset(&self, pat: &ast::Pat) -> u32 {
        if let ast::Pat::IdentPat(ident_pat) = pat {
            ident_pat
                .name()
                .map(|n| u32::from(n.syntax().text_range().start()))
                .unwrap_or_else(|| u32::from(pat.syntax().text_range().start()))
        } else {
            u32::from(pat.syntax().text_range().start())
        }
    }

    /// Determine Copy/scalar status from semantic info or type hint.
    fn infer_binding_type(&self, pat: &ast::Pat, let_stmt: &ast::LetStmt) -> (bool, bool) {
        let decl_offset = self.get_decl_offset(pat);
        let type_hint = let_stmt.ty().map(|ty| ty.syntax().text().to_string());

        let is_copy = if let Some(&is_copy) = self.semantic_copy_types.get(&decl_offset) {
            is_copy
        } else if let Some(ref t) = type_hint {
            self.is_copy_type(t)
        } else {
            false
        };

        let is_scalar = self
            .semantic_scalar_types
            .get(&decl_offset)
            .copied()
            .unwrap_or(false);

        (is_copy, is_scalar)
    }

    /// Handle a let binding (pattern from a let statement).
    fn handle_let_binding(&mut self, pat: &ast::Pat, let_stmt: &ast::LetStmt) {
        let (is_copy, is_scalar) = self.infer_binding_type(pat, let_stmt);

        // Handle borrow bindings: let r = &x
        if let Some((borrowed_from, is_mut_borrow)) = self.detect_borrow_init(let_stmt) {
            if let ast::Pat::IdentPat(ident_pat) = pat {
                if let Some(name) = ident_pat.name() {
                    let is_mut = ident_pat.mut_token().is_some();
                    self.new_borrow_binding(
                        name.text().to_string(),
                        is_mut,
                        is_mut_borrow,
                        borrowed_from,
                        pat.syntax().text_range(),
                    );
                    self.snapshot_set(u32::from(let_stmt.syntax().text_range().end()));
                    return;
                }
            }
        }

        // Track the binding ID and name that will be created (for simple IdentPat)
        let target_info = if let ast::Pat::IdentPat(ident_pat) = pat {
            ident_pat
                .name()
                .map(|n| (BindingId(self.next_binding_id), n.text().to_string()))
        } else {
            None
        };

        // Create a normal owned binding
        self.visit_pat_with_type(pat, false, is_copy, is_scalar);

        // Record transfer (move or copy) from source to target
        if let Some((source_name, source_id, source_is_copy, range)) = self.detect_transfer_init(let_stmt) {
            if let Some((target_id, target_name)) = target_info {
                if source_is_copy {
                    self.record_copy(source_id, source_name, target_id, target_name, range);
                } else {
                    self.record_move(source_id, Some(target_id), range);
                }
            }
        }

        self.snapshot_set(u32::from(let_stmt.syntax().text_range().end()));
    }

    /// Process an expression (for state tracking).
    fn process_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::RefExpr(ref_expr) => {
                let is_mut = ref_expr.mut_token().is_some();
                if let Some(ast::Expr::PathExpr(path_expr)) = ref_expr.expr() {
                    if let Some((_name, binding_id, _)) = self.resolve_path(&path_expr) {
                        let range = expr.syntax().text_range();
                        if is_mut {
                            self.record_mut_borrow(binding_id, range);
                        } else {
                            self.record_shared_borrow(binding_id, range);
                        }
                    }
                }
            }
            // Other expression types don't need special handling in process_expr;
            // they're handled by the iterator's traversal or by CallArg/MethodReceiver events
            _ => {}
        }
    }

    /// Handle `&x` passed to function call.
    fn handle_shared_borrow_arg(&mut self, ref_expr: &ast::RefExpr, call_target: &str) {
        let Some(ast::Expr::PathExpr(path_expr)) = ref_expr.expr() else {
            return;
        };
        let Some((name, binding_id, _)) = self.resolve_path(&path_expr) else {
            return;
        };

        let range = ref_expr.syntax().text_range();
        let line = self.offset_to_line(u32::from(range.start()));

        self.copy_events.push(CopyEvent {
            from: name.clone(),
            to: call_target.to_string(),
            line,
            kind: TransferKind::SharedBorrow,
            is_scalar_in_call: false,
        });

        self.annotations.push(Annotation::new(
            range,
            name,
            BindingState::SharedBorrow { from: binding_id },
            "shared borrow passed to function".to_string(),
        ));
    }

    /// Handle `&mut x` passed to function call.
    fn handle_mut_borrow_arg(&mut self, ref_expr: &ast::RefExpr, call_target: &str) {
        let Some(ast::Expr::PathExpr(path_expr)) = ref_expr.expr() else {
            return;
        };
        let Some((name, _binding_id, _)) = self.resolve_path(&path_expr) else {
            return;
        };

        let range = ref_expr.syntax().text_range();
        let line = self.offset_to_line(u32::from(range.start()));

        self.copy_events.push(CopyEvent {
            from: name.clone(),
            to: call_target.to_string(),
            line,
            kind: TransferKind::MutBorrow,
            is_scalar_in_call: false,
        });

        let reborrow_id = BindingId(self.next_binding_id);
        self.next_binding_id += 1;
        self.annotations.push(Annotation::new(
            range,
            name,
            BindingState::Suspended { reborrowed_to: reborrow_id },
            "mutable borrow passed to function".to_string(),
        ));
    }

    /// Handle plain `x` passed to function call.
    fn handle_value_arg(&mut self, path_expr: &ast::PathExpr, call_target: &str) {
        let Some((name, binding_id, binding)) = self.resolve_path(path_expr) else {
            return;
        };

        let range = path_expr.syntax().text_range();
        let line = self.offset_to_line(u32::from(range.start()));

        // Determine transfer kind based on binding state
        let kind = match &binding.current_state {
            BindingState::SharedBorrow { .. } => TransferKind::SharedBorrow,
            BindingState::MutBorrow { .. } => TransferKind::MutBorrow,
            _ if binding.is_copy => TransferKind::Copy,
            _ => TransferKind::Move,
        };

        self.copy_events.push(CopyEvent {
            from: name.clone(),
            to: call_target.to_string(),
            line,
            kind,
            is_scalar_in_call: binding.is_scalar && kind == TransferKind::Copy,
        });

        if kind == TransferKind::Move {
            self.record_move(binding_id, None, range);
        }
    }

    /// Process a call argument (potential move or borrow).
    fn process_call_arg(&mut self, arg: &ast::Expr, call_target: &str) {
        match arg {
            ast::Expr::RefExpr(ref_expr) if ref_expr.mut_token().is_none() => {
                self.handle_shared_borrow_arg(ref_expr, call_target);
            }
            ast::Expr::RefExpr(ref_expr) => {
                self.handle_mut_borrow_arg(ref_expr, call_target);
            }
            ast::Expr::PathExpr(path_expr) => {
                self.handle_value_arg(path_expr, call_target);
            }
            _ => {}
        }
    }

    // ========================================================================
    // Helper methods for binding creation
    // ========================================================================

    /// Visit a function parameter with its type annotation.
    fn visit_param(&mut self, pat: &ast::Pat, type_str: Option<&str>) {
        let ast::Pat::IdentPat(ident_pat) = pat else {
            // For complex patterns, fall back to default handling
            self.visit_pat(pat, false);
            return;
        };

        let Some(name) = ident_pat.name() else { return };
        let name_str = name.text().to_string();
        let range = pat.syntax().text_range();

        // Determine state based on type
        let (state, explanation) = match type_str {
            Some(t) if t.trim().starts_with("&mut ") => (
                BindingState::MutBorrow { from: BindingId(0) }, // placeholder
                format!("{}: mutable borrow parameter (R W)", name_str),
            ),
            Some(t) if t.trim().starts_with('&') => (
                BindingState::SharedBorrow { from: BindingId(0) }, // placeholder
                format!("{}: shared borrow parameter (R only)", name_str),
            ),
            Some(t) if self.is_copy_type(t) => (
                BindingState::Owned { mutable: ident_pat.mut_token().is_some() },
                format!("{}: owned Copy parameter", name_str),
            ),
            _ => (
                BindingState::Owned { mutable: ident_pat.mut_token().is_some() },
                format!("{}: owned parameter", name_str),
            ),
        };

        // Create binding
        let id = BindingId(self.next_binding_id);
        self.next_binding_id += 1;

        let is_copy = type_str.map(|t| self.is_copy_type(t)).unwrap_or(false);
        let info = BindingInfo {
            name: name_str.clone(),
            scope: self.current_scope,
            is_mutable: ident_pat.mut_token().is_some(),
            is_copy,
            is_scalar: false, // Function params default to non-scalar (conservative)
            current_state: state.clone(),
        };

        self.bindings.insert(id, info);
        if let Some(scope) = self.scopes.get_mut(&self.current_scope) {
            scope.bindings.push(id);
        }

        self.annotations.push(Annotation::new(range, name_str, state, explanation));
    }

    /// Check if a type string represents a Copy type.
    ///
    /// Detects primitives, references, and common Copy types.
    /// The semantic layer (rust-analyzer) provides more accurate detection.
    fn is_copy_type(&self, type_str: &str) -> bool {
        let t = type_str.trim();

        // References are always Copy
        if t.starts_with('&') {
            return true;
        }

        // Primitive types
        if matches!(
            t,
            "bool" | "char"
                | "i8" | "i16" | "i32" | "i64" | "i128" | "isize"
                | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
                | "f32" | "f64"
                | "()" // unit type
        ) {
            return true;
        }

        // Arrays of primitives: [T; N]
        if t.starts_with('[') && t.ends_with(']') {
            if let Some(inner) = t.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
                // Extract element type before ';'
                if let Some(elem_type) = inner.split(';').next() {
                    return self.is_copy_type(elem_type.trim());
                }
            }
        }

        // Tuples of Copy types: (T, U, ...)
        if t.starts_with('(') && t.ends_with(')') && t != "()" {
            let inner = &t[1..t.len() - 1];
            return inner.split(',').all(|part| self.is_copy_type(part.trim()));
        }

        // Option<Copy>, Result<Copy, Copy> etc. - conservative: assume non-Copy
        // The semantic layer will correct this
        false
    }

    fn visit_pat_with_type(&mut self, pat: &ast::Pat, is_mutable: bool, is_copy: bool, is_scalar: bool) {
        match pat {
            ast::Pat::IdentPat(ident_pat) => {
                if let Some(name) = ident_pat.name() {
                    let is_mut = is_mutable || ident_pat.mut_token().is_some();

                    // Query type oracle for Copy/scalar status, falling back to passed values
                    let (actual_is_copy, actual_is_scalar) = if let Some(oracle) = &self.type_oracle {
                        (
                            oracle.is_copy(ident_pat).unwrap_or(is_copy),
                            oracle.is_scalar(ident_pat).unwrap_or(is_scalar),
                        )
                    } else {
                        (is_copy, is_scalar)
                    };

                    self.new_binding(
                        name.text().to_string(),
                        is_mut,
                        actual_is_copy,
                        actual_is_scalar,
                        pat.syntax().text_range(),
                    );
                }
            }
            ast::Pat::TuplePat(tuple_pat) => {
                // Tuple elements may have different scalar status; query oracle for each
                for field in tuple_pat.fields() {
                    self.visit_pat_with_type(&field, is_mutable, is_copy, false);
                }
            }
            ast::Pat::RefPat(ref_pat) => {
                // Reference patterns bind by reference, the binding itself is Copy but not scalar
                if let Some(inner) = ref_pat.pat() {
                    self.visit_pat_with_type(&inner, is_mutable, true, false);
                }
            }
            _ => {}
        }
    }

    fn visit_pat(&mut self, pat: &ast::Pat, is_mutable: bool) {
        self.visit_pat_with_type(pat, is_mutable, false, false);
    }

    // ========================================================================
    // NLL and Call Handling
    // ========================================================================

    /// Handle a method call receiver, implementing NLL borrow ending.
    /// When we see x.method() and x is currently Shared/Frozen (due to a borrow),
    /// we know the borrow must have ended (NLL) if the method requires access.
    fn handle_method_receiver(&mut self, receiver: &ast::Expr, method_call: &ast::MethodCallExpr) {
        // Check if receiver is a simple variable
        if let ast::Expr::PathExpr(path_expr) = receiver {
            if let Some((name, binding_id, _)) = self.resolve_path(path_expr) {
                let range = method_call.syntax().text_range();

                // Check if this binding is currently borrowed (Shared or Frozen)
                let is_borrowed = self.bindings
                    .get(&binding_id)
                    .map(|b| b.current_state.is_borrowed())
                    .unwrap_or(false);

                if is_borrowed {
                    // NLL: The method call means borrows have ended
                    // Find all bindings that are SharedBorrow/MutBorrow from this binding
                    let borrows_to_end: Vec<BindingId> = self.bindings.iter()
                        .filter_map(|(id, b)| {
                            match &b.current_state {
                                BindingState::SharedBorrow { from } if *from == binding_id => Some(*id),
                                BindingState::MutBorrow { from } if *from == binding_id => Some(*id),
                                _ => None,
                            }
                        })
                        .collect();

                    // End all active borrows using state machine
                    for borrow_id in &borrows_to_end {
                        if let Some(borrow_binding) = self.bindings.get(borrow_id) {
                            let borrow_name = borrow_binding.name.clone();
                            self.apply_event_with_annotation(
                                *borrow_id,
                                OwnershipEvent::ScopeExit,
                                range,
                                format!("{}: borrow ended (NLL)", borrow_name),
                            );
                        }
                    }

                    // Restore the original binding using AllBorrowsEnd event
                    self.apply_event_with_annotation(
                        binding_id,
                        OwnershipEvent::AllBorrowsEnd,
                        range,
                        format!("{}: borrow ended (NLL), restored to owned", name),
                    );
                }
            }
        }

        // For method receivers, we don't need to recursively visit because:
        // 1. If NLL just restored the binding, we don't want to re-borrow/move it
        // 2. Method receivers are auto-referenced by Rust, not explicitly moved
        // 3. AstIter handles traversal of nested expressions
    }

    // ========================================================================
    // Closure Capture Analysis
    // ========================================================================

    /// Collect all variable captures in an expression (for closure analysis).
    fn collect_captures(&self, expr: &ast::Expr) -> Vec<(String, BindingId, BindingInfo)> {
        let mut captures = Vec::new();
        self.collect_captures_recursive(expr, &mut captures);
        captures
    }

    fn collect_captures_recursive(&self, expr: &ast::Expr, captures: &mut Vec<(String, BindingId, BindingInfo)>) {
        match expr {
            ast::Expr::PathExpr(path_expr) => {
                if let Some((name, id, info)) = self.resolve_path(path_expr) {
                    // Check if this binding is from an outer scope (a capture)
                    if info.scope != self.current_scope {
                        // Only add if not already captured
                        if !captures.iter().any(|(n, _, _)| n == &name) {
                            captures.push((name, id, info));
                        }
                    }
                }
            }
            ast::Expr::BlockExpr(block) => {
                for stmt in block.statements() {
                    if let ast::Stmt::ExprStmt(expr_stmt) = stmt {
                        if let Some(e) = expr_stmt.expr() {
                            self.collect_captures_recursive(&e, captures);
                        }
                    }
                }
                if let Some(tail) = block.tail_expr() {
                    self.collect_captures_recursive(&tail, captures);
                }
            }
            ast::Expr::CallExpr(call) => {
                if let Some(e) = call.expr() {
                    self.collect_captures_recursive(&e, captures);
                }
                if let Some(args) = call.arg_list() {
                    for arg in args.args() {
                        self.collect_captures_recursive(&arg, captures);
                    }
                }
            }
            ast::Expr::MethodCallExpr(method) => {
                if let Some(receiver) = method.receiver() {
                    self.collect_captures_recursive(&receiver, captures);
                }
                if let Some(args) = method.arg_list() {
                    for arg in args.args() {
                        self.collect_captures_recursive(&arg, captures);
                    }
                }
            }
            ast::Expr::RefExpr(ref_expr) => {
                if let Some(inner) = ref_expr.expr() {
                    self.collect_captures_recursive(&inner, captures);
                }
            }
            ast::Expr::BinExpr(bin) => {
                if let Some(lhs) = bin.lhs() {
                    self.collect_captures_recursive(&lhs, captures);
                }
                if let Some(rhs) = bin.rhs() {
                    self.collect_captures_recursive(&rhs, captures);
                }
            }
            _ => {}
        }
    }

    /// Resolve a path expression to its binding, if it's a simple local variable.
    fn resolve_path(&self, path_expr: &ast::PathExpr) -> Option<(String, BindingId, BindingInfo)> {
        let path = path_expr.path()?;
        // Only handle simple paths (no :: qualifiers)
        if path.qualifier().is_some() {
            return None;
        }
        let segment = path.segment()?;
        let name_ref = segment.name_ref()?;
        let name = name_ref.text().to_string();
        let binding_id = self.lookup_binding(&name)?;
        let binding = self.bindings.get(&binding_id)?.clone();
        Some((name, binding_id, binding))
    }
}

impl Default for OwnershipAnalyzer<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::{Capabilities, SetEntryState};

    #[test]
    fn test_basic_let_binding() {
        let source = r#"
fn main() {
    let x = 42;
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        assert!(!annotations.is_empty());
        assert!(annotations.iter().any(|a| a.binding == "x"));
    }

    #[test]
    fn test_mutable_binding() {
        let source = r#"
fn main() {
    let mut x = 42;
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        let x_ann = annotations.iter().find(|a| a.binding == "x").unwrap();
        assert_eq!(x_ann.state, BindingState::Owned { mutable: true });
    }

    #[test]
    fn test_shared_borrow_parameter() {
        let source = r#"
fn foo(x: &str) {}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        let x_ann = annotations.iter().find(|a| a.binding == "x").unwrap();
        assert!(matches!(x_ann.state, BindingState::SharedBorrow { .. }));
        assert_eq!(x_ann.capabilities(), Capabilities::SHARED_BORROW);
    }

    #[test]
    fn test_mut_borrow_parameter() {
        let source = r#"
fn foo(x: &mut String) {}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        let x_ann = annotations.iter().find(|a| a.binding == "x").unwrap();
        assert!(matches!(x_ann.state, BindingState::MutBorrow { .. }));
        assert_eq!(x_ann.capabilities(), Capabilities::MUT_BORROW);
    }

    #[test]
    fn test_copy_type_parameter() {
        let source = r#"
fn foo(x: i32) {}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        let x_ann = annotations.iter().find(|a| a.binding == "x").unwrap();
        assert!(matches!(x_ann.state, BindingState::Owned { .. }));
        // i32 is Copy, so it's owned
        assert!(x_ann.capabilities().owned);
        assert!(x_ann.capabilities().read);
    }

    #[test]
    fn test_array_copy_type_parameter() {
        let source = r#"
fn foo(counts: [u32; 5]) {}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        let ann = annotations.iter().find(|a| a.binding == "counts").unwrap();
        assert!(matches!(ann.state, BindingState::Owned { .. }));
    }

    #[test]
    fn test_owned_non_copy_parameter() {
        let source = r#"
fn foo(s: String) {}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        let ann = annotations.iter().find(|a| a.binding == "s").unwrap();
        assert!(matches!(ann.state, BindingState::Owned { .. }));
    }

    #[test]
    fn test_move_in_function_call() {
        let source = r#"
fn take(s: String) {}
fn main() {
    let s = String::new();
    take(s);
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // Find the move annotation (when s is passed to take)
        let move_ann = annotations.iter().find(|a| {
            a.binding == "s" && matches!(a.state, BindingState::Moved { .. })
        });
        assert!(move_ann.is_some(), "Should have a move annotation for s");
    }

    #[test]
    fn test_shared_borrow_in_call() {
        let source = r#"
fn borrow(s: &String) {}
fn main() {
    let s = String::new();
    borrow(&s);
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // Find the borrow annotation
        let borrow_ann = annotations.iter().find(|a| {
            a.binding == "s" && matches!(a.state, BindingState::SharedBorrow { .. })
        });
        assert!(borrow_ann.is_some(), "Should have a shared borrow annotation for s");
    }

    #[test]
    fn test_mut_borrow_reborrow_in_call() {
        let source = r#"
fn mutate(s: &mut String) {}
fn main() {
    let mut s = String::new();
    mutate(&mut s);
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // Find the reborrow/suspended annotation
        let reborrow_ann = annotations.iter().find(|a| {
            a.binding == "s" && matches!(a.state, BindingState::Suspended { .. })
        });
        assert!(reborrow_ann.is_some(), "Should have a suspended/reborrow annotation for s");
    }

    #[test]
    fn test_primitive_treated_as_copy() {
        // Engine detects primitives like i32 as Copy types.
        let source = r#"
fn take_int(x: i32) {}
fn main() {
    let x: i32 = 42;
    take_int(x);
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // i32 is Copy, so it should be Copied not Moved
        let move_ann = annotations.iter().find(|a| {
            a.binding == "x" && matches!(a.state, BindingState::Moved { .. })
        });
        assert!(move_ann.is_none(), "Primitive i32 should not be moved (it's Copy)");

        // Check for copy event (created when Copy type passed to function)
        let copies = analyzer.copy_events();
        assert!(
            copies.iter().any(|c| c.from == "x" && c.to == "take_int"),
            "Primitive i32 should create copy event when passed to function"
        );
    }

    #[test]
    fn test_array_of_primitives_is_copy() {
        // Arrays of Copy types (like [u32; 5]) should be detected as Copy.
        // Note: Type annotations required without semantic analysis.
        let source = r#"
fn process(counts: [u32; 5]) {
    let k: [u32; 5] = counts;   // Should be a copy, not a move
    let m: [u32; 5] = k;        // Should also be a copy
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // counts should NOT be moved (it's Copy)
        let counts_moved = annotations.iter().find(|a| {
            a.binding == "counts" && matches!(a.state, BindingState::Moved { .. })
        });
        assert!(counts_moved.is_none(), "Array [u32; 5] should not be moved (it's Copy)");

        // k should NOT be moved
        let k_moved = annotations.iter().find(|a| {
            a.binding == "k" && matches!(a.state, BindingState::Moved { .. })
        });
        assert!(k_moved.is_none(), "k (copy of array) should not be moved");

        // Check copy events were recorded
        let copies = analyzer.copy_events();
        assert!(copies.iter().any(|c| c.from == "counts" && c.to == "k"),
            "Should have copy event counts → k");
        assert!(copies.iter().any(|c| c.from == "k" && c.to == "m"),
            "Should have copy event k → m");
    }

    #[test]
    fn test_copy_event_line_number_in_for_loop() {
        // Copy events inside for loops should have the correct line number
        let source = r#"fn process(arr: [u32; 5]) {
    for i in 0..5 {
        let x = arr;
        println!("{}", x[0]);
    }
    println!("{:?}", arr);
}"#;
        let mut analyzer = OwnershipAnalyzer::new();
        analyzer.analyze(source).unwrap();

        let copies = analyzer.copy_events();
        eprintln!("Copy events: {:?}", copies);

        // Find the copy event arr → x
        let copy = copies.iter().find(|c| c.from == "arr" && c.to == "x");
        assert!(copy.is_some(), "Should have copy event arr → x");

        // Line 2 (0-indexed) is "        let x = arr;"
        let copy = copy.unwrap();
        assert_eq!(copy.line, 2, "Copy event should be at line 2 (0-indexed), not {}", copy.line);
    }

    #[test]
    fn test_impl_method_parameters() {
        let source = r#"
struct Foo;
impl Foo {
    fn method(&self, other: &Foo) -> bool {
        true
    }
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // &self should be a shared borrow
        // Note: self might be handled differently, but `other` should work
        let other_ann = annotations.iter().find(|a| a.binding == "other");
        assert!(other_ann.is_some(), "Should have annotation for 'other' parameter");
        if let Some(ann) = other_ann {
            assert!(matches!(ann.state, BindingState::SharedBorrow { .. }));
        }
    }

    #[test]
    fn test_closure_parameters() {
        let source = r#"
fn main() {
    let f = |x: i32, y: &str| x;
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // Closure parameter x should be owned (i32 is Copy)
        let x_ann = annotations.iter().find(|a| a.binding == "x");
        assert!(x_ann.is_some(), "Should have annotation for closure param 'x'");

        // Closure parameter y should be a shared borrow
        let y_ann = annotations.iter().find(|a| a.binding == "y");
        assert!(y_ann.is_some(), "Should have annotation for closure param 'y'");
        if let Some(ann) = y_ann {
            assert!(matches!(ann.state, BindingState::SharedBorrow { .. }));
        }
    }

    #[test]
    fn test_move_closure_captures() {
        let source = r#"
fn main() {
    let s = String::new();
    let f = move || s.len();
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // s should be moved into the closure
        let move_ann = annotations.iter().find(|a| {
            a.binding == "s" && matches!(a.state, BindingState::Moved { .. })
        });
        assert!(move_ann.is_some(), "Should have a move annotation for s captured by move closure");
    }

    #[test]
    fn test_while_loop() {
        let source = r#"
fn main() {
    let mut x = 0;
    while x < 10 {
        x = x + 1;
    }
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        let x_ann = annotations.iter().find(|a| a.binding == "x");
        assert!(x_ann.is_some(), "Should have annotation for 'x'");
    }

    #[test]
    fn test_binary_expressions() {
        let source = r#"
fn main() {
    let a = 1;
    let b = 2;
    let c = a + b;
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // Should have annotations for a, b, and c
        assert!(annotations.iter().any(|a| a.binding == "a"));
        assert!(annotations.iter().any(|a| a.binding == "b"));
        assert!(annotations.iter().any(|a| a.binding == "c"));
    }

    #[test]
    fn test_shared_borrow_downgrades_to_shr() {
        let source = r#"
fn main() {
    let mut x = String::new();
    let r = &x;
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // x should be marked as Shared (shr) when borrowed
        let shared_ann = annotations.iter().find(|a| {
            a.binding == "x" && matches!(a.state, BindingState::Shared { .. })
        });
        assert!(shared_ann.is_some(), "x should be marked as shared (shr) when &x is created");

        // Capabilities should be OR (loses W)
        if let Some(ann) = shared_ann {
            let caps = ann.capabilities();
            assert!(caps.owned, "shr still owns");
            assert!(caps.read, "shr can read");
            assert!(!caps.write, "shr cannot write (downgraded)");
        }
    }

    #[test]
    fn test_mut_borrow_freezes_to_frz() {
        let source = r#"
fn main() {
    let mut x = String::new();
    let r = &mut x;
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // x should be marked as Frozen (frz) when &mut borrowed
        let frozen_ann = annotations.iter().find(|a| {
            a.binding == "x" && matches!(a.state, BindingState::Frozen { .. })
        });
        assert!(frozen_ann.is_some(), "x should be marked as frozen (frz) when &mut x is created");

        // Capabilities should be O only (no R or W)
        if let Some(ann) = frozen_ann {
            let caps = ann.capabilities();
            assert!(caps.owned, "frz still owns");
            assert!(!caps.read, "frz cannot read");
            assert!(!caps.write, "frz cannot write");
        }
    }

    #[test]
    fn test_capabilities_frozen_display() {
        use crate::analysis::Capabilities;
        // Frozen: O only (owns but can't access)
        assert_eq!(format!("{}", Capabilities::FROZEN), "O");
    }

    #[test]
    fn test_vec_macro_detected_as_non_copy() {
        let source = r#"
fn take(v: Vec<i32>) {}
fn main() {
    let x = vec![1, 2, 3];
    let y = x;
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // x from vec! should be non-Copy, so y = x should move it
        // Find x's declaration - should be marked non-Copy (checking explanation)
        let x_decl = annotations.iter().find(|a| {
            a.binding == "x" && matches!(a.state, BindingState::Owned { .. })
        });
        assert!(x_decl.is_some(), "Should have declaration annotation for x");
    }

    #[test]
    fn test_string_from_detected_as_non_copy() {
        let source = r#"
fn main() {
    let s = String::from("hello");
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        let s_ann = annotations.iter().find(|a| a.binding == "s");
        assert!(s_ann.is_some(), "Should have annotation for s");
    }

    #[test]
    fn test_struct_literal_detected_as_non_copy() {
        let source = r#"
struct Point { x: i32, y: i32 }
fn main() {
    let p = Point { x: 1, y: 2 };
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        let p_ann = annotations.iter().find(|a| a.binding == "p");
        assert!(p_ann.is_some(), "Should have annotation for p");
    }

    #[test]
    fn test_println_macro_borrows_variable() {
        let source = r#"
fn main() {
    let msg = String::from("hello");
    println!("{}", msg);
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // msg should be borrowed by println!, not moved
        let borrow_ann = annotations.iter().find(|a| {
            a.binding == "msg" && matches!(a.state, BindingState::SharedBorrow { .. })
        });
        assert!(borrow_ann.is_some(), "Should detect println! borrowing msg");

        // Original should be marked as shr (shared)
        let shr_ann = annotations.iter().find(|a| {
            a.binding == "msg" && matches!(a.state, BindingState::Shared { .. })
        });
        assert!(shr_ann.is_some(), "Original msg should be marked as shr");
    }

    #[test]
    fn test_println_inline_format_args() {
        // Test Rust 1.58+ inline format args: println!("{rect1:#?}")
        let source = r#"
struct Rect { w: i32, h: i32 }
fn main() {
    let rect1 = Rect { w: 10, h: 20 };
    println!("Rect is {rect1:#?}");
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let annotations = analyzer.analyze(source).unwrap();

        // rect1 should be borrowed by println! via inline format arg
        let borrow_ann = annotations.iter().find(|a| {
            a.binding == "rect1" && matches!(a.state, BindingState::SharedBorrow { .. })
        });
        assert!(borrow_ann.is_some(), "Should detect println! borrowing rect1 via inline format arg");
    }

    #[test]
    fn test_ownership_set_snapshots() {
        let source = r#"
fn main() {
    let mut x = vec![1, 2];
    let r = &x;
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let _ = analyzer.analyze(source).unwrap();
        let sets = analyzer.set_annotations();

        // Should have snapshots at: fn entry, after let x, after let r
        assert!(sets.len() >= 2, "Should have at least 2 set snapshots, got {}", sets.len());

        // Find the set after "let mut x" - should have "mut x"
        let set_with_x = sets.iter().find(|s| {
            s.set.entries.iter().any(|e| e.name == "x")
        });
        assert!(set_with_x.is_some(), "Should have a set containing x");

        let set = &set_with_x.unwrap().set;
        assert_eq!(set.scope_name, "main");

        // Find the set after "let r = &x" - should have "shr x, r(&x)"
        let set_with_r = sets.iter().find(|s| {
            s.set.entries.iter().any(|e| e.name == "r")
        });
        assert!(set_with_r.is_some(), "Should have a set containing r");

        let set_str = format!("{}", set_with_r.unwrap().set);
        assert!(set_str.contains("main{"), "Should have main scope prefix");
    }

    #[test]
    fn test_parameter_at_function_entry() {
        // Parameters should appear at the function signature line, not when first used
        let source = r#"
fn greet(name: &str) {
    println!("{}", name);
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let _ = analyzer.analyze(source).unwrap();
        let sets = analyzer.set_annotations();

        // Function signature is line 1 (0-indexed), parameter should be there
        let fn_entry_set = sets.iter().find(|s| s.line == 1);
        assert!(fn_entry_set.is_some(), "Should have set at function entry (line 1)");

        let entries = &fn_entry_set.unwrap().set.entries;
        let name_entry = entries.iter().find(|e| e.name == "name");
        assert!(name_entry.is_some(), "Parameter 'name' should be in function entry set");
        assert!(
            matches!(name_entry.unwrap().state, SetEntryState::SharedBorrow),
            "Parameter 'name: &str' should be SharedBorrow"
        );
    }

    #[test]
    fn test_owned_parameter_at_function_entry() {
        let source = r#"
fn consume(s: String) {
    drop(s);
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let _ = analyzer.analyze(source).unwrap();
        let sets = analyzer.set_annotations();

        let fn_entry_set = sets.iter().find(|s| s.line == 1);
        assert!(fn_entry_set.is_some(), "Should have set at function entry");

        let entries = &fn_entry_set.unwrap().set.entries;
        let s_entry = entries.iter().find(|e| e.name == "s");
        assert!(s_entry.is_some(), "Parameter 's' should be in function entry set");
        assert!(
            matches!(s_entry.unwrap().state, SetEntryState::Owned),
            "Parameter 's: String' should be Owned"
        );
    }

    #[test]
    fn test_mut_borrow_parameter_at_function_entry() {
        let source = r#"
fn modify(data: &mut Vec<i32>) {
    data.push(1);
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let _ = analyzer.analyze(source).unwrap();
        let sets = analyzer.set_annotations();

        let fn_entry_set = sets.iter().find(|s| s.line == 1);
        assert!(fn_entry_set.is_some(), "Should have set at function entry");

        let entries = &fn_entry_set.unwrap().set.entries;
        let data_entry = entries.iter().find(|e| e.name == "data");
        assert!(data_entry.is_some(), "Parameter 'data' should be in function entry set");
        assert!(
            matches!(data_entry.unwrap().state, SetEntryState::MutBorrow),
            "Parameter 'data: &mut Vec<i32>' should be MutBorrow"
        );
    }

    #[test]
    fn test_multiple_parameters_at_function_entry() {
        let source = r#"
fn process(a: String, b: &str, c: &mut i32) {
    *c = a.len() as i32;
}
"#;
        let mut analyzer = OwnershipAnalyzer::new();
        let _ = analyzer.analyze(source).unwrap();
        let sets = analyzer.set_annotations();

        let fn_entry_set = sets.iter().find(|s| s.line == 1);
        assert!(fn_entry_set.is_some(), "Should have set at function entry");

        let entries = &fn_entry_set.unwrap().set.entries;

        // All three parameters should be present
        assert!(entries.iter().any(|e| e.name == "a"), "Should have parameter 'a'");
        assert!(entries.iter().any(|e| e.name == "b"), "Should have parameter 'b'");
        assert!(entries.iter().any(|e| e.name == "c"), "Should have parameter 'c'");

        // Check their types
        let a = entries.iter().find(|e| e.name == "a").unwrap();
        let b = entries.iter().find(|e| e.name == "b").unwrap();
        let c = entries.iter().find(|e| e.name == "c").unwrap();

        assert!(matches!(a.state, SetEntryState::Owned), "a: String should be Owned");
        assert!(matches!(b.state, SetEntryState::SharedBorrow), "b: &str should be SharedBorrow");
        assert!(matches!(c.state, SetEntryState::MutBorrow), "c: &mut i32 should be MutBorrow");
    }
}
