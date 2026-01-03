//! Core analysis engine for tracking ownership state.
//!
//! - **Engine** (`engine/`): Event-driven ownership analyzer
//! - **Semantic** (`semantic.rs`): rust-analyzer integration for type info and macro expansion
//! - **State** (`state.rs`): Core state machine types (BindingState, OwnershipEvent)
//! - **TypeOracle**: Single source of truth for type queries (Copy, scalar, macro expansion)

mod state;
mod engine;
mod semantic;

use ra_ap_syntax::ast;

pub use state::{
    Annotation, BindingId, BindingKind, BindingState, Capabilities, OwnershipEvent, OwnershipSet,
    Scope, SetAnnotation, SetEntry, SetEntryState, TransitionError,
};
pub use engine::{CopyEvent, OwnershipAnalyzer, TransferKind};
pub use semantic::{DiagnosticSeverity, LastUseInfo, RaDiagnostic, SemanticAnalyzer, SemanticResult, SemanticTypeOracle};

/// Single source of truth for type queries during ownership analysis.
///
/// TypeOracle provides on-demand type information at each AST node during traversal.
/// No heuristic fallbacks: if TypeOracle returns `None`, the analyzer uses conservative
/// defaults (assume non-Copy).
///
/// Implemented by [`SemanticTypeOracle`] using rust-analyzer's semantic analysis.
/// Requires a cargo project for accurate type resolution.
pub trait TypeOracle {
    /// Check if the binding in this pattern implements Copy.
    fn is_copy(&self, pat: &ast::IdentPat) -> Option<bool>;

    /// Check if the binding is a scalar primitive (i32, usize, bool, etc.).
    /// Used for noise filtering in function call annotations.
    fn is_scalar(&self, pat: &ast::IdentPat) -> Option<bool>;

    /// Get the binding kind (owned/copy/reference) for this pattern.
    /// Used for iterator variable tracking in for-loops.
    fn binding_kind(&self, pat: &ast::IdentPat) -> Option<BindingKind>;

    /// Expand a macro call and find variable references in the expanded code.
    ///
    /// Returns a list of variable names that are referenced in the macro expansion.
    /// For format macros (println!, format!, etc.), these are implicitly borrowed.
    ///
    /// Returns `None` if macro expansion is not available or fails.
    fn expand_macro_vars(&self, macro_call: &ast::MacroCall) -> Option<Vec<String>>;
}
