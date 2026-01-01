//! Core analysis engine for tracking ownership state.
//!
//! - **Engine** (`engine.rs`): AST-based ownership analyzer
//! - **Semantic** (`semantic.rs`): rust-analyzer integration for accurate type info
//! - **State** (`state.rs`): Core state machine types
//! - **TypeOracle**: Trait for on-demand type queries (Copy detection, etc.)

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

/// Trait for on-demand type queries during ownership analysis.
///
/// This allows the ownership analyzer to query type information (e.g., is this type Copy?)
/// at the point of use, rather than pre-collecting and matching by offset.
///
/// When rust-analyzer is available, queries go directly to the type system.
/// For non-cargo projects, returns `None` to fall back to heuristics.
pub trait TypeOracle {
    /// Check if the binding in this pattern implements Copy.
    fn is_copy(&self, pat: &ast::IdentPat) -> Option<bool>;

    /// Check if the binding is a scalar primitive (i32, usize, bool, etc.).
    /// Used for noise filtering in function call annotations.
    fn is_scalar(&self, pat: &ast::IdentPat) -> Option<bool>;
}
