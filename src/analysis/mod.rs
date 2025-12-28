//! Core analysis engine for tracking ownership state.
//!
//! - **Engine** (`engine.rs`): AST-based ownership analyzer
//! - **Semantic** (`semantic.rs`): rust-analyzer integration for accurate type info
//! - **State** (`state.rs`): Core state machine types

mod state;
mod engine;
mod semantic;

pub use state::{
    Annotation, BindingId, BindingState, Capabilities, OwnershipEvent, OwnershipSet,
    Scope, SetAnnotation, SetEntry, SetEntryState, TransitionError,
};
pub use engine::OwnershipAnalyzer;
pub use semantic::{DiagnosticSeverity, LastUseInfo, RaDiagnostic, SemanticAnalyzer, SemanticResult};
