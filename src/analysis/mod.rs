//! Core analysis engine for tracking ownership state.
//!
//! ## Layered Architecture
//!
//! - **Layer 1** (`layers.rs`): AST-based state machine, always available
//! - **Layer 2** (`oracle.rs`): Type queries via rust-analyzer
//! - **Composer** (`composer.rs`): Combines layers into unified analysis
//! - **Engine** (`engine.rs`): Original analyzer (kept for compatibility)

mod state;
mod engine;
mod visitor;
mod types;
mod semantic;
mod layers;
mod oracle;
mod composer;

pub use state::{
    Annotation, BindingId, BindingState, Capabilities, OwnershipEvent, OwnershipSet,
    Scope, SetAnnotation, SetEntry, SetEntryState, TransitionError,
};
pub use engine::OwnershipAnalyzer;
pub use semantic::{DiagnosticSeverity, LastUseInfo, RaDiagnostic, SemanticAnalyzer, SemanticResult};
pub use layers::{AstStateMachine, AstEvent, VarState, TypeOracle, CopyStatus};
pub use composer::{LayeredAnalyzer, NoOpOracle};
pub use oracle::{RaTypeOracle, TypeOracleQuery, HeuristicOracle};
