//! Function analysis for ownership visualization.
//!
//! This module provides per-function analysis, matching how the borrow checker
//! actually works (static analysis of each function independently).
//!
//! Key concepts:
//! - `FunctionRegistry`: Maps all callables (functions, methods, closures) to line ranges
//! - `FunctionView`: Per-function view for stepping through one function at a time
//! - `CallTransfer`: What's moved/borrowed at each call site

mod registry;
mod calls;
mod trace;
mod transfers;

pub use registry::{FunctionInfo, FunctionKind, FunctionRegistry};
pub use calls::{CallKind, CallSite, detect_calls};
pub use trace::FunctionView;
pub use transfers::{analyze_call_transfers, analyze_method_call_transfers, CallTransfer, TransferKind};
