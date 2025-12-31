//! rs-commentary: Rust ownership state visualizer
//!
//! This library analyzes Rust source code and annotates it with
//! ownership, borrowing, and move state information at each line.

pub mod analysis;
pub mod execution;
pub mod lsp;
pub mod output;
pub mod util;

pub use analysis::{Annotation, BindingState, Capabilities};
pub use execution::{FunctionRegistry, FunctionView, CallTransfer, TransferKind};
pub use util::{offset_to_position, position_to_offset, StateTimeline, ChangeType, StateChange, VariableDrop};
