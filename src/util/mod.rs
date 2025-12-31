//! Shared utilities for rs-commentary.

mod position;
mod state;

pub use position::{offset_to_position, position_to_offset};
pub use state::{
    BoundaryState, ChangeType, LineState, StateChange, StateTimeline, StateTransition,
    TransitionReason, VariableDrop, VarSnapshot,
};
