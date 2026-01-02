//! Ownership behavior specification tests.
//!
//! Each module is a concept, each function is a specific behavior.
//! Use `//~` comments to specify expected ownership states.

mod borrow_shared;
mod borrow_mut;
mod copy_types;
mod drop_nll;
mod move_semantics;

// Helper modules (not tests)
mod helpers;
