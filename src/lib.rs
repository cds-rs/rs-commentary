//! # rs-commentary
//!
//! **Rust ownership state visualizer** — makes Rust's ownership system visible.
//!
//! rs-commentary annotates source code with ownership, borrowing, and move state
//! at each binding, showing what the borrow checker sees.
//!
//! ## Core Concept: Capabilities
//!
//! Every binding has three capabilities:
//!
//! | Capability | Meaning |
//! |------------|---------|
//! | **O** (Ownership) | Can transfer ownership (move or drop) |
//! | **R** (Read) | Can read the value |
//! | **W** (Write) | Can mutate the value |
//!
//! ## State Notation
//!
//! | State | Notation | Capabilities | Example |
//! |-------|----------|--------------|---------|
//! | Owned (mut) | `●●●` | O R W | `let mut x = val;` |
//! | Owned (immut) | `●●○` | O R | `let x = val;` |
//! | Frozen | `●○○` | O | `x` while `&mut x` exists |
//! | Shared borrow | `○●○` | R | `let r = &x;` |
//! | Mutable borrow | `○●●` | R W | `let r = &mut x;` |
//! | Moved/Dropped | `†` | ∅ | after move or scope end |
//!
//! ### Aliasing XOR Mutability
//!
//! When borrowed, the original binding's capabilities are temporarily reduced:
//!
//! - **Shared (`&x`)**: Original becomes read-only — can't mutate while alias exists
//! - **Mutable (`&mut x`)**: Original is frozen — exclusive access transferred to borrow
//!
//! ## Example Output
//!
//! ```text
//!    ╭─
//!  1 │ fn main() {
//!  2 │     let mut x = String::from("hello");
//!    │             ─
//!    │             └─ x: ●●● owned mut
//!    │
//!  3 │     let r = &x;
//!    │         ─    ──
//!    │         |    └─ x: ●●○ shared (borrowed)
//!    │         └─ r: ○●○ &x
//!    │
//!  4 │     println!("{}", r);
//!    │     └─ note: `r` dropped (last use)
//!    │
//!  5 │     x.push_str(" world");
//!    │     ─
//!    │     └─ x: ●●● owned mut (restored)
//!  6 │ }
//!    ╰─
//! ```
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │  Command Line Interface (CLI)                                │
//! │    rs-commentary annotate file.rs --style=diagnostic        │
//! ├─────────────────────────────────────────────────────────────┤
//! │  Output Layer (output/)                                     │
//! │    Pluggable renderers: diagnostic, inline, html, etc.      │
//! ├─────────────────────────────────────────────────────────────┤
//! │  Utilities (util/)                                          │
//! │    AstIter - lazy iterator over AST events                  │
//! │    StateTimeline - time-traveling state queries             │
//! ├─────────────────────────────────────────────────────────────┤
//! │  Analysis Engine (analysis/)                                │
//! │    OwnershipAnalyzer - AST visitor, state machine           │
//! │    SemanticAnalyzer - rust-analyzer integration             │
//! │    BindingState - state transitions                         │
//! ├─────────────────────────────────────────────────────────────┤
//! │  rust-analyzer (ra_ap_*)                                    │
//! │    Parsing, type inference, Copy detection, drop points     │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Usage
//!
//! ```bash
//! # Annotate with diagnostic style (default)
//! rs-commentary annotate file.rs
//!
//! # Other output styles
//! rs-commentary annotate file.rs --style=inline
//! rs-commentary annotate file.rs --style=html --serve
//!
//! # Include Copy types (hidden by default)
//! rs-commentary annotate file.rs --all
//! ```
//!
//! ## Requirements
//!
//! - Files must be part of a **Cargo project** (for rust-analyzer type inference)
//! - Analysis is **per-function** (no cross-function tracking)
//!
//! ## Modules
//!
//! - [`analysis`] - Core ownership analysis engine and state machine
//! - [`output`] - Rendering framework and output styles
//! - [`util`] - AST iteration, state timeline, position utilities
//! - [`execution`] - Function registry and call transfer tracking
//! - [`lsp`] - Language Server Protocol implementation

pub mod analysis;
pub mod execution;
pub mod lsp;
pub mod output;
pub mod testing;
pub mod util;

pub use analysis::{Annotation, BindingState, Capabilities};
pub use execution::{FunctionRegistry, FunctionView, CallTransfer, TransferKind};
pub use util::{offset_to_position, position_to_offset, StateTimeline, ChangeType, StateChange, VariableDrop};
