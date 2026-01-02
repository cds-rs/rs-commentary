//! Move semantics behavior.
//!
//! Moves transfer ownership from source to target.
//! Key behaviors:
//! - Source becomes `moved` (no capabilities, cannot be used)
//! - Target becomes new owner
//! - Applies to non-Copy types only

fn invalidates_source_binding() {
    let x = String::new();      //~ x: owned
    let y = x;                  //~ y: owned, x: moved
    let _ = y;
}

fn target_becomes_owner() {
    let x = String::new();      //~ x: owned
    let y = x;                  //~ y: owned
    let _ = y;
}
