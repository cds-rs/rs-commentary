//! Move semantics behavior.
//!
//! Moves transfer ownership from source to target.
//! Key behaviors:
//! - Source becomes `moved` (no capabilities, cannot be used)
//! - Target becomes new owner
//! - Applies to non-Copy types only

fn invalidates_source() {
    let x = String::new();      //~ x: owned
    let y = x;                  //~ y: owned
                                //~ x: moved
    let _ = y;
}
