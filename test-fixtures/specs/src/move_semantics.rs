//! Move semantics behavior.
//!
//! Moves transfer ownership from source to target.
//! Key behaviors:
//! - Source becomes `moved` (no capabilities, cannot be used)
//! - Target becomes new owner
//! - Applies to non-Copy types only
//! - `!x` asserts variable is no longer live

fn move_non_copy_invalidates_source() {
    let x = String::from("hi");       //~ x: owned
    let y = x;                        //~ y: owned
                                      //~ x: moved
    let _ = y;
    //~ !x
}

fn move_from_mut_owner_is_still_move() {
    let mut x = String::from("hi");   //~ x: owned_mut
    let y = x;                        //~ y: owned
                                      //~ x: moved
    let _ = y;
    //~ !x
}
