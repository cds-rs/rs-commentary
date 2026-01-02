//! Copy type behavior.
//!
//! Types implementing `Copy` are duplicated instead of moved.
//! Key behaviors:
//! - Source remains valid after "move" (it's actually a copy)
//! - Both source and target are independent owners
//! - Includes primitives, arrays of Copy types, etc.

fn take(_x: usize) {}

fn for_loop_var_detected_as_copy() {
    let max_group = 5usize;
    for group_size in 1..=max_group {
        take(group_size);  // group_size is usize, should show as "copied"
    }
}

fn copy_preserves_source() {
    let x = 42i32;              //~ x: owned
    let y = x;                  //~ y: owned
    let _ = (x, y);             // x still valid after copy
}
