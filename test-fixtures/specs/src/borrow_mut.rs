//! Mutable borrow (`&mut T`) behavior.
//!
//! Mutable borrows provide exclusive read-write access.
//! Key behaviors:
//! - Creates a `ref_mut` binding with R+W capabilities
//! - Freezes owner to `frozen` state (no access until borrow ends)
//! - Only one mutable borrow allowed at a time

fn freezes_owner() {
    let mut x = String::new();  //~ x: owned_mut
    let y = &mut x;             //~ y: ref_mut
                                //~ x: frozen
    let _ = y;
}
