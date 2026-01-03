//! Shared borrow (`&T`) behavior.
//!
//! Shared borrows allow read-only access while the owner retains ownership.
//! Key behaviors:
//! - Creates a `ref_shared` binding with R capability
//! - Downgrades owner to `shared` (writes blocked, reads allowed)
//! - Multiple shared borrows can coexist
//! - NLL: borrow ends at last use, restoring owner

fn shared_borrow_downgrades_owner() {
    let mut x = String::from("hi");   //~ x: owned_mut
    let r = &x;                       //~ r: ref_shared
                                      //~ x: shared
    let _ = r.len();
}

fn multiple_shared_borrows_allowed() {
    let x = String::from("hi");       //~ x: owned
    let r1 = &x;                      //~ r1: ref_shared
                                      //~ x: shared
    let r2 = &x;                      //~ r2: ref_shared
    let _ = (r1, r2);
}

fn shared_borrow_ends_then_write_allowed_nll() {
    let mut x = String::from("hi");   //~ x: owned_mut
    let r = &x;                       //~ r: ref_shared
                                      //~ x: shared
    let _ = r.len();
    // after last use of r, x should be writable again (NLL)
    x.push('!');                      //~ x: owned_mut
}
