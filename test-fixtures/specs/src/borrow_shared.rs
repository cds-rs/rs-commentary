//! Shared borrow (`&T`) behavior.
//!
//! Shared borrows allow read-only access while the owner retains ownership.
//! Key behaviors:
//! - Creates a `ref_shared` binding
//! - Downgrades owner from `owned` to `shared` (loses write capability)
//! - Multiple shared borrows can coexist

fn downgrades_owner_to_shared() {
    let x = String::new();      //~ x: owned
    let y = &x;                 //~ y: ref_shared
                                //~ x: shared
    let _ = y;
}

fn multiple_shared_borrows_allowed() {
    let x = String::new();      //~ x: owned
    let y = &x;                 //~ y: ref_shared
                                //~ x: shared
    let z = &x;                 //~ z: ref_shared
    let _ = (y, z);
}
