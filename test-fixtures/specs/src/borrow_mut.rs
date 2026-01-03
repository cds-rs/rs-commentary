//! Mutable borrow (`&mut T`) behavior.
//!
//! Mutable borrows provide exclusive read-write access.
//! Key behaviors:
//! - Creates a `ref_mut` binding with R+W capabilities
//! - Freezes owner to `frozen` state (all access blocked)
//! - Only one mutable borrow allowed at a time
//! - NLL: borrow ends at last use, restoring owner

fn mutable_borrow_freezes_owner() {
    let mut x = String::from("hi");   //~ x: owned_mut
    let m = &mut x;                   //~ m: ref_mut
                                      //~ x: frozen
    m.push('!');
}

fn mutable_borrow_ends_then_owner_restored_nll() {
    let mut x = String::from("hi");   //~ x: owned_mut
    {
        let m = &mut x;               //~ m: ref_mut
                                      //~ x: frozen
        m.push('!');
    }
    x.push('?');                      //~ x: owned_mut
}

fn reborrow_through_call_does_not_consume() {
    fn takes_mut(_: &mut String) {}
    let mut x = String::from("hi");   //~ x: owned_mut
    let m = &mut x;                   //~ m: ref_mut
                                      //~ x: frozen
    takes_mut(m);
    m.push('!');                      //~ m: ref_mut
}
