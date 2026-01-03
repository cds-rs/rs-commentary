//! Iterator item borrow behavior.
//!
//! Iterators yield borrows, not owned values (for iter/iter_mut).
//! Key behaviors:
//! - `iter()` yields `&T` (ref_shared), owner becomes shared
//! - `iter_mut()` yields `&mut T` (ref_mut), owner becomes frozen
//! - Owner is restored after loop ends

fn iter_yields_shared_borrow_item() {
    let a = [1u32; 2];                //~ a: owned
    for count in a.iter() {           //~ count: ref_shared, a: shared
        let _ = *count;
    }
    let _ = a;                        //~ a: owned
}

fn iter_mut_yields_mut_borrow_item() {
    let mut a = [1u32; 2];            //~ a: owned_mut
    for count in a.iter_mut() {       //~ count: ref_mut, a: frozen
        *count += 1;
    }
    a[0] = 42;                        //~ a: owned_mut
}
