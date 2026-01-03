//! NLL (Non-Lexical Lifetimes) behavior.
//!
//! Rust ends borrows at last use, not scope end.
//! Key behaviors:
//! - Borrows end earlier than lexical scope
//! - Owner capabilities restore after borrow's last use
//! - Enables code that wouldn't compile with lexical lifetimes

fn shared_borrow_ends_before_scope() {
    let mut x = String::from("hi");   //~ x: owned_mut
    let r = &x;                       //~ r: ref_shared
                                      //~ x: shared
    let _ = r.len();
    // r's last use is above; x should be writable now
    x.push('!');                      //~ x: owned_mut
}

fn mutable_borrow_ends_before_scope() {
    let mut x = String::from("hi");   //~ x: owned_mut
    let m = &mut x;                   //~ m: ref_mut
                                      //~ x: frozen
    m.push('!');
    // m's last use is above; x should be unfrozen now
    x.push('?');                      //~ x: owned_mut
}
