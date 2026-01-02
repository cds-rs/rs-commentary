//! NLL (Non-Lexical Lifetimes) drop behavior.
//!
//! Rust drops values at last use, not scope end.
//! Key behaviors:
//! - Borrows end at last use, freeing the owner earlier
//! - Explicit `drop()` ends lifetime immediately
//! - Enables code that wouldn't compile with lexical lifetimes

fn drops_at_last_use() {
    let x = String::new();
    let y = &x;
    println!("{}", y);
    // y's borrow ends here (last use), not at scope end
    drop(x);
    println!("after drop");
}

fn borrow_ends_before_scope() {
    let x = String::new();      //~ x: owned
    let y = &x;                 //~ y: ref_shared, x: shared
    println!("{}", y);
    // After this point, y is no longer used, so x can be moved
}
