//! Copy type behavior.
//!
//! Types implementing `Copy` are duplicated instead of moved.
//! Key behaviors:
//! - Source remains valid after assignment (it's a copy, not a move)
//! - Both source and target are independent owners
//! - Includes primitives, arrays of Copy types, Copy structs

fn copy_scalar_does_not_move() {
    let x: u32 = 7;                   //~ x: owned
    let y = x;                        //~ y: owned
    let _ = x;                        //~ x: owned
}

fn copy_array_does_not_move() {
    let x = [1u32; 5];                //~ x: owned
    let y = x;                        //~ y: owned
    let _ = x;                        //~ x: owned
}

fn non_copy_struct_moves() {
    struct S(String);
    let x = S(String::from("hi"));    //~ x: owned
    let y = x;                        //~ y: owned
                                      //~ x: moved
    let _ = y;
    //~ !x
}

fn copy_into_call_does_not_invalidate_source() {
    fn takes(_: [u32; 5]) {}
    let x = [1u32; 5];                //~ x: owned
    takes(x);
    let _ = x;                        //~ x: owned
}
