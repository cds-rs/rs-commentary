mod allergies;

fn main() {
    let x = String::new();
    let y = &x;
    println!("{}", y);
    drop(x);
    println!("after drop");
}
