//  MAIN.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:33:52
//  Last edited:
//    01 Mar 2024, 15:26:06
//  Auto updated?
//    Yes
//
//  Description:
//!   To run some tests in
//


/***** ENTRYPOINT *****/
fn main() {
    let main: String = "hiya".into();
    let a: &str = &main[..2];
    let b: &str = &main[2..];
    println!("{:?} VS {:?}", a as *const str, b as *const str);
}
