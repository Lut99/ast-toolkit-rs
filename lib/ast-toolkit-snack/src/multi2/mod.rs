//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:04:24
//  Last edited:
//    01 Dec 2024, 21:18:24
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that will greedily try to parse as much as
//!   possible.
//

// Declare modules
pub mod many0;
// TODO: Move to `complete` / `streaming` module
pub mod many1;
// pub mod many_n;
// pub mod separated_list0;
// pub mod separated_list1;
// pub mod separated_list_n;

// Import some of it
pub use many0::many0;
pub use many1::many1;
// pub use many_n::many_n;
// pub use separated_list0::separated_list0;
// pub use separated_list1::separated_list1;
// pub use separated_list_n::separated_list_n;
