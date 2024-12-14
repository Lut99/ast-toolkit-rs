//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:04:24
//  Last edited:
//    14 Dec 2024, 18:12:09
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that will greedily try to parse as much as
//!   possible.
//

// Declare modules
pub mod complete;
pub mod many0;
// pub mod separated_list0;
pub mod streaming;

// Import some of it
pub use many0::many0;
// pub use separated_list0::separated_list0;
