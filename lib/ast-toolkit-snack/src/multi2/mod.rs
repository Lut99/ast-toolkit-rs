//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:04:24
//  Last edited:
//    01 Feb 2025, 13:48:16
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that will greedily try to parse as much as
//!   possible.
//

// Declare modules
pub mod many0;
pub mod many1;
pub mod most0;
pub mod most1;
pub mod separated_most0;
// pub mod separated_most1;
// pub mod separated_many0;
// pub mod separated_many1;

// Import some of it
pub use many0::many0;
pub use many1::many1;
pub use most0::most0;
pub use most1::most1;
pub use separated_most0::separated_most0;
// pub use separated_many0::separated_many0;
// pub use separated_many1::separated_many1;
// pub use separated_most1::separated_most1;
