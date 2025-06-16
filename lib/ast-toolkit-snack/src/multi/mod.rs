//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:04:24
//  Last edited:
//    24 Mar 2025, 11:49:41
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
pub mod separated_many0;
pub mod separated_many1;

// Import some of it
pub use many0::many0;
pub use many1::many1;
pub use separated_many0::separated_many0;
pub use separated_many1::separated_many1;
