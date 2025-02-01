//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:04:24
//  Last edited:
//    01 Feb 2025, 13:06:13
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
pub mod separated_many0;

// Import some of it
pub use many0::many0;
pub use many1::many1;
pub use most0::most0;
pub use most1::most1;
pub use separated_many0::separated_many0;
