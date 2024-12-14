//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:04:24
//  Last edited:
//    14 Dec 2024, 18:44:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that will greedily try to parse as much as
//!   possible.
//

// Declare modules
pub mod few0;
pub mod few1;
pub mod many0;
pub mod many1;
// pub mod separated_list0;

// Import some of it
pub use few0::few0;
pub use few1::few1;
pub use many0::many0;
pub use many1::many1;
// pub use separated_list0::separated_list0;
