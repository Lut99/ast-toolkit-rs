//  MOD.rs
//    by Lut99
//
//  Created:
//    07 Mar 2025, 17:11:09
//  Last edited:
//    07 Mar 2025, 17:15:12
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements [`snack`]-parsers for [`Punctuated`](crate::normal::Punctuated) and
//!   [`PunctuatedTrailing`](crate::trailing::PunctuatedTrailing).
//

// Declare the parser modules
pub mod punctuated_many0;
pub mod punctuated_many1;
pub mod punctuated_most0;
pub mod punctuated_most1;

// Use the combinator functions themselves
pub use punctuated_many0::punctuated_many0;
pub use punctuated_many1::punctuated_many1;
pub use punctuated_most0::punctuated_most0;
pub use punctuated_most1::punctuated_most1;
