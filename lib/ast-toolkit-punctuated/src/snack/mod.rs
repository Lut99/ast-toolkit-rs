//  MOD.rs
//    by Lut99
//
//  Created:
//    07 Mar 2025, 17:11:09
//  Last edited:
//    12 Mar 2025, 13:47:04
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements [`snack`]-parsers for [`Punctuated`](crate::normal::Punctuated) and
//!   [`PunctuatedTrailing`](crate::trailing::PunctuatedTrailing).
//

// Declare the parser modules
pub mod punctuated_nontrailing_many0;
pub mod punctuated_nontrailing_many1;

// Use the combinator functions themselves
pub use punctuated_nontrailing_many0::punctuated_nontrailing_many0;
pub use punctuated_nontrailing_many1::punctuated_nontrailing_many1;
