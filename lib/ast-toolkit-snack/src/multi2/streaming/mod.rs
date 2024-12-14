//  MOD.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 17:54:51
//  Last edited:
//    14 Dec 2024, 18:03:48
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines streaming versions of the multi combinators.
//!   
//!   This means that the combinators assume that more input may be given when
//!   needed. As such, the special
//!   [`SnackError::NotEnough`](crate::result::SnackError::NotEnough) is
//!   returned on unexpected end-of-files instead of a regular recoverable
//!   errors;
//

// Modules
pub mod many1;
// pub mod separated_list1;

// Exports
pub use many1::many1;
// pub use separated_list1::separated_list1;
