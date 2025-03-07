//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 14:31:55
//  Last edited:
//    30 Nov 2024, 23:39:22
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines streaming versions of the C-combinators.
//!   
//!   This means that the combinators assume that more input may be given when
//!   needed. As such, the special
//!   [`SnackError::NotEnough`](crate::result::SnackError::NotEnough) is
//!   returned on unexpected end-of-files instead of a regular recoverable
//!   errors;
//

// Declare modules
pub mod escaped;

// Use some of that
pub use escaped::escaped;
