//  MOD.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:21:07
//  Last edited:
//    02 Nov 2024, 11:49:27
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements complete versions of combinators that do UTF-8 specific
//!   parsing.
//!   
//!   Non-complete versions (i.e., versions that throw
//!   [`SnackError::NotEnough`](crate::result::SnackError::NotEnough) when they
//!   reach an unexpected EOF) are found in the [`streaming`](super::streaming)
//!   counterpart to this module.
//

// Declare modules
mod digit1;
mod tag;
mod while1;

// Use 'em
pub use digit1::*;
pub use tag::*;
pub use while1::*;
