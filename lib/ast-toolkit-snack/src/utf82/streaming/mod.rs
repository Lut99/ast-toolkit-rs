//  MOD.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:21:07
//  Last edited:
//    02 Nov 2024, 11:49:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements streaming versions of combinators that do UTF-8 specific
//!   parsing.
//!   
//!   Complete versions (i.e., versions that don't treat unexpected EOF
//!   specially) are found in the [`complete`](super::complete) counterpart to
//!   this module.
//

// Declare modules
mod digit1;
mod tag;
mod while1;

// Use 'em
pub use digit1::*;
pub use tag::*;
pub use while1::*;
