//  MOD.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:21:07
//  Last edited:
//    30 Apr 2025, 09:20:00
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
pub mod digit1;
pub mod graph;
pub mod one_of1;
pub mod tag;
pub mod while1;
pub mod whitespace1;

// Use 'em
pub use digit1::digit1;
pub use graph::graph;
pub use one_of1::one_of1;
pub use tag::tag;
pub use while1::while1;
pub use whitespace1::whitespace1;
