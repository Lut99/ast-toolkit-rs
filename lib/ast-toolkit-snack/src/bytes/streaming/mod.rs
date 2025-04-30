//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 14:28:51
//  Last edited:
//    30 Apr 2025, 09:15:24
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines streaming versions of byte combinators.
//!   
//!   This means that the combinators assume that more input may be given when
//!   needed. As such, the special
//!   [`SnackError::NotEnough`](crate::result::SnackError::NotEnough) is
//!   returned on unexpected end-of-files instead of a regular recoverable
//!   errors;
//

// Declare modules
pub mod byte;
pub mod one_of1;
pub mod tag;
pub mod while1;

// Import some of that
pub use byte::byte;
pub use one_of1::one_of1;
pub use tag::tag;
pub use while1::while1;
