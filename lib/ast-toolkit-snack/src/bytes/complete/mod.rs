//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 14:26:20
//  Last edited:
//    30 Apr 2025, 08:59:57
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines complete versions of byte combinators.
//!   
//!   This means that the combinators assume that what they see is what they
//!   get, and will throw regular recoverables when they see unexpected end-of-
//!   files.
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
