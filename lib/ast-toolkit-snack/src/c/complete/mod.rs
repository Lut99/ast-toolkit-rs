//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 14:31:33
//  Last edited:
//    30 Nov 2024, 23:00:45
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines complete versions of the C-combinators.
//!   
//!   This means that the combinators assume that what they see is what they
//!   get, and will throw regular recoverables when they see unexpected end-of-
//!   files.
//

// Declare modules
pub mod escaped;

// Use some of that
pub use escaped::escaped;
