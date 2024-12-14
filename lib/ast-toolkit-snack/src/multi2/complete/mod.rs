//  MOD.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 17:54:28
//  Last edited:
//    14 Dec 2024, 18:03:43
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines complete versions of the multi combinators.
//!   
//!   This means that the combinators assume that what they see is what they
//!   get, and will throw regular recoverables when they see unexpected end-of-
//!   files.
//

// Modules
pub mod many1;
// pub mod separated_list1;

// Exports
pub use many1::many1;
// pub use separated_list1::separated_list1;
