//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:06:33
//  Last edited:
//    01 Dec 2024, 12:17:59
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that are used for debugging or inspecting the
//!   parsing process.
//

// Declare modules
pub mod inspect;
pub mod inspect_after;

// Use some of it
pub use inspect::inspect;
pub use inspect_after::inspect_after;