//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 14:30:51
//  Last edited:
//    20 Mar 2025, 13:24:34
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines additional combinators that are less elementary, and more
//!   opiniated to specific languages.
//!
//!   For example, this includes combinators for parsing Rust-like integer
//!   literals or C-style escaped strings.
//

// Declare modules
pub mod escaped;

// Use the combinators
pub use escaped::escaped;
