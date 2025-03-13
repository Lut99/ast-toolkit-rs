//  MOD.rs
//    by Lut99
//
//  Created:
//    09 Sep 2024, 15:26:17
//  Last edited:
//    13 Mar 2025, 21:10:12
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that integrates the tokens into the
//!   [`snack`](ast_toolkit_snack) parsing framework.
//

// Declare the two submodules
pub mod complete;
pub mod streaming;

// Re-export some snack stuff for the macros
pub use ast_toolkit_snack::{Combinator, span};
