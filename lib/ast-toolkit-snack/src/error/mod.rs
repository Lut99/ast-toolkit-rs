//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 14:19:41
//  Last edited:
//    22 Apr 2025, 11:58:33
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that relate to working with errors of other
//!   combinators.
//

// Modules
pub mod cut;
pub mod expected;
pub mod fatal;
pub mod recoverable;
pub mod uncut;

// Use some of those things
pub use cut::cut;
pub use expected::expected;
pub use fatal::fatal;
pub use recoverable::recoverable;
pub use uncut::uncut;

// Re-export some things from the combinator module
pub use crate::combinator::{map_fallible, map_recoverable};
