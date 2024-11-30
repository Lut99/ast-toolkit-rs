//  MOD.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 14:19:41
//  Last edited:
//    30 Nov 2024, 21:48:06
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators that relate to working with errors of other
//!   combinators.
//

// Modules
pub mod cut;
// pub mod fatal;
// pub mod recoverable;

// Use some of those things
pub use cut::cut;

// pub use fatal::fatal;
// pub use recoverable::recoverable;

// Re-export some things from the combinator module
pub use crate::combinator2::{map_fallible, map_recoverable};
