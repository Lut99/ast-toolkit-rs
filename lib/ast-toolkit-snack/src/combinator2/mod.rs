//  MOD.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:21:02
//  Last edited:
//    03 Nov 2024, 12:14:40
//  Auto updated?
//    Yes
//
//  Description:
//!   Some miscellaneous combinators that operate on other combinators.
//

// Modules
pub mod consume;
pub mod map;
pub mod map_fallible;
pub mod map_fatal;
pub mod map_recoverable;

// Imports
pub use consume::consume;
pub use map::map;
pub use map_fallible::map_fallible;
pub use map_fatal::map_fatal;
pub use map_recoverable::map_recoverable;
