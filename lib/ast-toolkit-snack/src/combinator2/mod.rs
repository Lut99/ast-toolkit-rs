//  MOD.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:21:02
//  Last edited:
//    03 Nov 2024, 19:38:40
//  Auto updated?
//    Yes
//
//  Description:
//!   Some miscellaneous combinators that operate on other combinators.
//

// Modules
pub mod consume;
pub mod discard;
pub mod map;
pub mod map_fallible;
pub mod map_fatal;
pub mod map_recoverable;
pub mod nop;
pub mod not;

// Imports
pub use consume::consume;
pub use discard::discard;
pub use map::map;
pub use map_fallible::map_fallible;
pub use map_fatal::map_fatal;
pub use map_recoverable::map_recoverable;
pub use nop::nop;
pub use not::not;
