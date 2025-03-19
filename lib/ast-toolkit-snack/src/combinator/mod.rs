//  MOD.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:21:02
//  Last edited:
//    19 Mar 2025, 10:45:45
//  Auto updated?
//    Yes
//
//  Description:
//!   Some miscellaneous combinators that operate on other combinators.
//

// Modules
pub mod closure;
pub mod consume;
pub mod discard;
pub mod map;
pub mod map_fallible;
pub mod map_fatal;
pub mod map_recoverable;
// pub mod nop;
// pub mod not;
// pub mod opt;
// pub mod peek;
// pub mod recognize;
// pub mod remember;

// Imports
pub use closure::closure;
pub use consume::consume;
pub use discard::discard;
pub use map::map;
pub use map_fallible::map_fallible;
pub use map_fatal::map_fatal;
pub use map_recoverable::map_recoverable;
// pub use nop::nop;
// pub use not::not;
// pub use opt::opt;
// pub use peek::peek;
// pub use recognize::recognize;
// pub use remember::remember;
