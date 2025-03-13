//  MOD.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:05:13
//  Last edited:
//    18 Jan 2025, 18:45:30
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements combinators for doing other combinators in sequence.
//

// Modules
pub mod delimited;
pub mod pair;
pub mod preceded;
pub mod repeated;
pub mod separated_pair;
pub mod terminated;
pub mod tuple;

// Use the combinators
pub use delimited::delimited;
pub use pair::pair;
pub use preceded::preceded;
pub use repeated::repeated;
pub use separated_pair::separated_pair;
pub use terminated::terminated;
pub use tuple::tuple;
