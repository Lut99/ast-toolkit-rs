//  MOD.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:05:13
//  Last edited:
//    30 Nov 2024, 14:24:22
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements combinators for doing other combinators in sequence.
//

// Modules
pub mod pair;
pub mod tuple;
// pub mod preceded;
// pub mod terminated;
// pub mod delimited;
// pub mod separated_pair;

// Use the combinators
pub use pair::pair;
pub use tuple::tuple;
// pub use preceded::preceded;
// pub use terminated::terminated;
// pub use delimited::delimited;
// pub use separated_pair::separated_pair;
