//  MOD.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:05:13
//  Last edited:
//    14 Dec 2024, 18:14:32
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements combinators for doing other combinators in sequence.
//

// Modules
pub mod pair;
pub mod repeated;
pub mod tuple;
// pub mod preceded;
// pub mod terminated;
// pub mod delimited;
// pub mod separated_pair;

// Use the combinators
pub use pair::pair;
pub use repeated::repeated;
pub use tuple::tuple;
// pub use preceded::preceded;
// pub use terminated::terminated;
// pub use delimited::delimited;
// pub use separated_pair::separated_pair;
