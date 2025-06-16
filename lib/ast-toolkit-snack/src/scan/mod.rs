//  MOD.rs
//    by Lut99
//
//  Description:
//!   Defines combinators for matching specific sequences of elements.
//

// Declare modules
pub mod elem;
pub mod one_of0;
pub mod one_of1;
pub mod tag;
pub mod while0;
pub mod while1;

// Import some of that
pub use elem::elem;
pub use one_of0::one_of0;
pub use one_of1::one_of1;
pub use tag::tag;
pub use while0::while0;
pub use while1::while1;
