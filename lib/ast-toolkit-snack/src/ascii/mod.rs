//  MOD.rs
//    by Lut99
//
//  Description:
//!   Defines some parsers specific for ASCII/single-byte UTF-8
//!   representations.
//!
//!   More more elaborate, full UTF-8 support, see the
//!   [`utf8`](super::utf8)-module.
//

// Declare the combinator modules
pub mod digit0;
pub mod digit1;
pub mod whitespace0;
pub mod whitespace1;

// Declare the combinators
pub use digit0::digit0;
pub use digit1::digit1;
pub use whitespace0::whitespace0;
pub use whitespace1::whitespace1;
