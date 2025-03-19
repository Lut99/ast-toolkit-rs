//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:29
//  Last edited:
//    19 Mar 2025, 10:40:00
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines value combinators that are matching UTF-8 sequences.
//!   
//!   Note this doesn't necessarily mean they are matching on _strings_. can
//!   also recognize (some) UTF-8 sequences in possible-UTF8 byte input.
//

// Submodules
pub mod complete;
pub mod digit0;
pub mod one_of0;
pub mod streaming;
pub mod while0;
pub mod whitespace0;

// Use the combinator functions themselves
pub use digit0::digit0;
pub use one_of0::one_of0;
pub use while0::while0;
pub use whitespace0::whitespace0;
