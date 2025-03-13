//  MOD.rs
//    by Lut99
//
//  Created:
//    13 Mar 2025, 21:12:31
//  Last edited:
//    13 Mar 2025, 21:12:35
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements streaming versions of token parsers.
//

// Declare the submodules
pub mod utf8_delim;
pub mod utf8_token;

// Use some of it
pub use utf8_delim::utf8_delim;
pub use utf8_token::utf8_token;
