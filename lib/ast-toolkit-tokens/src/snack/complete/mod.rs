//  MOD.rs
//    by Lut99
//
//  Created:
//    12 Mar 2025, 13:53:13
//  Last edited:
//    13 Mar 2025, 20:38:06
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements complete versions of token parsers.
//

// Declare the submodules
pub mod utf8_delim;
pub mod utf8_token;

// Use some of it
pub use utf8_delim::utf8_delim;
pub use utf8_token::utf8_token;
