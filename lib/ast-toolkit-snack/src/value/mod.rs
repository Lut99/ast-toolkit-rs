//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:31:45
//  Last edited:
//    24 Apr 2024, 10:45:39
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines combinators for matching input by its value.
//!   
//!   This defines all the "hands on" combinators, which actually touch the
//!   input stream instead of only other combinators.
//

// Declare submodules
pub mod bytes;
pub mod complete;
pub mod streaming;
pub mod utf8;
