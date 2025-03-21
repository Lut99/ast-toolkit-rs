//  LIB.rs
//    by Lut99
//
//  Created:
//    26 Feb 2024, 14:01:57
//  Last edited:
//    07 Mar 2025, 17:12:37
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a wrapper around a [`Vec`] that allows it to be
//!   convenient for modelling punctuated lists in ASTs (e.g.,
//!   comma-separated list).
//

// Declare the modules
pub mod common;
#[cfg(feature = "normal")]
pub mod normal;
#[cfg(feature = "snack")]
pub mod snack;
#[cfg(feature = "trailing")]
pub mod trailing;

// Bring some of it into this namespace
pub use common::*;
#[cfg(feature = "normal")]
pub use normal::Punctuated;
#[cfg(feature = "trailing")]
pub use trailing::{NextValue, PunctuatedTrailing};
