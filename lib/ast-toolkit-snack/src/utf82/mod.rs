//  MOD.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 13:37:29
//  Last edited:
//    02 Nov 2024, 11:49:03
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
pub mod streaming;
