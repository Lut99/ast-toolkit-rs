//  COMMON.rs
//    by Lut99
//
//  Created:
//    15 Mar 2024, 16:30:11
//  Last edited:
//    15 Mar 2024, 16:31:11
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements common helper- and auxillary structs and -interfaces.
//

use std::ops::{Deref, DerefMut};


/***** LIBRARY *****/
/// Represents an implicit index into the values of a punctuation.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ValueIndex(pub usize);
impl Deref for ValueIndex {
    type Target = usize;

    #[inline]
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl DerefMut for ValueIndex {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

/// Represents an implicit index into the punctuation of a punctuation.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PunctIndex(pub usize);
impl Deref for PunctIndex {
    type Target = usize;

    #[inline]
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl DerefMut for PunctIndex {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}
