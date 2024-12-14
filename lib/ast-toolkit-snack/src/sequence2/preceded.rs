//  PRECEDED.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 19:43:22
//  Last edited:
//    14 Dec 2024, 19:47:10
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`preceded()`]-combinator.
//

use std::marker::PhantomData;

use super::pair::Pair;
use crate::Combinator2;
use crate::combinator2::map::Map;


/***** TYPE ALIASES *****/
/// Actually implements the [`preceded()`]-combinator.
pub struct Preceded<C1, C2, F, S> {
    /// The left combinator, to discard.
    left:  C1,
    /// The right combinator, not to discard.
    right: C2,
    _f:    PhantomData<F>,
    _s:    PhantomData<S>,
}





/***** LIBRARY *****/
