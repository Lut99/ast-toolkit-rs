//  PRECEDED.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 19:43:22
//  Last edited:
//    15 Dec 2024, 10:14:24
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`preceded()`]-combinator.
//

use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use super::pair::Pair;
use crate::Combinator2;
use crate::combinator2::map::Map;
use crate::result::Result as SResult;


/***** ERRORS *****/
/// Defines the recoverable errors thrown by [`Preceded`].
pub enum PrecededRecoverable<E1, E2, F, S> {
    /// The preceding combinataor has failed.
    Preceding { span: Span<F, S>, err: E1 },
    /// The main combinator has failed.
    Comb { span: Span<F, S>, err: E2 },
}
impl<E1: Debug, E2: Debug, F, S> Debug for PrecededRecoverable<E1, E2, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Preceding { span, err } => {
                let mut fmt = f.debug_struct("PrecededRecoverable::Preceding");
                fmt.field("span", span);
                fmt.field("err", err);
                fmt.finish()
            },
            Self::Comb { span, err } => {
                let mut fmt = f.debug_struct("PrecededRecoverable::Comb");
                fmt.field("span", span);
                fmt.field("err", err);
                fmt.finish()
            },
        }
    }
}
impl<E1, E2, F, S> Display for PrecededRecoverable<E1, E2, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {}
}





/***** COMBINATORS *****/
/// Actually implements the [`preceded()`]-combinator.
pub struct Preceded<C1, C2, F, S> {
    /// The left combinator, to discard.
    left:  C1,
    /// The right combinator, not to discard.
    right: C2,
    _f:    PhantomData<F>,
    _s:    PhantomData<S>,
}
impl<'t, C1, C2, F, S> Combinator2<'t, F, S> for Preceded<C1, C2, F, S>
where
    C1: Combinator2<'t, F, S>,
    C2: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = PrecededExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = C2::Output;
    type Recoverable = PrecededRecoverable<C1::Recoverable, C2::Recoverable>;
    type Fatal = PrecededFatal<C1::Fatal, C2::Fatal>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { PrecededExpectsFormatter { fmt1: self.left.expects(), fmt2: self.right.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> { todo!() }
}





/***** LIBRARY *****/
