//  COMB.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:01:57
//  Last edited:
//    25 Apr 2024, 17:33:35
//  Auto updated?
//    Yes
//
//  Description:
//!   Some miscellaneous combinators that operate on other combinators.
//

use std::fmt::{Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};

use crate::error_new::{expects_not, Common, Failure};
use crate::{Combinator, Expects, ExpectsExt as _, Result};


/***** LIBRARY FUNCTIONS *****/
/// Implements the reverse of a combinator.
///
/// Specifically, will return `Result::Ok(())` if the combinator [`Result::Fail`]s, or a [`Result::Fail`] if it [`Result::Ok`]'s.
///
/// # Arguments
/// - `comb`: The [`Combinator`] to negate.
///
/// # Returns
/// A new [`Combinator`] that will do the same as `comb` but reversed. Note that this combinator is the exact opposite of the given one!
///
/// Also note that errors are left untouched.
#[inline]
pub fn not<F, S, C>(comb: C) -> Not<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<F, S>,
    C::Output: Spanning<F, S>,
{
    Not { comb, _f: Default::default(), _s: Default::default() }
}



/// Maps the result of a combinator to something else.
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `func`: Some closure that takes the `comb`'s result and maps it to something else - including some kind of map error.
///
/// # Returns
/// A new closure that runs `comb`, then maps with `func` if `comb` succeeds.
///
/// # Fails
/// This combinator inherits the fail state of the given one.
///
/// # Errors
/// This combinator inherits the error state of the given one.
#[inline]
pub fn map<F, S, R1, R2, C, M>(comb: C, func: M) -> Map<F, S, C, M>
where
    C: Combinator<F, S, Output = R1>,
    M: FnMut(R1) -> R2,
{
    Map { comb, func, _f: Default::default(), _s: Default::default() }
}





/***** LIBRARY *****/
/// The concrete type returned by [`not()`].
pub struct Not<F, S, C> {
    /// The combinator to negate.
    comb: C,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<F, S, C: Expects> Expects for Not<F, S, C> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult { expects_not(f, indent, &self.comb.expects()) }
}
impl<F, S, C> Combinator<F, S> for Not<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<F, S>,
    C::Output: Spanning<F, S>,
{
    type Output = ();

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        match self.comb.parse(input.clone()) {
            Result::Ok(_, res) => Result::Fail(Failure::Common(Common::Not { expects: self.comb.expects(), span: res.span() })),
            Result::Fail(_) => Result::Ok(input, ()),
            Result::Error(err) => Result::Error(err),
        }
    }
}



/// The concrete type returned by [`map()`].
pub struct Map<F, S, C, M> {
    /// The combinator who's output to negate.
    comb: C,
    /// The mapping closure.
    func: M,
    /// The type of the `F`rom string, which is stored here to keep the link between combinator construction and parsing.
    _f:   PhantomData<F>,
    /// The type of the `S`ource string, which is stored here to keep the link between combinator construction and parsing.
    _s:   PhantomData<S>,
}
impl<F, S, C: Expects, M> Expects for Map<F, S, C, M> {
    #[inline]
    fn fmt(&self, f: &mut Formatter, indent: usize) -> FResult { self.comb.fmt(f, indent) }
}
impl<F, S, R1, R2, C, M> Combinator<F, S> for Map<F, S, C, M>
where
    C: Combinator<F, S, Output = R1>,
    M: FnMut(R1) -> R2,
{
    type Output = R2;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<Self::Output, F, S> {
        match self.comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, (self.func)(res)),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}
