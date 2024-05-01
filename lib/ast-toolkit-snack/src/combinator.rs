//  COMB.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:01:57
//  Last edited:
//    01 May 2024, 17:48:14
//  Auto updated?
//    Yes
//
//  Description:
//!   Some miscellaneous combinators that operate on other combinators.
//

use std::fmt::{Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};

use crate::error::{Common, Failure};
use crate::{Combinator, Expects, ExpectsFormatter, Result};


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
pub fn not<'c, F, S, C>(comb: C) -> Not<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
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
pub fn map<'c, F, S, R1, R2, C, M>(comb: C, func: M) -> Map<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R1>,
    M: FnMut(R1) -> R2,
{
    Map { comb, func, _f: Default::default(), _s: Default::default() }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Not`] combinator.
#[derive(Debug)]
pub struct NotExpects<'t> {
    /// The thing we _don't_ expect.
    fmt: Box<dyn 't + ExpectsFormatter>,
}
impl<'t> ExpectsFormatter for NotExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "not ")?;
        self.fmt.expects_fmt(f, indent)
    }
}

/// ExpectsFormatter for the [`Map`] combinator.
#[derive(Debug)]
pub struct MapExpects<'t> {
    /// The thing we expect.
    fmt: Box<dyn 't + ExpectsFormatter>,
}
impl<'t> ExpectsFormatter for MapExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult { self.fmt.expects_fmt(f, indent) }
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
impl<'t, F, S, C: Expects<'t>> Expects<'t> for Not<F, S, C> {
    type Formatter = NotExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { NotExpects { fmt: Box::new(self.comb.expects()) } }
}
impl<'c, F, S, C> Combinator<'c, F, S> for Not<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator<'c, F, S>,
    C::Output: Spanning<F, S>,
{
    type Output = ();

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        match self.comb.parse(input.clone()) {
            Result::Ok(_, res) => Result::Fail(Failure::Common(Common::Not { expects: Box::new(self.comb.expects()), span: res.span() })),
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
impl<'t, F, S, C: Expects<'t>, M> Expects<'t> for Map<F, S, C, M> {
    type Formatter = MapExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { MapExpects { fmt: Box::new(self.comb.expects()) } }
}
impl<'c, F, S, R1, R2, C, M> Combinator<'c, F, S> for Map<F, S, C, M>
where
    C: Combinator<'c, F, S, Output = R1>,
    M: FnMut(R1) -> R2,
{
    type Output = R2;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> Result<'c, Self::Output, F, S> {
        match self.comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, (self.func)(res)),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}
