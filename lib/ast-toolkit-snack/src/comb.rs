//  COMB.rs
//    by Lut99
//
//  Created:
//    05 Apr 2024, 18:01:57
//  Last edited:
//    05 Apr 2024, 19:21:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Some miscellaneous combinators that operate on other combinators.
//

use std::error;
use std::rc::Rc;

use ast_toolkit_span::{Span, Spanning};
use enum_debug::EnumDebug;

use crate::error::Error;
use crate::fail::Failure;
use crate::{Combinator, Result};


/***** AUXILLARY *****/
/// Helper enum that is almost [`Result`] but a bit smaller to make [`map_fallible()`] more convenient.
#[derive(Clone, Debug, EnumDebug)]
pub enum MapResult<R, F, E> {
    /// The result in the success case.
    Ok(R),
    /// An error occurred, but other parse paths may still recover this one.
    Fail(F),
    /// An error occurred that is unrecoverable.
    Error(E),
}
impl<R, F, E> MapResult<R, F, E> {
    /// Promotes this MapResult into a full [`Result`] by supplying a `rem`ainder span.
    ///
    /// # Arguments
    /// - `rem`: Some [`Span`] that is the rest of the source we hadn't parsed yet.
    ///
    /// # Returns
    /// A new [`Result`] that is the same as this one except that, if we're Self::Ok, this is [`Result::Ok`] _with_ the given `rem`.
    #[inline]
    fn promote<RF, RS>(self, rem: Span<RF, RS>) -> Result<R, RF, RS>
    where
        F: 'static + error::Error + Spanning<RF, RS>,
        E: 'static + error::Error + Spanning<RF, RS>,
    {
        match self {
            Self::Ok(res) => Result::Ok(rem, res),
            Self::Fail(fail) => Result::Fail(Failure::Custom { problem: Rc::new(fail) }),
            Self::Error(err) => Result::Error(Error::Custom { problem: Rc::new(err) }),
        }
    }
}





/***** LIBRARY *****/
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
pub fn not<F, S, C>(mut comb: C) -> impl FnMut(Span<F, S>) -> Result<(), F, S>
where
    F: Clone,
    S: Clone,
    C: Combinator<F, S>,
    C::Output: Spanning<F, S>,
{
    move |input: Span<F, S>| -> Result<(), F, S> {
        match comb.parse(input.clone()) {
            Result::Ok(_, res) => Result::Fail(Failure::Not { span: res.span() }),
            Result::Fail(_) => Result::Ok(input, ()),
            Result::Error(err) => Result::Error(err),
        }
    }
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
pub fn map<F, S, R1, R2, C, M>(mut comb: C, mut func: M) -> impl FnMut(Span<F, S>) -> Result<R2, F, S>
where
    C: Combinator<F, S, Output = R1>,
    M: FnMut(R1) -> R2,
{
    move |input: Span<F, S>| -> Result<R2, F, S> {
        match comb.parse(input) {
            Result::Ok(rem, res) => Result::Ok(rem, func(res)),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}

/// Maps the result of a combinator to something else.
///
/// This version doesn't just map to another success state, but in fact can just map to another failure or error.
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `func`: Some closure that takes the `comb`'s result and maps it to something else.
///
/// # Returns
/// A new closure that runs `comb`, then maps with `func` if `comb` succeeds.
///
/// # Fails
/// This combinator inherits the fail state of the given one, plus if `func` decides to fail.
///
/// # Errors
/// This combinator inherits the error state of the given one, plus if `func` decides to error.
pub fn map_fallible<F, S, R1, C, MF, MS, R2, M>(mut comb: C, mut func: M) -> impl FnMut(Span<F, S>) -> Result<R2, F, S>
where
    C: Combinator<F, S, Output = R1>,
    MF: 'static + error::Error + Spanning<F, S>,
    MS: 'static + error::Error + Spanning<F, S>,
    M: FnMut(R1) -> MapResult<R2, MF, MS>,
{
    move |input: Span<F, S>| -> Result<R2, F, S> {
        match comb.parse(input) {
            Result::Ok(rem, res) => func(res).promote(rem),
            Result::Fail(fail) => Result::Fail(fail),
            Result::Error(err) => Result::Error(err),
        }
    }
}