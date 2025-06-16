//  OPT.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 13:56:52
//  Last edited:
//    08 May 2025, 11:20:09
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`opt()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// Expectsformatter for the [`Opt`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<F> {
    /// The nested formatter of the thing we _didn'c_ expect.
    pub fmt: F,
}
impl<F: crate::ExpectsFormatter> Display for ExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<F: crate::ExpectsFormatter> crate::ExpectsFormatter for ExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "optionally ")?;
        self.fmt.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`opt()`]-combinator.
pub struct Opt<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for Opt<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Option<C::Output>;
    type Recoverable = Infallible;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match self.comb.parse(input.clone()) {
            Ok((rem, res)) => Ok((rem, Some(res))),
            Err(SnackError::Recoverable(_)) => Ok((input, None)),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(err)),
        }
    }
}





/***** LIBRARY *****/
/// Turns a combinator into one that is allowed to fail.
///
/// Specifically, will copy the given combinator's behaviour, except that any [`Result::Fail`]s
/// will simply cause `opt()` to return [`None`].
///
/// # Arguments
/// - `comb`: The [`Combinator`] to make optional.
///
/// # Returns
/// A combinator [`Opt`] that will either match the given `comb`, or return [`None`].
///
/// # Fails
/// The returned combinator never fails recoverably. However, if the nested combinator fails
/// fatally, this is propagated.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::opt;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = opt(tag(b"Hello"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), Some(span1.slice(..5)))));
/// assert_eq!(comb.parse(span2), Ok((span2, None)));
/// ```
#[inline]
pub const fn opt<'c, 's, C, S>(comb: C) -> Opt<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    Opt { comb, _s: PhantomData }
}
