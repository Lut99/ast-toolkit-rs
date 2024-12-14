//  OPT.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 13:56:52
//  Last edited:
//    14 Dec 2024, 19:36:06
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`opt()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator2, ExpectsFormatter};


/***** FORMATTERS *****/
/// Expectsformatter for the [`Opt`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct OptExpectsFormatter<F> {
    /// The nested formatter of the thing we _didn't_ expect.
    pub fmt: F,
}
impl<F: ExpectsFormatter> Display for OptExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<F: ExpectsFormatter> ExpectsFormatter for OptExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "optionally ")?;
        self.fmt.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`opt()`]-combinator.
pub struct Opt<C, F, S> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator2<'t, F, S> for Opt<C, F, S>
where
    F: Clone,
    S: Clone,
    C: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = OptExpectsFormatter<C::ExpectsFormatter>;
    type Output = Option<C::Output>;
    type Recoverable = Infallible;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { OptExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        match self.comb.parse(input.clone()) {
            Ok((rem, res)) => Ok((rem, Some(res))),
            Err(SnackError::Recoverable(_)) => Ok((input, None)),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(err)),
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
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
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::combinator2::opt;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = opt(tag("Hello"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), Some(span1.slice(..5)))));
/// assert_eq!(comb.parse(span2), Ok((span2, None)));
/// ```
#[inline]
pub const fn opt<'t, C, F, S>(comb: C) -> Opt<C, F, S>
where
    F: Clone,
    S: Clone,
    C: Combinator2<'t, F, S>,
{
    Opt { comb, _f: PhantomData, _s: PhantomData }
}
