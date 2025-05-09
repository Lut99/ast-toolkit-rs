//  CUT.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 21:34:30
//  Last edited:
//    08 May 2025, 11:20:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`cut()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::Combinator;
use crate::result::{CutError, Result as SResult, ResultExt as _};


/***** ERRORS *****/
/// Defines fatal errors for the [`Cut`]-combinator.
pub type Fatal<E1, E2> = CutError<E1, E2>;





/***** COMBINATORS *****/
/// Actual implementation of the [`cut()`]-operator.
pub struct Cut<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for Cut<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = Infallible;
    type Fatal = Fatal<C::Recoverable, C::Fatal>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> { self.comb.parse(input).cut() }
}





/***** LIBRARY *****/
/// Powerful combinator that turns recoverable errors into fatal ones.
///
/// This can be used to tell the parser that a currect branch has been identified (i.e., it is
/// certain what the user attempts to express), but something about it is ill-formed.
///
/// # Arguments
/// - `comb`: The combinator whos recoverable errors to turn into fatal ones.
///
/// # Returns
/// A combinator [`Cut`] that copies the given `comb`inator's behaviour.
///
/// # Fails
/// The returned combinator fails exactly when the given `comb`inator fails. However, if this
/// failure is a recoverable error, it is returned as a fatal one instead.
///
/// Note that, to do this, the cut-combinator returns its own [fatal error type](Fatal) that
/// covers both possible errors of the given `comb`inator.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::error::cut;
/// use ast_toolkit_snack::error::cut::Fatal;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = cut(tag("Hello"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Fatal(Fatal::Recoverable(tag::Recoverable {
///         tag:  "Hello",
///         span: span2.slice(..),
///     })))
/// );
/// ```
#[inline]
pub const fn cut<'c, 's, C, S>(comb: C) -> Cut<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    Cut { comb, _s: PhantomData }
}
