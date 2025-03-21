//  CUT.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 21:34:30
//  Last edited:
//    18 Jan 2025, 17:53:35
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`cut()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};

use crate::Combinator;
use crate::result::{Result as SResult, SnackError};


/***** ERRORS *****/
/// Defines fatal errors for the [`Cut`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub enum Fatal<E1, E2> {
    /// It's a recoverable error of the nested combinator.
    Recoverable(E1),
    /// It's a fatal error of the nested combinator.
    Fatal(E2),
}
impl<E1: Display, E2: Display> Display for Fatal<E1, E2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Recoverable(err) => err.fmt(f),
            Self::Fatal(err) => err.fmt(f),
        }
    }
}
impl<E1: Error, E2: Error> Error for Fatal<E1, E2> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Recoverable(err) => err.source(),
            Self::Fatal(err) => err.source(),
        }
    }
}
impl<F, S, E1: Spanning<F, S>, E2: Spanning<F, S>> Spanning<F, S> for Fatal<E1, E2> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Recoverable(err) => err.span(),
            Self::Fatal(err) => err.span(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<F, S> {
        match self {
            Self::Recoverable(err) => err.into_span(),
            Self::Fatal(err) => err.into_span(),
        }
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`cut()`]-operator.
pub struct Cut<C, F, S> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator<'t, F, S> for Cut<C, F, S>
where
    C: Combinator<'t, F, S>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = Infallible;
    type Fatal = Fatal<C::Recoverable, C::Fatal>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        match self.comb.parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Fatal(Fatal::Recoverable(err))),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(Fatal::Fatal(err))),
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
        }
    }
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
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = cut(tag("Hello"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Fatal(Fatal::Recoverable(tag::Recoverable {
///         tag:  "Hello",
///         span: span2.slice(0..),
///     })))
/// );
/// ```
#[inline]
pub const fn cut<'t, C, F, S>(comb: C) -> Cut<C, F, S>
where
    C: Combinator<'t, F, S>,
{
    Cut { comb, _f: PhantomData, _s: PhantomData }
}
