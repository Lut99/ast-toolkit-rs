//  UNCUT.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 23:12:16
//  Last edited:
//    18 Jan 2025, 17:56:18
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`uncut()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};

use crate::Combinator2;
use crate::result::{Result as SResult, SnackError};


/***** ERRORS *****/
/// Defines recoverable errors for the [`Uncut`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub enum Recoverable<E1, E2> {
    /// It's a recoverable error of the nested combinator.
    Recoverable(E1),
    /// It's a fatal error of the nested combinator.
    Fatal(E2),
}
impl<E1: Display, E2: Display> Display for Recoverable<E1, E2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Recoverable(err) => err.fmt(f),
            Self::Fatal(err) => err.fmt(f),
        }
    }
}
impl<E1: Error, E2: Error> Error for Recoverable<E1, E2> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Recoverable(err) => err.source(),
            Self::Fatal(err) => err.source(),
        }
    }
}
impl<F, S, E1: Spanning<F, S>, E2: Spanning<F, S>> Spanning<F, S> for Recoverable<E1, E2> {
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
/// Actual implementation of the [`uncut()`]-operator.
pub struct Uncut<C, F, S> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator2<'t, F, S> for Uncut<C, F, S>
where
    C: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = Recoverable<C::Recoverable, C::Fatal>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        match self.comb.parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Recoverable::Recoverable(err))),
            Err(SnackError::Fatal(err)) => Err(SnackError::Recoverable(Recoverable::Fatal(err))),
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
        }
    }
}





/***** LIBRARY *****/
/// Powerful combinator that turns fatal errors into recoverable ones.
///
/// This can be used to tell the parser that a currect branch has been identified (i.e., it is
/// certain what the user attempts to express), but something about it is ill-formed.
///
/// # Arguments
/// - `comb`: The combinator whos fatal errors to turn into recoverable ones.
///
/// # Returns
/// A combinator [`Uncut`] that copies the given `comb`inator's behaviour.
///
/// # Fails
/// The returned combinator fails exactly when the given `comb`inator fails. However, if this
/// failure is a fatal error, it is returned as a recoverable one instead.
///
/// Note that, to do this, the uncut-combinator returns its own
/// [recoverable error type](Recoverable) that covers both possible errors of the given
/// `comb`inator.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::error2::{fatal, uncut};
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "Hello, world!");
///
/// let mut comb = uncut(fatal());
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::Recoverable(uncut::Recoverable::Fatal(fatal::Fatal { span: span1 })))
/// );
/// ```
#[inline]
pub const fn uncut<'t, C, F, S>(comb: C) -> Uncut<C, F, S>
where
    C: Combinator2<'t, F, S>,
{
    Uncut { comb, _f: PhantomData, _s: PhantomData }
}
