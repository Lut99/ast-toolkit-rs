//  CONSUME.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:22:15
//  Last edited:
//    19 Mar 2025, 10:44:24
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`consume()`]-combinator.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::result::{Result as SResult, SnackError};
use crate::span::Parsable;
use crate::{Combinator, ExpectsFormatter as _};


/***** ERRORS *****/
/// Defines the errors emitted by [`Consume`].
#[derive(Debug, Eq, PartialEq)]
pub enum Recoverable<E, S> {
    /// The nested combinator failed.
    Comb(E),
    /// There was input left.
    RemainingInput { span: Span<S> },
}
impl<E: Display, S> Display for Recoverable<E, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Comb(err) => <E as Display>::fmt(err, f),
            Self::RemainingInput { .. } => write!(f, "Not all input was successfully parsed."),
        }
    }
}
impl<E: Error, S: Spannable> Error for Recoverable<E, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Comb(err) => <E as Error>::source(err),
            Self::RemainingInput { .. } => None,
        }
    }
}
impl<E: Spanning<S>, S: Clone> Spanning<S> for Recoverable<E, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> {
        match self {
            Self::Comb(err) => err.span(),
            Self::RemainingInput { span } => Cow::Borrowed(span),
        }
    }

    #[inline]
    fn into_span(self) -> Span<S> {
        match self {
            Self::Comb(err) => err.into_span(),
            Self::RemainingInput { span } => span,
        }
    }
}




/***** FORMATTERS *****/
/// Expects formatter for the [`Consume`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<F> {
    /// The nested combinator is what we're expecting.
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
        self.fmt.expects_fmt(f, indent)?;
        write!(f, " (and nothing else)")
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`consume()`]-combinator.
#[derive(Debug)]
pub struct Consume<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'t, C, S> Combinator<'t, S> for Consume<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = C::Output;
    type Recoverable = Recoverable<C::Recoverable, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // First, parse the combinator as usual
        let (rem, res): (Span<S>, C::Output) = match self.comb.parse(input) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => return Err(SnackError::Recoverable(Recoverable::Comb(err))),
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(err)),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Then assert that nothing is remaining
        if rem.is_empty() { Result::Ok((rem, res)) } else { Err(SnackError::Recoverable(Recoverable::RemainingInput { span: rem })) }
    }
}





/***** LIBRARY *****/
/// Matches the full input text.
///
/// Note that this version could be counted as part of the `complete`-suite. There is no streaming
/// counterpart, though, because consuming all input while more may be expected doesn't make too
/// much sense.
///
/// # Arguments
/// - `comb`: Some combinator to apply to the input text in order to match it.
///
/// # Returns
/// A combinator [`Consume`] that will apply `comb`.
///
/// # Fails
/// The returned combinator fails if `comb` fails, or if there is any input left after `comb` has
/// parsed from the input.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::consume;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello");
/// let span2 = Span::new("Hello, world!");
/// let span3 = Span::new("Hey");
///
/// let mut comb = consume(tag("Hello"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(consume::Recoverable::RemainingInput {
///         span: span2.slice(5..),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(consume::Recoverable::Comb(tag::Recoverable {
///         tag:  "Hello",
///         span: span3,
///     })))
/// );
/// ```
#[inline]
pub const fn consume<'t, C, S>(comb: C) -> Consume<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    Consume { comb, _s: PhantomData }
}
