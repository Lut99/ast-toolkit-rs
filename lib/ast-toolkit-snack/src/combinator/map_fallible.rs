//  MAP FALLIBLE.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:57:10
//  Last edited:
//    08 May 2025, 11:19:05
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`map_fallible()`]-combinator.
//

use std::borrow::Cow;
use std::error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning, SpanningInf, SpanningMut, SpanningRef};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ParseError};


/***** TYPE ALIASES *****/
/// Recoverable errors emitted by [`MapFallible`].
pub type Recoverable<E1, E2> = Error<E1, E2>;

/// Fatal errors emitted by [`MapFallible`].
pub type Fatal<E1, E2> = Error<E1, E2>;





/***** ERRORS *****/
/// Recoverable & fatal errors emitted by [`MapFallible`].
#[derive(Debug, Eq, PartialEq)]
pub enum Error<E1, E2> {
    /// The error emitted by the combinator failing.
    Comb(E1),
    /// The error emitted by the map failing.
    Map(E2),
}
impl<E1: Display, E2: Display> Display for Error<E1, E2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Comb(err) => <E1 as Display>::fmt(err, f),
            Self::Map(err) => <E2 as Display>::fmt(err, f),
        }
    }
}
impl<E1: error::Error, E2: error::Error> error::Error for Error<E1, E2> {
    #[inline]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::Comb(err) => <E1 as error::Error>::source(err),
            Self::Map(err) => <E2 as error::Error>::source(err),
        }
    }
}
impl<E1: Spanning<S>, E2: Spanning<S>, S: Clone> Spanning<S> for Error<E1, E2> {
    #[inline]
    fn get_span(&self) -> Option<Cow<'_, Span<S>>> {
        match self {
            Self::Comb(err) => err.get_span(),
            Self::Map(err) => err.get_span(),
        }
    }

    #[inline]
    fn take_span(self) -> Option<Span<S>> {
        match self {
            Self::Comb(err) => err.take_span(),
            Self::Map(err) => err.take_span(),
        }
    }
}
impl<E1: SpanningInf<S>, E2: SpanningInf<S>, S: Clone> SpanningInf<S> for Error<E1, E2> {
    #[inline]
    fn span(&self) -> Cow<'_, Span<S>> {
        match self {
            Self::Comb(err) => err.span(),
            Self::Map(err) => err.span(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<S> {
        match self {
            Self::Comb(err) => err.into_span(),
            Self::Map(err) => err.into_span(),
        }
    }
}
impl<E1: SpanningRef<S>, E2: SpanningRef<S>, S: Clone> SpanningRef<S> for Error<E1, E2> {
    #[inline]
    fn span_ref(&self) -> &Span<S> {
        match self {
            Self::Comb(err) => err.span_ref(),
            Self::Map(err) => err.span_ref(),
        }
    }
}
impl<E1: SpanningMut<S>, E2: SpanningMut<S>, S: Clone> SpanningMut<S> for Error<E1, E2> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> {
        match self {
            Self::Comb(err) => err.span_mut(),
            Self::Map(err) => err.span_mut(),
        }
    }
}
impl<E1: ParseError<S>, E2: ParseError<S>, S: Clone> ParseError<S> for Error<E1, E2> {
    #[inline]
    #[track_caller]
    fn more_might_fix(&self) -> bool {
        match self {
            Self::Comb(err) => err.more_might_fix(),
            Self::Map(err) => err.more_might_fix(),
        }
    }

    #[inline]
    #[track_caller]
    fn needed_to_fix(&self) -> Option<usize> {
        match self {
            Self::Comb(err) => err.needed_to_fix(),
            Self::Map(err) => err.needed_to_fix(),
        }
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`map_fallible()`]-combinator.
pub struct MapFallible<C, P, S> {
    /// The nested combinator who's result we're mapping.
    comb: C,
    /// The predicate that does the mapping.
    pred: P,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, P, O1, O2, E1, E2, S> Combinator<'c, 's, S> for MapFallible<C, P, S>
where
    C: Combinator<'c, 's, S, Output = O1>,
    P: FnMut(O1) -> Result<O2, SnackError<E1, E2>>,
    E1: ParseError<S>,
    E2: ParseError<S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = O2;
    type Recoverable = Error<C::Recoverable, E1>;
    type Fatal = Error<C::Fatal, E2>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match self.comb.parse(input) {
            Ok((rem, res)) => match (self.pred)(res) {
                Ok(res) => Ok((rem, res)),
                Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Error::Map(err))),
                Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(Error::Map(err))),
            },
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(Error::Comb(err))),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(Error::Comb(err))),
        }
    }
}





/***** LIBRARY *****/
/// Maps the result of a combinator to something else, but fallibly so.
///
/// This is useful for when you are doing additional validation on parsed objects.
///
/// Not to be confused with [`map_fatal()`](super::map_fatal()).
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `pred`: Some closure that takes the `comb`'s result and maps it to something else, or emits
///   a custom error.
///
///   Note that this custom error must be a [`ParseError`]. If you aren't returning one, you can
///   refer to a [`SpanningError`](crate::result::SpanningError) to wrap it and make it do so.
///
/// # Returns
/// A combinator [`MapFallible`] that runs the given `comb`inator, and then maps the result using
/// pred`, fallibly.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails or if the given closure fails.
///
/// # Example
/// ```rust
/// use std::convert::Infallible;
/// use std::num::ParseIntError;
///
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::ascii::digit1;
/// use ast_toolkit_snack::combinator::map_fallible;
/// use ast_toolkit_snack::result::{SnackError, SpanningError};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("128");
/// let span2 = Span::new("abc");
/// let span3 = Span::new("256");
///
/// let mut comb = map_fallible(digit1(), |parsed| {
///     u8::from_str_radix(std::str::from_utf8(parsed.as_bytes()).unwrap(), 10)
///         .map_err(|err| SnackError::<Infallible, _>::Fatal(SpanningError::new(err, parsed)))
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), 128)));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(map_fallible::Error::Comb(digit1::Recoverable {
///         fmt:     digit1::ExpectsFormatter { what: "digit" },
///         fixable: None,
///         span:    span2,
///     })))
/// );
/// assert!(matches!(
///     comb.parse(span3),
///     Err(SnackError::Fatal(map_fallible::Error::Map(SpanningError {
///         err:     ParseIntError { .. },
///         fixable: None,
///         span:    span3,
///     })))
/// ));
/// ```
#[inline]
pub const fn map_fallible<'c, 's, C, P, O1, O2, E1, E2, S>(comb: C, pred: P) -> MapFallible<C, P, S>
where
    C: Combinator<'c, 's, S, Output = O1>,
    P: FnMut(O1) -> Result<O2, SnackError<E1, E2>>,
    E1: ParseError<S>,
    E2: ParseError<S>,
    S: Clone + Spannable<'s>,
{
    MapFallible { comb, pred, _s: PhantomData }
}
