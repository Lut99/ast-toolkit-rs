//  MAP FALLIBLE.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 11:57:10
//  Last edited:
//    03 Nov 2024, 12:34:07
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`map_fallible()`]-combinator.
//

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator2, Expects};


/***** ERRORS *****/
/// Recoverable & fatal errors emitted by [`MapFallible`].
#[derive(Debug)]
pub enum MapFallibleError<E1, E2> {
    /// The error emitted by the combinator failing.
    Comb(E1),
    /// The error emitted by the map failing.
    Map(E2),
}
impl<E1: Display, E2: Display> Display for MapFallibleError<E1, E2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Comb(err) => <E1 as Display>::fmt(err, f),
            Self::Map(err) => <E2 as Display>::fmt(err, f),
        }
    }
}
impl<E1: Error, E2: Error> Error for MapFallibleError<E1, E2> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Comb(err) => <E1 as Error>::source(err),
            Self::Map(err) => <E2 as Error>::source(err),
        }
    }
}
impl<F, S, E1: Spanning<F, S>, E2: Spanning<F, S>> Spanning<F, S> for MapFallibleError<E1, E2> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Comb(err) => err.span(),
            Self::Map(err) => err.span(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<F, S> {
        match self {
            Self::Comb(err) => err.into_span(),
            Self::Map(err) => err.into_span(),
        }
    }
}
impl<E1: Eq, E2: Eq> Eq for MapFallibleError<E1, E2> {}
impl<E1: PartialEq, E2: PartialEq> PartialEq for MapFallibleError<E1, E2> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Comb(lhs), Self::Comb(rhs)) => lhs == rhs,
            (Self::Map(lhs), Self::Map(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`map_fallible()`]-combinator.
pub struct MapFallible<C, P, F, S> {
    /// The nested combinator who's result we're mapping.
    comb: C,
    /// The predicate that does the mapping.
    pred: P,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C: Expects<'t>, P, F, S> Expects<'t> for MapFallible<C, P, F, S> {
    type Formatter = C::Formatter;

    #[inline]
    fn expects(&self) -> Self::Formatter { self.comb.expects() }
}
impl<'t, C, P, O1, O2, E1, E2, F, S> Combinator2<'t, F, S> for MapFallible<C, P, F, S>
where
    C: Combinator2<'t, F, S, Output = O1>,
    P: FnMut(O1) -> Result<O2, SnackError<F, S, E1, E2>>,
{
    type Output = O2;
    type Recoverable = MapFallibleError<C::Recoverable, E1>;
    type Fatal = MapFallibleError<C::Fatal, E2>;

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        match self.comb.parse(input) {
            Ok((rem, res)) => match (self.pred)(res) {
                Ok(res) => Ok((rem, res)),
                Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(MapFallibleError::Map(err))),
                Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(MapFallibleError::Map(err))),
                Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
            },
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(MapFallibleError::Comb(err))),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(MapFallibleError::Comb(err))),
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
        }
    }
}





/***** LIBRARY *****/
/// Maps the result of a combinator to something else.
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `pred`: Some closure that takes the `comb`'s result and maps it to something else.
///
/// # Returns
/// A combinator [`MapFallible`] that runs the given `comb`inator, and then maps the result using `pred`.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails.
///
/// # Example
/// ```rust
/// use std::convert::Infallible;
/// use std::num::ParseIntError;
///
/// use ast_toolkit_snack::combinator2::map_fallible;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::digit1;
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "128");
/// let span2 = Span::new("<example>", "abc");
/// let span3 = Span::new("<example>", "256");
///
/// let mut comb = map_fallible(digit1(), |parsed| {
///     u8::from_str_radix(parsed.value(), 10)
///         .map_err(|err| SnackError::<_, _, Infallible, ParseIntError>::Fatal(err))
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), 128)));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(map_fallible::MapFallibleError::Comb(
///         digit1::Digit1Recoverable { span: span2 }
///     )))
/// );
/// assert!(matches!(
///     comb.parse(span3),
///     Err(SnackError::Fatal(map_fallible::MapFallibleError::Map(ParseIntError { .. })))
/// ));
/// ```
#[inline]
pub const fn map_fallible<'t, C, P, O1, O2, E1, E2, F, S>(comb: C, pred: P) -> MapFallible<C, P, F, S>
where
    C: Combinator2<'t, F, S, Output = O1>,
    P: FnMut(O1) -> Result<O2, SnackError<F, S, E1, E2>>,
{
    MapFallible { comb, pred, _f: PhantomData, _s: PhantomData }
}
