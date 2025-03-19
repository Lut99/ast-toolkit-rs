//  MAP FATAL.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 12:11:28
//  Last edited:
//    19 Mar 2025, 10:49:57
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`map_fatal()`]-combinator.
//

use std::marker::PhantomData;

use crate::result::{Result as SResult, SnackError};
use crate::span::Parsable;
use crate::{Combinator, ParseError};


/***** COMBINATORS *****/
/// Actual implementation of the [`map_fatal()`]-combinator.
pub struct MapFatal<C, P, S> {
    /// The nested combinator who's result we're mapping.
    comb: C,
    /// The predicate that does the mapping.
    pred: P,
    _s:   PhantomData<S>,
}
impl<'t, C, P, E1, E2, S> Combinator<'t, S> for MapFatal<C, P, S>
where
    C: Combinator<'t, S, Fatal = E1>,
    P: FnMut(E1) -> E2,
    E2: ParseError<S>,
    S: Clone + Parsable,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = C::Recoverable;
    type Fatal = E2;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match self.comb.parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(err)),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal((self.pred)(err))),
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
        }
    }
}





/***** LIBRARY *****/
/// Maps the fatal error possibly emitted by a combinator to something else.
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `pred`: Some closure that takes the `comb`'s error and maps it to something else.
///
/// # Returns
/// A combinator [`MapFatal`] that runs the given `comb`inator, and then maps any fatal errors
/// using `pred`.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails, but with the mapped fatal.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::map_fatal;
/// use ast_toolkit_snack::error::cut;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// #[derive(Debug, PartialEq)]
/// struct Hidden;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = map_fatal(cut(tag("Hello")), |_err| Hidden);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(comb.parse(span2), Err(SnackError::Fatal(Hidden)));
/// ```
#[inline]
pub const fn map_fatal<'t, C, P, E1, E2, S>(comb: C, pred: P) -> MapFatal<C, P, S>
where
    C: Combinator<'t, S, Fatal = E1>,
    P: FnMut(E1) -> E2,
    E2: ParseError<S>,
    S: Clone + Parsable,
{
    MapFatal { comb, pred, _s: PhantomData }
}
