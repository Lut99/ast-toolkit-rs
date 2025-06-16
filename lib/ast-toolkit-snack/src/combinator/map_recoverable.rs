//  MAP RECOVERABLE.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 12:05:08
//  Last edited:
//    08 May 2025, 11:19:21
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`map_recoverable()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Spannable;

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ParseError};


/***** COMBINATORS *****/
/// Actual implementation of the [`map_recoverable()`]-combinator.
pub struct MapRecoverable<C, P, S> {
    /// The nested combinator who's result we're mapping.
    comb: C,
    /// The predicate that does the mapping.
    pred: P,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, P, E1, E2, S> Combinator<'c, 's, S> for MapRecoverable<C, P, S>
where
    C: Combinator<'c, 's, S, Recoverable = E1>,
    P: FnMut(E1) -> E2,
    E2: ParseError<S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = E2;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match self.comb.parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable((self.pred)(err))),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(err)),
        }
    }
}





/***** LIBRARY *****/
/// Maps the recoverable error possibly emitted by a combinator to something else.
///
/// # Arguments
/// - `comb`: Some combinator to run.
/// - `pred`: Some closure that takes the `comb`'s error and maps it to something else.
///
/// # Returns
/// A combinator [`MapRecoverable`] that runs the given `comb`inator, and then maps any recoverable
/// errors using `pred`.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails, but with the mapped recoverable.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::map_recoverable;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// // Some error type. Note that it has to be `ParseError`-compatible, either by itself
/// // (implements `Spanning`) or through `SpanningError`.
/// #[derive(Debug, PartialEq)]
/// struct Hidden;
/// impl std::fmt::Display for Hidden {
///     /* ... */
/// #   #[inline]
/// #   fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { write!(f, "REDACTED") }
/// }
/// impl std::error::Error for Hidden {}
/// impl<S: Clone> ast_toolkit_span::Spanning<S> for Hidden {
///     /* ... */
/// #   fn span(&self) -> std::borrow::Cow<Span<S>> { unreachable!() }
/// #   fn into_span(self) -> Span<S> { unreachable!() }
/// }
/// impl<S: Clone> ast_toolkit_snack::ParseError<S> for Hidden {
///     /* ... */
/// #   fn more_might_fix(&self) -> bool { false }
/// #   fn needed_to_fix(&self) -> Option<usize> { None }
/// }
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = map_recoverable(tag(b"Hello"), |_err| Hidden);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(comb.parse(span2), Err(SnackError::Recoverable(Hidden)));
/// ```
#[inline]
pub const fn map_recoverable<'c, 's, C, P, E1, E2, S>(comb: C, pred: P) -> MapRecoverable<C, P, S>
where
    C: Combinator<'c, 's, S, Recoverable = E1>,
    P: FnMut(E1) -> E2,
    E2: ParseError<S>,
    S: Clone + Spannable<'s>,
{
    MapRecoverable { comb, pred, _s: PhantomData }
}
