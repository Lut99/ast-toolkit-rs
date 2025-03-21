//  MAP RECOVERABLE.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 12:05:08
//  Last edited:
//    07 Mar 2025, 14:23:23
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`map_recoverable()`]-combinator.
//

use std::marker::PhantomData;

use crate::Combinator;
use crate::result::{Result as SResult, SnackError};


/***** COMBINATORS *****/
/// Actual implementation of the [`map_recoverable()`]-combinator.
pub struct MapRecoverable<C, P, F, S> {
    /// The nested combinator who's result we're mapping.
    comb: C,
    /// The predicate that does the mapping.
    pred: P,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, P, E1, E2, F, S> Combinator<'t, F, S> for MapRecoverable<C, P, F, S>
where
    C: Combinator<'t, F, S, Recoverable = E1>,
    P: FnMut(E1) -> E2,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = E2;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        match self.comb.parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable((self.pred)(err))),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(err)),
            Err(SnackError::NotEnough { needed, span }) => Err(SnackError::NotEnough { needed, span }),
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
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// #[derive(Debug, PartialEq)]
/// struct Hidden;
///
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = map_recoverable(tag("Hello"), |_err| Hidden);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(comb.parse(span2), Err(SnackError::Recoverable(Hidden)));
/// ```
#[inline]
pub const fn map_recoverable<'t, C, P, E1, E2, F, S>(comb: C, pred: P) -> MapRecoverable<C, P, F, S>
where
    C: Combinator<'t, F, S, Recoverable = E1>,
    P: FnMut(E1) -> E2,
{
    MapRecoverable { comb, pred, _f: PhantomData, _s: PhantomData }
}
