//  UNCUT.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 23:12:16
//  Last edited:
//    08 May 2025, 11:20:52
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`uncut()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::Combinator;
use crate::result::{CutError, Result as SResult, ResultExt};


/***** ERRORS *****/
/// Defines recoverable errors for the [`Uncut`]-combinator.
pub type Recoverable<E1, E2> = CutError<E1, E2>;





/***** COMBINATORS *****/
/// Actual implementation of the [`uncut()`]-operator.
pub struct Uncut<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for Uncut<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = C::Output;
    type Recoverable = Recoverable<C::Recoverable, C::Fatal>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> { self.comb.parse(input).uncut() }
}





/***** LIBRARY *****/
/// Combinator that turns fatal errors into recoverable ones.
///
/// This can be used to "catch" fatal errors as recoverable ones, allowing parent branches to be
/// searched further.
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
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::error::{fatal, uncut};
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
///
/// let mut comb = uncut(fatal());
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::Recoverable(uncut::Recoverable::Fatal(fatal::Fatal { span: span1 })))
/// );
/// ```
#[inline]
pub const fn uncut<'c, 's, C, S>(comb: C) -> Uncut<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    Uncut { comb, _s: PhantomData }
}
