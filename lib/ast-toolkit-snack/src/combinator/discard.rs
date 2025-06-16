//  DISCARD.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 18:55:10
//  Last edited:
//    08 May 2025, 11:18:57
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`discard()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Spannable;

use crate::Combinator;
use crate::result::Result as SResult;


/***** COMBINATORS *****/
/// Actual implementation of the [`discard()`]-combinator.
pub struct Discard<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for Discard<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = ();
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match self.comb.parse(input) {
            Ok((rem, _)) => Ok((rem, ())),
            Err(err) => Err(err),
        }
    }
}





/***** LIBRARY *****/
/// Discards the output of another combinator.
///
/// This useful when you only want to advance the stream but not get anything out of it.
///
/// # Arguments
/// - `comb`: Some combinator to apply and then to discard the output of.
///
/// # Returns
/// A combinator [`Discard`] that will apply `comb` and discard its output.
///
/// # Fails
/// The returned combinator fails exactly when `comb` fails.
///
/// # Exampel
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::discard;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = discard(tag(b"Hello"));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(5..), ())));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable {
///         tag: b"Hello",
///         is_fixable: false,
///         span: span2,
///     }))
/// );
/// ```
#[inline]
pub const fn discard<'c, 's, C, S>(comb: C) -> Discard<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    Discard { comb, _s: PhantomData }
}
