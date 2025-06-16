//  RECOGNIZE.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 13:58:07
//  Last edited:
//    08 May 2025, 11:20:20
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`recognize()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use super::remember;
use crate::Combinator;
use crate::result::Result as SResult;


/***** COMBINATORS *****/
/// Actual implementation of the [`recognize()`]-combinator.
pub struct Recognize<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for Recognize<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = C::ExpectsFormatter;
    type Output = Span<S>;
    type Recoverable = C::Recoverable;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.comb.expects() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // We simply use remember but discard the actual result
        remember(&mut self.comb).parse(input).map(|(rem, (_, span))| (rem, span))
    }
}





/***** LIBRARY *****/
/// Uses the given combinator to parse the input, but always returns the parsed input instead of
/// what the combinator returns.
///
/// This can be used to "dumb down" complex combinators that return complex types.
///
/// # Arguments
/// - `comb`: The [`Combinator`] to negate.
///
/// # Returns
/// A combinator [`Recognize`] that copies the behaviour of a given `comb`inator, but returns a
/// [`Span`] instead.
///
/// # Fails
/// The returned combinator fails if the given `comb`inator fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::recognize;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_snack::sequence::pair;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = recognize(pair(tag(b"Hello, "), tag(b"world!")));
/// assert_eq!(comb.parse(span1), Ok((span1.slice(13..), span1.slice(..13))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(pair::Recoverable::Comb0(tag::Recoverable {
///         tag: b"Hello, ",
///         is_fixable: false,
///         span: span2,
///     })))
/// );
/// ```
#[inline]
pub const fn recognize<'c, 's, C, S>(comb: C) -> Recognize<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    Recognize { comb, _s: PhantomData }
}
