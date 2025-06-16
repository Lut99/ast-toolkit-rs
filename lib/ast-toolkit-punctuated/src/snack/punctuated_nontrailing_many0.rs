//  PUNCTUATED MOST 0.rs
//    by Lut99
//
//  Created:
//    07 Mar 2025, 17:15:43
//  Last edited:
//    08 May 2025, 13:22:18
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`punctuated_nontrailing_many0()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_snack::Combinator;
use ast_toolkit_snack::combinator::remember;
pub use ast_toolkit_snack::multi::separated_many0::{ExpectsFormatter, Fatal};
use ast_toolkit_snack::result::{Result as SResult, SnackError};
use ast_toolkit_span::{Span, Spannable};

use crate::Punctuated;


/***** COMBINATORS *****/
/// Actual implementation of the [`punctuated_nontrailing_many0()`]-combinator.
pub struct PunctuatedNontrailingMany0<C1, C2, S> {
    comb: C1,
    sep:  C2,
    _s:   PhantomData<S>,
}
impl<'c, 's, C1, C2, S> Combinator<'c, 's, S> for PunctuatedNontrailingMany0<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = Punctuated<C1::Output, C2::Output>;
    type Recoverable = Infallible;
    type Fatal = Fatal<C1::Fatal, C2::Fatal, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects(), sep: self.sep.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Parse the first element
        let mut res: Punctuated<C1::Output, C2::Output> = Punctuated::new();
        let mut rem: Span<S> = match self.comb.parse(input.clone()) {
            Ok((rem, elem)) => {
                if res.len() >= res.capacity() {
                    res.reserve(1 + res.len())
                }
                // SAFETY: It's the first element, so always safe to push
                unsafe { res.push_value_unchecked(elem) };
                rem
            },
            Err(SnackError::Recoverable(_)) => return Ok((input, res)),
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Comb(err))),
        };

        // Then parse as long as there are commas
        loop {
            // Try the comma first
            let (sep, span): (C2::Output, Span<S>) = match remember(&mut self.sep).parse(rem.clone()) {
                Ok((rem2, sep)) => {
                    rem = rem2;
                    sep
                },
                Err(SnackError::Recoverable(_)) => return Ok((rem, res)),
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Separator(err))),
            };

            // Parse the element
            match self.comb.parse(rem) {
                Ok((rem2, elem)) => {
                    if res.len() >= res.capacity() {
                        res.reserve(1 + res.len())
                    }
                    res.push_punct(sep);
                    // SAFETY: We push a punctuation in the line above, so we always know it's safe to push
                    unsafe { res.push_value_unchecked(elem) };
                    rem = rem2;
                },
                Err(SnackError::Recoverable(_)) => return Err(SnackError::Fatal(Fatal::TrailingSeparator { span })),
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Comb(err))),
            }
        }
    }
}





/***** LIBRARY *****/
/// Applies some combinator interleaved by some separator as many times as possible until it fails,
/// greedily parsing multiple instances of the same input.
///
/// Note that this combinator is OK with matching no input, and can therefore itself not fail.
/// If you want at least one, see
/// [`punctuated_nontrailing_many1()`](super::punctuated_nontrailing_many1()) instead.
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`PunctuatedNontrailingMany0`] that applies the given `comb`inator until it fails.
///
/// It will return the input as a [`Punctuated`].
///
/// # Fails
/// The returned combinator cannot fail recoverably. However, if the given `comb`inator fails
/// fatally, that error is propagated up.
///
/// # Examples
/// ```rust
/// use ast_toolkit_punctuated::punct;
/// use ast_toolkit_punctuated::snack::punctuated_nontrailing_many0;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hello,hello,hellogoodbye");
/// let span2 = Span::new("hellogoodbye");
/// let span3 = Span::new("goodbye");
/// let span4 = Span::new(",hello");
/// let span5 = Span::new("hello,helgoodbye");
///
/// let mut comb = punctuated_nontrailing_many0(tag(b"hello"), tag(b","));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(17..), punct![
///         span1.slice(..5),
///         span1.slice(5..6),
///         span1.slice(6..11),
///         span1.slice(11..12),
///         span1.slice(12..17)
///     ]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), punct![span2.slice(..5)])));
/// assert_eq!(comb.parse(span3), Ok((span3, punct![])));
/// assert_eq!(comb.parse(span4), Ok((span4, punct![])));
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Fatal(punctuated_nontrailing_many0::Fatal::TrailingSeparator {
///         span: span5.slice(5..6),
///     }))
/// );
/// ```
#[inline]
pub const fn punctuated_nontrailing_many0<'c, 's, C1, C2, S>(comb: C1, sep: C2) -> PunctuatedNontrailingMany0<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    PunctuatedNontrailingMany0 { comb, sep, _s: PhantomData }
}
