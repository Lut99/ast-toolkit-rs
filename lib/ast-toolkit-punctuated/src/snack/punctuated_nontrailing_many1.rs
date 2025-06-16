//  PUNCTUATED MOST 1.rs
//    by Lut99
//
//  Created:
//    12 Mar 2025, 13:38:05
//  Last edited:
//    08 May 2025, 13:22:21
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`punctuated_nontrailing_many1()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_snack::combinator::remember;
pub use ast_toolkit_snack::multi::separated_many1::{ExpectsFormatter, Recoverable};
use ast_toolkit_snack::result::{Expected, Result as SResult, SnackError};
use ast_toolkit_snack::{Combinator, ParseError};
use ast_toolkit_span::{Span, Spannable};

pub use super::punctuated_nontrailing_many0::Fatal;
use crate::Punctuated;


/***** COMBINATORS *****/
/// Actual implementation of the [`punctuated_nontrailing_many1()`]-combinator.
pub struct PunctuatedNontrailingMany1<C1, C2, S> {
    comb: C1,
    sep:  C2,
    _s:   PhantomData<S>,
}
impl<'c, 's, C1, C2, S> Combinator<'c, 's, S> for PunctuatedNontrailingMany1<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = Punctuated<C1::Output, C2::Output>;
    type Recoverable = Recoverable<C1::ExpectsFormatter, C2::ExpectsFormatter, S>;
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
            Err(SnackError::Recoverable(err)) => {
                return Err(SnackError::Recoverable(Expected {
                    fmt:     self.expects(),
                    fixable: if err.more_might_fix() { Some(err.needed_to_fix()) } else { None },
                    span:    input,
                }));
            },
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
/// Note that this combinator requires at least 1 occurrence of the chosen combinator. If you want
/// a version that also accepts parsing none, see
/// [`punctuated_nontrailing_most0()`](super::punctuated_nontrailing_most0()) instead.
///
/// # Streaming
/// The punctuated_nontrailing_many1-combinator's streamingness comes from using a streamed version of the
/// nested combinator or not. Being greedy, if no input is left after a successful parse of `comb`,
/// this will _still_ return a [`SnackError::NotEnough`]. If you want the combinator to stop
/// parsing in such a scenario instead, consider using
/// [`punctuated_nontrailing_many1()`](super::punctuated_nontrailing_many1()) instead.
///
/// As a rule of thumb, use the `most`-combinators when the user indicates the end of the
/// repetitions by something concrete (e.g., expressions wrapped in parenthesis).
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`PunctuatedNontrailingMany1`] that applies the given `comb`inator until it fails.
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
/// use ast_toolkit_punctuated::snack::punctuated_nontrailing_many1;
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
/// let mut comb = punctuated_nontrailing_many1(tag(b"hello"), tag(b","));
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
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(punctuated_nontrailing_many1::Recoverable {
///         fmt:     punctuated_nontrailing_many1::ExpectsFormatter {
///             fmt: tag::ExpectsFormatter { tag: b"hello" },
///             sep: tag::ExpectsFormatter { tag: b"," },
///         },
///         fixable: None,
///         span:    span3,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(punctuated_nontrailing_many1::Recoverable {
///         fmt:     punctuated_nontrailing_many1::ExpectsFormatter {
///             fmt: tag::ExpectsFormatter { tag: b"hello" },
///             sep: tag::ExpectsFormatter { tag: b"," },
///         },
///         fixable: None,
///         span:    span4,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Fatal(punctuated_nontrailing_many1::Fatal::TrailingSeparator {
///         span: span5.slice(5..6),
///     }))
/// );
/// ```
#[inline]
pub const fn punctuated_nontrailing_many1<'c, 's, C1, C2, S>(comb: C1, sep: C2) -> PunctuatedNontrailingMany1<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    PunctuatedNontrailingMany1 { comb, sep, _s: PhantomData }
}
