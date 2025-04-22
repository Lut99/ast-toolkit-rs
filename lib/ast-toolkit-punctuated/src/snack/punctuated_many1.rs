//  PUNCTUATED MANY 1.rs
//    by Lut99
//
//  Created:
//    12 Mar 2025, 13:43:43
//  Last edited:
//    22 Apr 2025, 13:16:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`punctuated_many1()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_snack::Combinator;
use ast_toolkit_snack::combinator::remember;
use ast_toolkit_snack::result::{Expected, Result as SResult, SnackError};
use ast_toolkit_snack::span::Parsable;
use ast_toolkit_span::{Span, Spannable};

pub use super::punctuated_most1::{ExpectsFormatter, Fatal, Recoverable};
use crate::Punctuated;


/***** COMBINATORS *****/
/// Actual implementation of the [`punctuated_many1()`]-combinator.
pub struct PunctuatedMany1<C1, C2, S> {
    comb: C1,
    sep:  C2,
    _s:   PhantomData<S>,
}
impl<'c, 's, C1, C2, S> Combinator<'c, 's, S> for PunctuatedMany1<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
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
                res.push_first(elem);
                rem
            },
            Err(SnackError::Recoverable(_)) => return Err(SnackError::Recoverable(Expected { fmt: self.expects(), span: input })),
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Comb(err))),
            Err(SnackError::NotEnough { needed, span }) => {
                // We're lazy; so if we happen to bump into the end, we assume that none were found.
                if input.is_empty() {
                    return Err(SnackError::Recoverable(Expected { fmt: self.expects(), span: input }));
                }
                return Err(SnackError::NotEnough { needed, span });
            },
        };

        // Then parse as long as there are commas
        loop {
            // This is why it's lazy; if there's no input left, stop
            if rem.is_empty() {
                return Ok((rem, res));
            }

            // Try the comma first
            let (sep, span): (C2::Output, Span<S>) = match remember(&mut self.sep).parse(rem.clone()) {
                Ok((rem2, sep)) => {
                    rem = rem2;
                    sep
                },
                Err(SnackError::Recoverable(_)) => return Ok((rem, res)),
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Separator(err))),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
            };

            // Parse the element
            match self.comb.parse(rem) {
                Ok((rem2, elem)) => {
                    if res.len() >= res.capacity() {
                        res.reserve(1 + res.len())
                    }
                    res.push(sep, elem);
                    rem = rem2;
                },
                Err(SnackError::Recoverable(_)) => return Err(SnackError::Fatal(Fatal::TrailingSeparator { span })),
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Comb(err))),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
            }
        }
    }
}





/***** LIBRARY *****/
/// Applies some combinator interleaved by some separator as many times as possible until it fails,
/// greedily parsing multiple instances of the same input.
///
/// Note that this combinator requires at least 1 occurrence of the chosen combinator. If you want
/// a version that also accepts parsing none, see [`punctuated_many0()`](super::punctuated_many0())
/// instead.
///
/// # Streaming
/// The punctuated_many1-combinator's streamingness comes from using a streamed version of the
/// nested combinator or not. BBeing lazy, if no input is left after a successful parse of `comb`,
/// this will _not_ return a [`SnackError::NotEnough`] (unlike
/// [`punctuated_most1()`](super::punctuated_most1())). If you want the combinator to try and fetch
/// more input to continue parsing instead, consider using
/// [`punctuated_most1()`](super::punctuated_most1())).
///
/// As a rule of thumb, use the `many`-combinators when the user indicates the end of the
/// repetitions by simply not specifying any more (e.g., statements).
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`PunctuatedMany1`] that applies the given `comb`inator until it fails.
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
/// use ast_toolkit_punctuated::snack::punctuated_many1;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hello,hello,hellogoodbye");
/// let span2 = Span::new("hellogoodbye");
/// let span3 = Span::new("goodbye");
/// let span4 = Span::new(",hello");
/// let span5 = Span::new("hello,helgoodbye");
///
/// let mut comb = punctuated_many1(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(17..), punct![
///         v => span1.slice(..5),
///         p => span1.slice(5..6),
///         v => span1.slice(6..11),
///         p => span1.slice(11..12),
///         v => span1.slice(12..17)
///     ]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), punct![v => span2.slice(..5)])));
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(punctuated_many1::Recoverable {
///         fmt:  punctuated_many1::ExpectsFormatter {
///             fmt: tag::ExpectsFormatter { tag: "hello" },
///             sep: tag::ExpectsFormatter { tag: "," },
///         },
///         span: span3,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(punctuated_many1::Recoverable {
///         fmt:  punctuated_many1::ExpectsFormatter {
///             fmt: tag::ExpectsFormatter { tag: "hello" },
///             sep: tag::ExpectsFormatter { tag: "," },
///         },
///         span: span4,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Fatal(punctuated_many1::Fatal::TrailingSeparator {
///         span: span5.slice(5..6),
///     }))
/// );
/// ```
///
/// Another example which shows the usage w.r.t. unexpected end-of-files in streaming contexts:
/// ```rust
/// use ast_toolkit_punctuated::punct;
/// use ast_toolkit_punctuated::snack::punctuated_many1;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hello,hello");
/// let span2 = Span::new("hello,hel");
/// let span3 = Span::new("");
///
/// let mut comb = punctuated_many1(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(11..), punct![v => span1.slice(..5), p => span1.slice(5..6), v => span1.slice(6..11)]))
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::NotEnough { needed: Some(2), span: span2.slice(9..) })
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(punctuated_many1::Recoverable {
///         fmt:  punctuated_many1::ExpectsFormatter {
///             fmt: tag::ExpectsFormatter { tag: "hello" },
///             sep: tag::ExpectsFormatter { tag: "," },
///         },
///         span: span3,
///     }))
/// );
/// ```
#[inline]
pub const fn punctuated_many1<'c, 's, C1, C2, S>(comb: C1, sep: C2) -> PunctuatedMany1<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
{
    PunctuatedMany1 { comb, sep, _s: PhantomData }
}
