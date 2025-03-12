//  PUNCTUATED MOST 0.rs
//    by Lut99
//
//  Created:
//    07 Mar 2025, 17:15:43
//  Last edited:
//    12 Mar 2025, 13:33:50
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`punctuated_most0()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_snack::Combinator;
use ast_toolkit_snack::combinator::remember;
pub use ast_toolkit_snack::multi::separated_most0::{ExpectsFormatter, Fatal};
use ast_toolkit_snack::result::{Result as SResult, SnackError};
use ast_toolkit_span::Span;

use crate::Punctuated;


/***** COMBINATORS *****/
/// Actual implementation of the [`punctuated_most0()`]-combinator.
pub struct PunctuatedMost0<C1, C2, F, S> {
    comb: C1,
    sep:  C2,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C1, C2, F, S> Combinator<'t, F, S> for PunctuatedMost0<C1, C2, F, S>
where
    C1: Combinator<'t, F, S>,
    C2: Combinator<'t, F, S>,
    F: Clone,
    S: Clone,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = Punctuated<C1::Output, C2::Output>;
    type Recoverable = Infallible;
    type Fatal = Fatal<C1::Fatal, C2::Fatal, F, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects(), sep: self.sep.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        // Parse the first element
        let mut res: Punctuated<C1::Output, C2::Output> = Punctuated::new();
        let mut rem: Span<F, S> = match self.comb.parse(input.clone()) {
            Ok((rem, elem)) => {
                if res.len() >= res.capacity() {
                    res.reserve(1 + res.len())
                }
                res.push_first(elem);
                rem
            },
            Err(SnackError::Recoverable(_)) => return Ok((input, res)),
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Comb(err))),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Then parse as long as there are commas
        loop {
            // Try the comma first
            let (sep, span): (C2::Output, Span<F, S>) = match remember(&mut self.sep).parse(rem.clone()) {
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
/// Note that this combinator is OK with matching no input, and can therefore itself not fail.
/// If you want at least one, see [`punctuated_most1()`](super::punctuated_most1()) instead.
///
/// # Streaming
/// The punctuated_most0-combinator's streamingness comes from using a streamed version of the
/// nested combinator or not. Being greedy, if no input is left after a successful parse of `comb`,
/// this will _still_ return a [`SnackError::NotEnough`]. If you want the combinator to stop
/// parsing in such a scenario instead, consider using
/// [`punctuated_many0()`](super::punctuated_many0()) instead.
///
/// As a rule of thumb, use the `most`-combinators when the user indicates the end of the
/// repetitions by something concrete (e.g., expressions wrapped in parenthesis).
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`PunctuatedMost0`] that applies the given `comb`inator until it fails.
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
/// use ast_toolkit_punctuated::snack::punctuated_most0;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hellogoodbye");
/// let span2 = Span::new("<example>", "hellogoodbye");
/// let span3 = Span::new("<example>", "goodbye");
/// let span4 = Span::new("<example>", ",hello");
/// let span5 = Span::new("<example>", "hello,helgoodbye");
///
/// let mut comb = punctuated_most0(tag("hello"), tag(","));
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
/// assert_eq!(comb.parse(span3), Ok((span3, punct![])));
/// assert_eq!(comb.parse(span4), Ok((span4, punct![])));
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Fatal(punctuated_most0::Fatal::TrailingSeparator {
///         span: span5.slice(5..6),
///     }))
/// );
/// ```
///
/// Another example which shows the usage w.r.t. unexpected end-of-files in streaming contexts:
/// ```rust
/// use ast_toolkit_punctuated::snack::punctuated_most0;
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello");
/// let span2 = Span::new("<example>", "hello,hel");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = punctuated_most0(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::NotEnough { needed: Some(1), span: span1.slice(11..) })
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::NotEnough { needed: Some(2), span: span2.slice(9..) })
/// );
/// assert_eq!(comb.parse(span3), Err(SnackError::NotEnough { needed: Some(5), span: span3 }));
/// ```
#[inline]
pub const fn punctuated_most0<'t, C1, C2, F, S>(comb: C1, sep: C2) -> PunctuatedMost0<C1, C2, F, S>
where
    C1: Combinator<'t, F, S>,
    C2: Combinator<'t, F, S>,
    F: Clone,
    S: Clone,
{
    PunctuatedMost0 { comb, sep, _f: PhantomData, _s: PhantomData }
}
