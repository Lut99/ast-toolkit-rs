//  MANY 1.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 18:44:42
//  Last edited:
//    22 Apr 2025, 12:01:32
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`many1()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

pub use super::most1::ExpectsFormatter;
use crate::Combinator;
use crate::result::{Expected, Result as SResult, SnackError};
use crate::span::Parsable;


/***** TYPE ALIASES *****/
/// The recoverable error returned by [`Many1`].
pub type Recoverable<C, S> = Expected<ExpectsFormatter<C>, S>;





/***** COMBINATORS *****/
/// Actual implementation of the [`many1()`]-combinator.
pub struct Many1<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for Many1<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Vec<C::Output>;
    type Recoverable = Recoverable<C::ExpectsFormatter, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        let mut res: Vec<C::Output> = Vec::new();
        let mut rem: Span<S> = input;
        loop {
            // This is why it's lazy; if there's no input left, stop
            if rem.is_empty() {
                if !res.is_empty() {
                    return Ok((rem, res));
                } else {
                    return Err(SnackError::NotEnough { needed: None, span: rem });
                }
            }
            match self.comb.parse(rem.clone()) {
                Ok((rem2, res2)) => {
                    if res.len() >= res.capacity() {
                        res.reserve(1 + res.len())
                    }
                    res.push(res2);
                    rem = rem2;
                },
                Err(SnackError::Recoverable(_)) => {
                    if res.is_empty() {
                        return Err(SnackError::Recoverable(Expected { fmt: self.expects(), span: rem }));
                    } else {
                        return Ok((rem, res));
                    }
                },
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(err)),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
            }
        }
    }
}





/***** LIBRARY *****/
/// Defines a lazy alternative to [`many1()`](super::many1()) that applies some other combinator as
/// many times as possible until it fails, parsing multiple instances of the same input.
///
/// Note that this combinator requires at least 1 occurrence of the chosen combinator. If you want
/// a version that also accepts parsing none, see [`many0()`](super::many0()) instead.
///
/// # Streaming
/// The many1-combinator's streamingness comes from using a streamed version of the nested
/// combinator or not. Being lazy, if no input is left after a successful parse of `comb`, this
/// will _not_ return a [`SnackError::NotEnough`] (unlike [`most1()`](super::most1())). If you want
/// the combinator to try and fetch more input to continue parsing instead, consider using
/// [`most1()`](super::most1()).
///
/// Note that, in the case the above occurs while no input is parsed, [`SnackError::NotEnough`]
/// _is_ returned to indicate at least one is expected.
///
/// As a rule of thumb, use the `many`-combinators when the user indicates the end of the
/// repetitions by simply not specifying any more (e.g., statements).
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`Many1`] that applies the given `comb`inator until it fails.
///
/// It will return the input as a [`Vec`].
///
/// # Fails
/// The returned combinator cannot fail recoverably. However, if the given `comb`inator fails
/// fatally, that error is propagated up.
///
/// # Examples
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::multi::many1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hellohellohellogoodbye");
/// let span2 = Span::new("hellohelgoodbye");
/// let span3 = Span::new("goodbye");
///
/// let mut comb = many1(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(15..), vec![span1.slice(..5), span1.slice(5..10), span1.slice(10..15)]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), vec![span2.slice(..5)])));
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(many1::Recoverable {
///         fmt:  many1::ExpectsFormatter { fmt: tag::ExpectsFormatter { tag: "hello" } },
///         span: span3,
///     }))
/// );
/// ```
///
/// Another example which shows the usage w.r.t. unexpected end-of-files in streaming contexts:
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::multi::many1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hellohello");
/// let span2 = Span::new("hellohel");
/// let span3 = Span::new("");
///
/// let mut comb = many1(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(10..), vec![span1.slice(..5), span1.slice(5..10)]))
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::NotEnough { needed: Some(2), span: span2.slice(8..) })
/// );
/// assert_eq!(comb.parse(span3), Err(SnackError::NotEnough { needed: None, span: span3 }));
/// ```
#[inline]
pub const fn many1<'c, 's, C, S>(comb: C) -> Many1<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
{
    Many1 { comb, _s: PhantomData }
}
