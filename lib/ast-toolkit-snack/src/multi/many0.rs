//  MANY 0.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 18:37:50
//  Last edited:
//    20 Mar 2025, 15:51:07
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`many0()`]-combinator.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::Span;

pub use super::most0::ExpectsFormatter;
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};
use crate::span::Parsable;


/***** COMBINATORS *****/
/// Actual implementation of the [`many0()`]-combinator.
pub struct Many0<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'t, C, S> Combinator<'t, S> for Many0<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Vec<C::Output>;
    type Recoverable = Infallible;
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
                return Ok((rem, res));
            }
            match self.comb.parse(rem.clone()) {
                Ok((rem2, res2)) => {
                    if res.len() >= res.capacity() {
                        res.reserve(1 + res.len())
                    }
                    res.push(res2);
                    rem = rem2;
                },
                Err(SnackError::Recoverable(_)) => return Ok((rem, res)),
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(err)),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
            }
        }
    }
}





/***** LIBRARY *****/
/// Defines a lazy alternative to [`many0()`](super::many0()) that applies some other combinator as
/// many times as possible until it fails, parsing multiple instances of the same input.
///
/// Note that this combinator is OK with matching no input, and can therefore itself not fail.
/// If you want at least one, see [`many1()`](super::many1()) instead.
///
/// # Streaming
/// The many0-combinator's streamingness comes from using a streamed version of the nested
/// combinator or not. Being lazy, if no input is left after a successful parse of `comb`, this
/// will _not_ return a [`SnackError::NotEnough`] (unlike [`most0()`](super::most0())). If you want
/// the combinator to try and fetch more input to continue parsing instead, consider using
/// [`most0()`](super::most0()).
///
/// As a rule of thumb, use the `many`-combinators when the user indicates the end of the
/// repetitions by simply not specifying any more (e.g., statements).
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`Many0`] that applies the given `comb`inator until it fails.
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
/// use ast_toolkit_snack::multi::many0;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hellohellohellogoodbye");
/// let span2 = Span::new("hellohelgoodbye");
/// let span3 = Span::new("goodbye");
///
/// let mut comb = many0(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(15..), vec![span1.slice(..5), span1.slice(5..10), span1.slice(10..15)]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), vec![span2.slice(..5)])));
/// assert_eq!(comb.parse(span3), Ok((span3, vec![])));
/// ```
///
/// Another example which shows the usage w.r.t. unexpected end-of-files in streaming contexts:
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::multi::many0;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hellohello");
/// let span2 = Span::new("hellohel");
/// let span3 = Span::new("");
///
/// let mut comb = many0(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(10..), vec![span1.slice(..5), span1.slice(5..10)]))
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::NotEnough { needed: Some(2), span: span2.slice(8..) })
/// );
/// assert_eq!(comb.parse(span3), Ok((span3, vec![])));
/// ```
#[inline]
pub const fn many0<'t, C, S>(comb: C) -> Many0<C, S>
where
    C: Combinator<'t, S>,
    S: Clone + Parsable,
{
    Many0 { comb, _s: PhantomData }
}
