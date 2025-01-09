//  FEW 1.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 18:44:42
//  Last edited:
//    09 Jan 2025, 20:36:39
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`few1()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

pub use super::many1::Many1ExpectsFormatter as Few1ExpectsFormatter;
use crate::Combinator2;
use crate::result::{Result as SResult, SnackError};
use crate::span::LenBytes;


/***** COMBINATORS *****/
/// Actual implementation of the [`many1()`]-combinator.
pub struct Few1<F, S, C> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, F, S, C> Combinator2<'t, F, S> for Few1<F, S, C>
where
    F: Clone,
    S: Clone + LenBytes,
    C: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = Few1ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Vec<C::Output>;
    type Recoverable = Few1Recoverable<F, S, C::ExpectsFormatter>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { Few1ExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let mut res: Vec<C::Output> = Vec::new();
        let mut rem: Span<F, S> = input;
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
                        return Err(SnackError::Recoverable(Few1Recoverable { what: self.comb.expects(), span: rem }));
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
/// a version that also accepts parsing none, see [`few0()`](super::few0()) instead.
///
/// # Streaming
/// The few1-combinator's streamingness comes from using a streamed version of the nested
/// combinator or not. Being lazy, if no input is left after a successful parse of `comb`, this
/// will _not_ return a [`SnackError::NotEnough`] (unlike [`many1()`](super::many1())). If you want
/// the combinator to try and fetch more input to continue parsing instead, consider using
/// [`many1()`](super::many1()).
///
/// Note that, in the case the above occurs while no input is parsed, [`SnackError::NotEnough`]
/// _is_ returned to indicate at least one is expected.
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`Few1`] that applies the given `comb`inator until it fails.
///
/// It will return the input as a [`Vec`].
///
/// # Fails
/// The returned combinator cannot fail recoverably. However, if the given `comb`inator fails
/// fatally, that error is propagated up.
///
/// # Examples
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::multi2::few1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohellohellogoodbye");
/// let span2 = Span::new("<example>", "hellohelgoodbye");
/// let span3 = Span::new("<example>", "goodbye");
///
/// let mut comb = few1(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(15..), vec![span1.slice(..5), span1.slice(5..10), span1.slice(10..15)]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), vec![span2.slice(..5)])));
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(few1::Few1Recoverable {
///         what: tag::TagExpectsFormatter { tag: "hello" },
///         span: span3,
///     }))
/// );
/// ```
///
/// Another example which shows the usage w.r.t. unexpected end-of-files in streaming contexts:
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::multi2::few1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohello");
/// let span2 = Span::new("<example>", "hellohel");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = few1(tag("hello"));
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
pub const fn few1<'t, F, S, C>(comb: C) -> Few1<F, S, C>
where
    F: Clone,
    S: Clone + LenBytes,
    C: Combinator2<'t, F, S>,
{
    Few1 { comb, _f: PhantomData, _s: PhantomData }
}
