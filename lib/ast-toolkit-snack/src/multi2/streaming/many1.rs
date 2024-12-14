//  MANY 1.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 17:57:55
//  Last edited:
//    14 Dec 2024, 18:01:23
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`many1()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::Span;

pub use super::super::complete::many1::{Many1ExpectsFormatter, Many1Recoverable};
use crate::Combinator2;
use crate::result::{Result as SResult, SnackError};
use crate::span::LenBytes;


/***** COMBINATORS *****/
/// Actual implementation of the [`many1()`]-combinator.
pub struct Many1<F, S, C> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, F, S, C> Combinator2<'t, F, S> for Many1<F, S, C>
where
    F: Clone,
    S: Clone + LenBytes,
    C: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = Many1ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Vec<C::Output>;
    type Recoverable = Many1Recoverable<F, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { Many1ExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let mut res: Vec<C::Output> = Vec::new();
        let mut rem: Span<F, S> = input;
        loop {
            if rem.is_empty() {
                if res.is_empty() {
                    return Err(SnackError::NotEnough { needed: None, span: rem });
                } else {
                    return Ok((rem, res));
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
                        return Err(SnackError::Recoverable(Many1Recoverable { what: self.comb.expects().to_string(), span: rem }));
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
/// Applies some other combinator as many times as possible until it fails, greedily parsing
/// multiple instances of the same input.
///
/// Note that this combinator requires at least 1 occurrence of the chosen combinator. If you want
/// a version that also accepts parsing none, see [`many0()`](super::many0()) instead.
///
/// # Streaming
/// The many1-combinator's streamingness comes from using a streamed version of the nested
/// combinator or not. However, note that there is a special use-case: if the many1-combinator
/// precisely consumed the input, it will not re-try parsing the input and throw a
/// [`SnackError::NotEnough`] if it's streaming, but instead stop with the happy path. This
/// prevents users from being endlessly prompted to provide more instances of the thing being
/// parsed multiple times. Except, of course, if none have been parsed yet; in that case, the
/// special [`SnackError::NotEnough`] _is_ returned to require the user to enter at least 1.
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
/// The returned combinator fails if the given `comb`inator cannot be applied at least once. In
/// addition, if the given `comb`inator fails fatally, that error is propagated up.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::multi2::streaming::many1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohellohellogoodbye");
/// let span2 = Span::new("<example>", "hellohelgoodbye");
/// let span3 = Span::new("<example>", "goodbye");
/// let span4 = Span::new("<example>", "");
///
/// let mut comb = many1(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(15..), vec![span1.slice(..5), span1.slice(5..10), span1.slice(10..15)]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), vec![span2.slice(..5)])));
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(many1::Many1Recoverable { what: "hello".into(), span: span3 }))
/// );
/// assert_eq!(comb.parse(span4), Err(SnackError::NotEnough { needed: None, span: span4 }));
/// ```
#[inline]
pub const fn many1<'t, F, S, C>(comb: C) -> Many1<F, S, C>
where
    F: Clone,
    S: Clone + LenBytes,
    C: Combinator2<'t, F, S>,
{
    Many1 { comb, _f: PhantomData, _s: PhantomData }
}
