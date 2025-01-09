//  MANY 1.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 17:57:55
//  Last edited:
//    09 Jan 2025, 20:36:27
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`many1()`]-combinator.
//

use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::result::{Expected, Result as SResult, SnackError};
use crate::{Combinator2, ExpectsFormatter};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Many1`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct Many1ExpectsFormatter<E> {
    /// The thing we expect multiple times.
    pub fmt: E,
}
impl<E: ExpectsFormatter> Display for Many1ExpectsFormatter<E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<E: ExpectsFormatter> ExpectsFormatter for Many1ExpectsFormatter<E> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "at least one repetition of ")?;
        self.fmt.expects_fmt(f, indent)
    }
}





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
    S: Clone,
    C: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = Many1ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Vec<C::Output>;
    type Recoverable = Expected<Many1ExpectsFormatter<C::ExpectsFormatter>, F, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { Many1ExpectsFormatter { fmt: self.comb.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let mut res: Vec<C::Output> = Vec::new();
        let mut rem: Span<F, S> = input;
        loop {
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
/// Applies some other combinator as many times as possible until it fails, greedily parsing
/// multiple instances of the same input.
///
/// Note that this combinator requires at least 1 occurrence of the chosen combinator. If you want
/// a version that also accepts parsing none, see [`many0()`](super::many0()) instead.
///
/// # Streaming
/// The many1-combinator's streamingness comes from using a streamed version of the nested
/// combinator or not. Being greedy, if no input is left after a successful parse of `comb`, this
/// will _still_ return a [`SnackError::NotEnough`]. If you want the combinator to stop parsing in
/// such a scenario instead, consider using [`few1()`](super::few1()) instead.
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
/// # Examples
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::multi2::many1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohellohellogoodbye");
/// let span2 = Span::new("<example>", "hellohelgoodbye");
/// let span3 = Span::new("<example>", "goodbye");
///
/// let mut comb = many1(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(15..), vec![span1.slice(..5), span1.slice(5..10), span1.slice(10..15)]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), vec![span2.slice(..5)])));
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(many1::Many1Recoverable {
///         what: tag::TagExpectsFormatter { tag: "hello" },
///         span: span3,
///     }))
/// );
/// ```
///
/// Another example which shows the usage w.r.t. unexpected end-of-files in streaming contexts:
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::multi2::many1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohello");
/// let span2 = Span::new("<example>", "hellohel");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = many1(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::NotEnough { needed: Some(5), span: span1.slice(10..) })
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::NotEnough { needed: Some(2), span: span2.slice(8..) })
/// );
/// assert_eq!(comb.parse(span3), Err(SnackError::NotEnough { needed: Some(5), span: span3 }));
/// ```
#[inline]
pub const fn many1<'t, F, S, C>(comb: C) -> Many1<F, S, C>
where
    F: Clone,
    S: Clone,
    C: Combinator2<'t, F, S>,
{
    Many1 { comb, _f: PhantomData, _s: PhantomData }
}
