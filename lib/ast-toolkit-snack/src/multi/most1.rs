//  MOST 1.rs
//    by Lut99
//
//  Created:
//    14 Dec 2024, 17:57:55
//  Last edited:
//    22 Apr 2025, 12:02:16
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`most1()`]-combinator.
//

use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::result::{Expected, Result as SResult, SnackError};
use crate::span::Parsable;
use crate::{Combinator, ExpectsFormatter as _};


/***** TYPE ALIASES *****/
/// The recoverable error returned by [`Most1`].
pub type Recoverable<C, S> = Expected<ExpectsFormatter<C>, S>;





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Most1`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<E> {
    /// The thing we expect multiple times.
    pub fmt: E,
}
impl<E: crate::ExpectsFormatter> Display for ExpectsFormatter<E> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<E: crate::ExpectsFormatter> crate::ExpectsFormatter for ExpectsFormatter<E> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "at least one repetition of ")?;
        self.fmt.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`most1()`]-combinator.
pub struct Most1<C, S> {
    comb: C,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for Most1<C, S>
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
/// a version that also accepts parsing none, see [`most0()`](super::most0()) instead.
///
/// # Streaming
/// The most1-combinator's streamingness comes from using a streamed version of the nested
/// combinator or not. Being greedy, if no input is left after a successful parse of `comb`, this
/// will _still_ return a [`SnackError::NotEnough`]. If you want the combinator to stop parsing in
/// such a scenario instead, consider using [`many1()`](super::many1()) instead.
///
/// As a rule of thumb, use the `most`-combinators when the user indicates the end of the
/// repetitions by something concrete (e.g., expressions wrapped in parenthesis).
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`Most1`] that applies the given `comb`inator until it fails.
///
/// It will return the input as a [`Vec`].
///
/// # Fails
/// The returned combinator fails if the given `comb`inator cannot be applied at least once. In
/// addition, if the given `comb`inator fails fatally, that error is propagated up.
///
/// # Examples
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::multi::most1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hellohellohellogoodbye");
/// let span2 = Span::new("hellohelgoodbye");
/// let span3 = Span::new("goodbye");
///
/// let mut comb = most1(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(15..), vec![span1.slice(..5), span1.slice(5..10), span1.slice(10..15)]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), vec![span2.slice(..5)])));
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(most1::Recoverable {
///         fmt:  most1::ExpectsFormatter { fmt: tag::ExpectsFormatter { tag: "hello" } },
///         span: span3,
///     }))
/// );
/// ```
///
/// Another example which shows the usage w.r.t. unexpected end-of-files in streaming contexts:
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::multi::most1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hellohello");
/// let span2 = Span::new("hellohel");
/// let span3 = Span::new("");
///
/// let mut comb = most1(tag("hello"));
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::NotEnough { needed: Some(5), span: span1.slice(10..) })
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::NotEnough { needed: Some(2), span: span2.slice(8..) })
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::NotEnough { needed: Some(5), span: span3.slice(0..) })
/// );
/// ```
#[inline]
pub const fn most1<'c, 's, C, S>(comb: C) -> Most1<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
{
    Most1 { comb, _s: PhantomData }
}
