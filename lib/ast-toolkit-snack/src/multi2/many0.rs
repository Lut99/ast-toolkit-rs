//  MANY 0.rs
//    by Lut99
//
//  Created:
//    01 Dec 2024, 12:23:14
//  Last edited:
//    18 Jan 2025, 18:00:37
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`many0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator2, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Many0`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<F> {
    /// The thing we expect multiple times.
    pub fmt: F,
}
impl<F: crate::ExpectsFormatter> Display for ExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<F: crate::ExpectsFormatter> crate::ExpectsFormatter for ExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "multiple repetitions of ")?;
        self.fmt.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`many0()`]-combinator.
pub struct Many0<C, F, S> {
    comb: C,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C, F, S> Combinator2<'t, F, S> for Many0<C, F, S>
where
    F: Clone,
    S: Clone,
    C: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Vec<C::Output>;
    type Recoverable = Infallible;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects() } }

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
                Err(SnackError::Recoverable(_)) => return Ok((rem, res)),
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
/// Note that this combinator is OK with matching no input, and can therefore itself not fail.
/// If you want at least one, see [`many1()`](super::many1()) instead.
///
/// # Streaming
/// The many0-combinator's streamingness comes from using a streamed version of the nested
/// combinator or not. Being greedy, if no input is left after a successful parse of `comb`, this
/// will _still_ return a [`SnackError::NotEnough`]. If you want the combinator to stop parsing in
/// such a scenario instead, consider using [`few0()`](super::few0()) instead.
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
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::multi2::many0;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohellohellogoodbye");
/// let span2 = Span::new("<example>", "hellohelgoodbye");
/// let span3 = Span::new("<example>", "goodbye");
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
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::multi2::many0;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hellohello");
/// let span2 = Span::new("<example>", "hellohel");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = many0(tag("hello"));
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
pub const fn many0<'t, C, F, S>(comb: C) -> Many0<C, F, S>
where
    C: Combinator2<'t, F, S>,
    F: Clone,
    S: Clone,
{
    Many0 { comb, _f: PhantomData, _s: PhantomData }
}
