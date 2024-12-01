//  MANY 1.rs
//    by Lut99
//
//  Created:
//    01 Dec 2024, 20:50:18
//  Last edited:
//    01 Dec 2024, 21:17:02
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`many1()`]-combinator.
//

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableEq, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::span::LenBytes;
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Defines the error that is thrown by [`Many1`] when there isn't any.
pub struct Many1Recoverable<F, S> {
    // NOTE: The `what` is a little hacky. But using the real formatter here is VERY inconvenient.
    //       Open to ideas!
    /// Some thing describing what we expected.
    pub what: String,
    /// The span where the error occurred.
    pub span: Span<F, S>,
}
// NOTE: We don't derive this, as the macro automatically applies unnecessary [`Debug`] bounds on
// `F` and `S`
impl<F, S> Debug for Many1Recoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct("Many1Recoverable");
        fmt.field("span", &self.span);
        fmt.finish()
    }
}
impl<F, S: ExpectsFormatter> Display for Many1Recoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", Many1ExpectsFormatter { fmt: &self.what }) }
}
impl<F, S: ExpectsFormatter> Error for Many1Recoverable<F, S> {}
impl<F: Clone, S: Clone> Spanning<F, S> for Many1Recoverable<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S>
    where
        Self: Sized,
    {
        self.span
    }
}
impl<F, S: SpannableEq> Eq for Many1Recoverable<F, S> {}
impl<F, S: SpannableEq> PartialEq for Many1Recoverable<F, S> {
    /// NOTE: This does not include `fmt`. Debatable, but much easier this way.
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.span == other.span }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Many1`] combinator.
#[derive(Debug)]
pub struct Many1ExpectsFormatter<E> {
    /// The thing we expect multiple times.
    pub(crate) fmt: E,
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
/// use ast_toolkit_snack::multi2::many1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::tag;
/// use ast_toolkit_snack::Combinator2 as _;
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
///     Err(SnackError::Recoverable(many1::Many1Recoverable { what: "hello".into(), span: span3 }))
/// );
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
