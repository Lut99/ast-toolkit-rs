//  ONE OF 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 12:19:21
//  Last edited:
//    03 Nov 2024, 19:22:55
//  Auto updated?
//    Yes
//
//  Description:
//!   Implemens the [`one_of1()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, SpannableEq, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::span::OneOfUtf8;
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Error thrown by the [`OneOf1`]-combinator that encodes that not even one of the expected
/// characters was parsed.
pub struct OneOf1Recoverable<'t, F, S> {
    /// The set of characters to one of.
    pub charset: &'t [&'t str],
    /// The location where no characters were found.
    pub span:    Span<F, S>,
}
// NOTE: We manually implement `Debug` to avoid an unnecessary `Debug`-bound on `F` and `S`
impl<'t, F, S> Debug for OneOf1Recoverable<'t, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct("OneOf1Recoverable");
        fmt.field("charset", &self.charset);
        fmt.field("span", &self.span);
        fmt.finish()
    }
}
impl<'t, F, S> Display for OneOf1Recoverable<'t, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", OneOf1ExpectsFormatter { charset: self.charset }) }
}
impl<'t, F, S> Error for OneOf1Recoverable<'t, F, S> {}
impl<'t, F: Clone, S: Clone> Spanning<F, S> for OneOf1Recoverable<'t, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S> { self.span }
}
impl<'t, F, S: SpannableEq> Eq for OneOf1Recoverable<'t, F, S> {}
impl<'t, F, S: SpannableEq> PartialEq for OneOf1Recoverable<'t, F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.span == other.span }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf1`]-combinator.
#[derive(Debug)]
pub struct OneOf1ExpectsFormatter<'t> {
    /// The charset that we expected one of.
    pub(crate) charset: &'t [&'t str],
}
impl<'t> Display for OneOf1ExpectsFormatter<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for OneOf1ExpectsFormatter<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "at least one of ")?;
        for i in 0..self.charset.len() {
            if i == 0 {
                // SAFETY: Loops prevents us from going outside of charset's length
                write!(f, "{:?}", unsafe { self.charset.get_unchecked(i) })?;
            } else if i < self.charset.len() - 1 {
                // SAFETY: Loops prevents us from going outside of charset's length
                write!(f, ", {:?}", unsafe { self.charset.get_unchecked(i) })?;
            } else {
                // SAFETY: Loops prevents us from going outside of charset's length
                write!(f, " or {:?}", unsafe { self.charset.get_unchecked(i) })?;
            }
        }
        Ok(())
    }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`one_of1()`].
#[derive(Debug)]
pub struct OneOf1<'t, F, S> {
    charset: &'t [&'t str],
    _f:      PhantomData<F>,
    _s:      PhantomData<S>,
}
impl<'t, F, S> Combinator2<'t, F, S> for OneOf1<'t, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type ExpectsFormatter = OneOf1ExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = OneOf1Recoverable<'t, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { OneOf1ExpectsFormatter { charset: self.charset } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let match_point: usize = input.one_of_utf8(SpanRange::Open, self.charset);
        if match_point > 0 {
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            Err(SnackError::Recoverable(OneOf1Recoverable { charset: self.charset, span: input.start_onwards() }))
        }
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many characters from the start of a span as possible, as long as those
/// characters are in the set of to-be-searched-for characters.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`](super::super::one_of0())
/// to also allow finding none.
///
/// # Arguments
/// - `charset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A combinator [`OneOf1`] that will match the prefix of input as long as those characters are in
/// `charset`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one character.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::one_of1;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "abÿcdef");
/// let span4 = Span::new("<example>", "hijklmn");
/// let span5 = Span::new("<example>", "");
///
/// let mut comb = one_of1(&["a", "b", "c", "ÿ"]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(one_of1::OneOf1Recoverable {
///         charset: &["a", "b", "c", "ÿ"],
///         span:    span4,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Recoverable(one_of1::OneOf1Recoverable {
///         charset: &["a", "b", "c", "ÿ"],
///         span:    span5,
///     }))
/// );
/// ```
#[inline]
pub const fn one_of1<'t, F, S>(charset: &'t [&'t str]) -> OneOf1<'t, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    OneOf1 { charset, _f: PhantomData, _s: PhantomData }
}
