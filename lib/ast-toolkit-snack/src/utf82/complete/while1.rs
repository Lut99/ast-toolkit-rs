//  WHILE 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 11:40:18
//  Last edited:
//    03 Nov 2024, 19:23:47
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`while1()`]-combinator.
//

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, SpannableEq, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::span::WhileUtf8;
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Error thrown by the [`While1`]-combinator that encodes that not even one of the expected
/// characters was parsed.
pub struct While1Recoverable<F, S> {
    /// The location where no characters were found.
    pub span: Span<F, S>,
}
// NOTE: We manually implement `Debug` to avoid an unnecessary `Debug`-bound on `F` and `S`
impl<F, S> Debug for While1Recoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct("While1Recoverable");
        fmt.field("span", &self.span);
        fmt.finish()
    }
}
impl<F, S> Display for While1Recoverable<F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", While1ExpectsFormatter) }
}
impl<F, S> Error for While1Recoverable<F, S> {}
impl<F: Clone, S: Clone> Spanning<F, S> for While1Recoverable<F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S> { self.span }
}
impl<F, S: SpannableEq> Eq for While1Recoverable<F, S> {}
impl<F, S: SpannableEq> PartialEq for While1Recoverable<F, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.span == other.span }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`While1`]-combinator.
#[derive(Debug)]
pub struct While1ExpectsFormatter;
impl Display for While1ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for While1ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one specific character") }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`While1()`].
#[derive(Debug)]
pub struct While1<P, F, S> {
    predicate: P,
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<P, F, S> Combinator2<'static, F, S> for While1<P, F, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    F: Clone,
    S: Clone + WhileUtf8,
{
    type ExpectsFormatter = While1ExpectsFormatter;
    type Output = Span<F, S>;
    type Recoverable = While1Recoverable<F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { While1ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let match_point: usize = input.while_utf8(SpanRange::Open, &mut self.predicate);
        if match_point > 0 {
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            Err(SnackError::Recoverable(While1Recoverable { span: input.start_onwards() }))
        }
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many characters from the start of a span as possible, as long as those
/// characters match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::super::while0())
/// to also allow finding none.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching characters, and false for non-matching
///   characters. All characters that are matched are returned up to the first for which
///   `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While1`] that will match the prefix of input as long as those characters match
/// the given `predicate`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one character.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::complete::while1;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "abÿcdef");
/// let span4 = Span::new("<example>", "hijklmn");
/// let span5 = Span::new("<example>", "");
///
/// let mut comb = while1(|c: &str| -> bool { c == "a" || c == "b" || c == "c" || c == "ÿ" });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(while1::While1Recoverable { span: span4 }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Recoverable(while1::While1Recoverable { span: span5 }))
/// );
/// ```
#[inline]
pub const fn while1<P, F, S>(predicate: P) -> While1<P, F, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    F: Clone,
    S: Clone + WhileUtf8,
{
    While1 { predicate, _f: PhantomData, _s: PhantomData }
}
