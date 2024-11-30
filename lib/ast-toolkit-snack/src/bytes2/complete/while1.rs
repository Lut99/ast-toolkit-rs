//  WHILE 1.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:42:41
//  Last edited:
//    30 Nov 2024, 22:48:52
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
use crate::span::WhileBytes;
use crate::{Combinator2, ExpectsFormatter};


/***** ERRORS *****/
/// Error thrown by the [`While1`]-combinator that encodes that not even one of the expected
/// bytes was parsed.
pub struct While1Recoverable<F, S> {
    /// The location where no bytes were found.
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
/// ExpectsFormatter for the [`While1`] combinator.
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "at least one specific byte") }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`while1()`]-combinator.
pub struct While1<F, S, P> {
    /// The predicate used for matching.
    predicate: P,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'b`.
impl<F, S, P> Combinator2<'static, F, S> for While1<F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    type ExpectsFormatter = While1ExpectsFormatter;
    type Output = Span<F, S>;
    type Recoverable = While1Recoverable<F, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { While1ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let match_point: usize = input.while_bytes(SpanRange::Open, &mut self.predicate);
        if match_point > 0 {
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            Err(SnackError::Recoverable(While1Recoverable { span: input.start_onwards() }))
        }
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those
/// bytes match a given predicate.
///
/// This version does _not_ accept matching none of them. See [`while0()`](super::super::while0())
/// to also allow finding none.
///
/// # Arguments
/// - `predicate`: A closure that returns true for matching bytes, and false for non-matching
///   bytes. All bytes that are matched are returned up to the first for which
///   `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While1`] that will match the prefix of input as long as those bytes match
/// the given `predicate`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one byte.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::bytes2::complete::while1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::<&str, &[u8]>::new("<example>", b"abcdefg");
/// let span2 = Span::<&str, &[u8]>::new("<example>", b"cdefghi");
/// let span3 = Span::<&str, &[u8]>::new("<example>", "abÿcdef".as_bytes());
/// let span4 = Span::<&str, &[u8]>::new("<example>", b"hijklmn");
/// let span5 = Span::<&str, &[u8]>::new("<example>", b"");
///
/// let mut comb =
///     while1(|b: u8| -> bool { b == b'a' || b == b'b' || b == b'c' || b == 191 || b == 195 });
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
pub const fn while1<F, S, P>(predicate: P) -> While1<F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    While1 { predicate, _f: PhantomData, _s: PhantomData }
}
