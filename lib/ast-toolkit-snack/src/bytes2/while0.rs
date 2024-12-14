//  WHILE 0.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:09:36
//  Last edited:
//    14 Dec 2024, 19:35:40
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`while0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;
use ast_toolkit_span::range::SpanRange;

use crate::result::Result as SResult;
use crate::span::WhileBytes;
use crate::{Combinator2, ExpectsFormatter};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`While0`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct While0ExpectsFormatter<'t> {
    /// A description of what was while'd
    pub what: &'t str,
}
impl<'t> Display for While0ExpectsFormatter<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for While0ExpectsFormatter<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{}", self.what) }
}





/***** COMBINATORS *****/
/// Actually implements the [`while0()`]-combinator.
pub struct While0<'t, F, S, P> {
    /// The predicate used for matching.
    predicate: P,
    /// A description of what was while'd
    what: &'t str,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<'t, F, S, P> Combinator2<'t, F, S> for While0<'t, F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    type ExpectsFormatter = While0ExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { While0ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let match_point: usize = input.while_bytes(SpanRange::Open, &mut self.predicate);
        Ok((input.slice(match_point..), input.slice(..match_point)))
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those
/// bytes match a given predicate.
///
/// This version accepts matching none of them. See [`while1()`](super::complete::while1()) (or its
/// streaming version, [`while1()`](super::streaming::while1())) to assert at least something must
/// be matched.
///
/// # Arguments
/// - `what`: A short string describing what byte is being matched. Should finish the sentence
///   "Expected ...".
/// - `predicate`: A closure that returns true for matching bytes, and false for non-matching
///   bytes. All bytes that are matched are returned up to the first for which `predicate` returns
///   false (if any).
///
/// # Returns
/// A combinator [`While0`] that will match the prefix of input as long as those bytes match the
/// given `predicate`.
///
/// # Fails
/// The returned combinator will never fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::bytes2::while0;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::<&str, &[u8]>::new("<example>", b"abcdefg");
/// let span2 = Span::<&str, &[u8]>::new("<example>", b"cdefghi");
/// let span3 = Span::<&str, &[u8]>::new("<example>", "abÿcdef".as_bytes());
/// let span4 = Span::<&str, &[u8]>::new("<example>", b"hijklmn");
/// let span5 = Span::<&str, &[u8]>::new("<example>", b"");
///
/// // Note: the magic numbers below are the two bytes made up by "ÿ"
/// let mut comb = while0("'a', 'b', 'c' or 'ÿ'", |b: u8| -> bool {
///     b == b'a' || b == b'b' || b == b'c' || b == 191 || b == 195
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(comb.parse(span4), Ok((span4, span4.slice(..0))));
/// assert_eq!(comb.parse(span5), Ok((span5, span5.slice(..0))));
/// ```
#[inline]
pub const fn while0<'t, F, S, P>(what: &'t str, predicate: P) -> While0<'t, F, S, P>
where
    F: Clone,
    S: Clone + WhileBytes,
    P: FnMut(u8) -> bool,
{
    While0 { predicate, what, _f: PhantomData, _s: PhantomData }
}
