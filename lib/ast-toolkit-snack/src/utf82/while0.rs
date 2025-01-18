//  WHILE 0.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 12:45:04
//  Last edited:
//    18 Jan 2025, 18:21:10
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`while0`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;
use ast_toolkit_span::range::SpanRange;

use crate::result::Result as SResult;
use crate::span::WhileUtf8;
use crate::{Combinator2, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`While0`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'t> {
    /// Something describing what we expected.
    pub what: &'t str,
}
impl<'t> Display for ExpectsFormatter<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> crate::ExpectsFormatter for ExpectsFormatter<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "{}", self.what) }
}





/***** COMBINATORS *****/
/// Actual combinator implementing [`while0()`].
#[derive(Debug, Eq, PartialEq)]
pub struct While0<'t, P, F, S> {
    predicate: P,
    what: &'t str,
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<'t, P, F, S> Combinator2<'static, F, S> for While0<'t, P, F, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    F: Clone,
    S: Clone + WhileUtf8,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let match_point: usize = input.while_utf8(SpanRange::Open, &mut self.predicate);
        Ok((input.slice(match_point..), input.slice(..match_point)))
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many characters from the start of a span as possible, as long as those
/// characters match a given predicate.
///
/// This version accepts matching none of them. See [`while1()`](super::complete::while1()) (or its
/// streaming version, [`while1()`](super::streaming::while1())) to assert at least something must
/// be matched.
///
/// # Arguments
/// - `what`: A short string describing what byte is being matched. Should finish the sentence
///   "Expected ...".
/// - `predicate`: A closure that returns true for matching characters, and false for non-matching
///   characters. All characters that are matched are returned up to the first for which
///   `predicate` returns false (if any).
///
/// # Returns
/// A combinator [`While0`] that will match the prefix of input as long as those characters match
/// the given `predicate`.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::while0;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "abÿcdef");
/// let span4 = Span::new("<example>", "hijklmn");
/// let span5 = Span::new("<example>", "");
///
/// let mut comb = while0("'a', 'b', 'c' or 'ÿ'", |c: &str| -> bool {
///     c == "a" || c == "b" || c == "c" || c == "ÿ"
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(comb.parse(span4), Ok((span4, span4.slice(..0))));
/// assert_eq!(comb.parse(span5), Ok((span5, span5.slice(..0))));
/// ```
#[inline]
pub const fn while0<'t, P, F, S>(what: &'t str, predicate: P) -> While0<'t, P, F, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    F: Clone,
    S: Clone + WhileUtf8,
{
    While0 { predicate, what, _f: PhantomData, _s: PhantomData }
}
