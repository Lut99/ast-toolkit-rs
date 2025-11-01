//  WHILE 0.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:09:36
//  Last edited:
//    08 May 2025, 11:34:35
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`while0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use crate::span::{Source, Span};
use crate::spec::{Combinator, ExpectsFormatter as _, SResult};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`While0`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'t> {
    /// A description of what was while'd
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
/// Actually implements the [`while0()`]-combinator.
pub struct While0<'c, P, S: ?Sized> {
    /// The predicate used for matching.
    predicate: P,
    /// A description of what was while'd
    what: &'c str,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<'a, 'c, 's, P, S> Combinator<'a, 's, S> for While0<'c, P, S>
where
    'c: 'a,
    P: FnMut(&'s S::Elem) -> bool,
    S: 's + ?Sized + Source,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<'s, S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn try_parse(&mut self, input: Span<'s, S>) -> Result<SResult<'s, Self::Output, Self::Recoverable, Self::Fatal, S>, S::Error> {
        input.slice_while(&mut self.predicate).map(Ok)
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many bytes from the start of a span as possible, as long as those
/// bytes match a given predicate.
///
/// This version accepts matching none of them. See [`while1()`](super::while1()) to assert at
/// least something must be matched.
///
/// # Arguments
/// - `what`: A short string describing what byte is being matched. Should finish the sentence
///   "Expected ...".
/// - `predicate`: A closure that returns true for matching elements, and false for non-matching
///   ones. This combinator parsers up until (but not including) the first false, if any (the end
///   of the input otherwise).
///
/// # Returns
/// A combinator [`While0`] that will match the prefix of input as long as those elements match the
/// given `predicate`.
///
/// # Fails
/// The returned combinator will never fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::scan::while0;
/// use ast_toolkit_snack::{Combinator as _, Span};
///
/// let span1 = Span::new("abcdefg");
/// let span2 = Span::new("cdefghi");
/// let span3 = Span::new("abÿcdef");
/// let span4 = Span::new("hijklmn");
/// let span5 = Span::new("");
///
/// // Note: the magic numbers below are the two bytes made up by "ÿ"
/// let mut comb = while0("'a', 'b', 'c' or 'ÿ'", |b: &u8| -> bool {
///     *b == b'a' || *b == b'b' || *b == b'c' || *b == 191 || *b == 195
/// });
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(comb.parse(span4), Ok((span4.slice(0..), span4.slice(..0))));
/// assert_eq!(comb.parse(span5), Ok((span5.slice(0..), span5.slice(..0))));
/// ```
#[inline]
pub const fn while0<'c, 's, P, S>(what: &'c str, predicate: P) -> While0<'c, P, S>
where
    P: FnMut(&'s S::Elem) -> bool,
    S: 's + ?Sized + Source,
{
    While0 { predicate, what, _s: PhantomData }
}
