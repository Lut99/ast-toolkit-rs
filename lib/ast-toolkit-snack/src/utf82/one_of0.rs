//  ONE OF 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 12:19:21
//  Last edited:
//    18 Jan 2025, 18:20:56
//  Auto updated?
//    Yes
//
//  Description:
//!   Implemens the [`one_of0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;
use ast_toolkit_span::range::SpanRange;

use crate::result::Result as SResult;
use crate::span::OneOfUtf8;
use crate::{Combinator2, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf0`]-combinator.
#[derive(Debug)]
pub struct ExpectsFormatter<'t> {
    /// The charset that we expected one of.
    pub charset: &'t [&'t str],
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "one or more of ")?;
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
/// Actual combinator implementing [`one_of0()`].
#[derive(Debug, Eq, PartialEq)]
pub struct OneOf0<'t, F, S> {
    charset: &'t [&'t str],
    _f:      PhantomData<F>,
    _s:      PhantomData<S>,
}
impl<'t, F, S> Combinator2<'t, F, S> for OneOf0<'t, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<F, S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { charset: self.charset } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        let match_point: usize = input.one_of_utf8(SpanRange::Open, self.charset);
        Ok((input.slice(match_point..), input.slice(..match_point)))
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many characters from the start of a span as possible, as long as those
/// characters are in the set of to-be-searched-for characters.
///
/// This version accepts matching none of them. See [`one_of1()`](super::complete::one_of1()) (or
/// its streaming version, [`one_of1()`](super::streaming::one_of1())) to assert at least something
/// must be matched.
///
/// # Arguments
/// - `charset`: A byte array(-like) that defines the set of characters we are looking for.
///
/// # Returns
/// A combinator [`OneOf0`] that will match the prefix of input as long as those characters are in
/// `charset`.
///
/// # Fails
/// The returned combinator will never fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf82::one_of0;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "abcdefg");
/// let span2 = Span::new("<example>", "cdefghi");
/// let span3 = Span::new("<example>", "abÿcdef");
/// let span4 = Span::new("<example>", "hijklmn");
/// let span5 = Span::new("<example>", "");
///
/// let mut comb = one_of0(&["a", "b", "c", "ÿ"]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(comb.parse(span4), Ok((span4, span4.slice(..0))));
/// assert_eq!(comb.parse(span5), Ok((span5, span5.slice(..0))));
/// ```
#[inline]
pub const fn one_of0<'t, F, S>(charset: &'t [&'t str]) -> OneOf0<'t, F, S>
where
    F: Clone,
    S: Clone + OneOfUtf8,
{
    OneOf0 { charset, _f: PhantomData, _s: PhantomData }
}
