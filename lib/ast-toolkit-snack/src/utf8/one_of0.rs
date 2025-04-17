//  ONE OF 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 12:19:21
//  Last edited:
//    19 Mar 2025, 10:39:44
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

use crate::result::Result as SResult;
use crate::span::Utf8Parsable;
use crate::{Combinator, ExpectsFormatter as _};


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
pub struct OneOf0<'t, S> {
    charset: &'t [&'t str],
    _s:      PhantomData<S>,
}
impl<'t, S> Combinator<'t, S> for OneOf0<'t, S>
where
    S: Clone + Utf8Parsable,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = Span<S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { charset: self.charset } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Try to iterate over the head to find the match
        let mut i: usize = 0;
        for c in input.graphs() {
            // Check if it's in the set
            if self.charset.contains(&c) {
                i += c.len();
                continue;
            } else {
                break;
            }
        }

        // Being `0`, this combinator always returns
        Ok((input.slice(i..), input.slice(..i)))
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
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::one_of0;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("abcdefg");
/// let span2 = Span::new("cdefghi");
/// let span3 = Span::new("abÿcdef");
/// let span4 = Span::new("hijklmn");
/// let span5 = Span::new("");
///
/// let mut comb = one_of0(&["a", "b", "c", "ÿ"]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(comb.parse(span4), Ok((span4.slice(0..), span4.slice(..0))));
/// assert_eq!(comb.parse(span5), Ok((span5.slice(0..), span5.slice(..0))));
/// ```
#[inline]
pub const fn one_of0<'t, S>(charset: &'t [&'t str]) -> OneOf0<'t, S>
where
    S: Clone + Utf8Parsable,
{
    OneOf0 { charset, _s: PhantomData }
}
