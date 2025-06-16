//  ONE OF 1.rs
//    by Lut99
//
//  Created:
//    02 Nov 2024, 12:19:21
//  Last edited:
//    08 May 2025, 11:55:33
//  Auto updated?
//    Yes
//
//  Description:
//!   Implemens the [`one_of1()`]-combinator.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, SpannableUtf8, Spanning};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ParseError};


/***** ERRORS *****/
/// Error thrown by the [`OneOf1`]-combinator that encodes that not even one of the expected
/// characters was parsed.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'c, 's, S>, bound = (S: Spannable<'s>))]
pub struct Recoverable<'c, S> {
    /// The set of characters to one of.
    pub charset: &'c [&'c str],
    /// The location where no characters were found.
    pub span:    Span<S>,
}
impl<'c, S> Display for Recoverable<'c, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter { charset: self.charset }) }
}
impl<'c, 's, S: Spannable<'s>> Error for Recoverable<'c, S> {}
impl<'c, S: Clone> Spanning<S> for Recoverable<'c, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<'c, 's, S: Clone + Spannable<'s>> ParseError<S> for Recoverable<'c, S> {
    #[inline]
    fn more_might_fix(&self) -> bool { self.span.is_empty() }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> {
        if self.more_might_fix() { Some(self.charset.iter().copied().map(str::len).min().unwrap_or(0)) } else { None }
    }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf1`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'c> {
    /// The charset that we expected one of.
    pub charset: &'c [&'c str],
}
impl<'c> Display for ExpectsFormatter<'c> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<'c> crate::ExpectsFormatter for ExpectsFormatter<'c> {
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
pub struct OneOf1<'c, S> {
    charset: &'c [&'c str],
    _s:      PhantomData<S>,
}
impl<'c, 's, 'a, S> Combinator<'a, 's, S> for OneOf1<'c, S>
where
    'c: 'a,
    S: Clone + SpannableUtf8<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'c, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { charset: self.charset } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Return if there's at least one
        let split: usize = input.match_utf8_while(|c| self.charset.contains(&c));
        if split > 0 {
            Ok((input.slice(split..), input.slice(..split)))
        } else {
            Err(SnackError::Recoverable(Recoverable { charset: self.charset, span: input }))
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
/// The returned combinator fails if it did not match at least one character. If this match failed
/// because end-of-input was reached, then this fails with a [`SnackError::NotEnough`] instead.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::one_of1;
/// use ast_toolkit_snack::{Combinator as _, ParseError as _};
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("abcdefg");
/// let span2 = Span::new("cdefghi");
/// let span3 = Span::new("abÿcdef");
/// let span4 = Span::new("hijklmn");
/// let span5 = Span::new("");
///
/// let mut comb = one_of1(&["a", "b", "c", "ÿ"]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
///
/// let err = comb.parse(span4);
/// assert_eq!(
///     err,
///     Err(SnackError::Recoverable(one_of1::Recoverable {
///         charset: &["a", "b", "c", "ÿ"],
///         span:    span4,
///     }))
/// );
/// assert!(!err.unwrap_err().more_might_fix());
///
/// let err = comb.parse(span5);
/// assert_eq!(
///     err,
///     Err(SnackError::Recoverable(one_of1::Recoverable {
///         charset: &["a", "b", "c", "ÿ"],
///         span:    span5,
///     }))
/// );
/// let err = err.unwrap_err();
/// assert!(err.more_might_fix());
/// assert_eq!(err.needed_to_fix(), Some(1));
/// ```
#[inline]
pub const fn one_of1<'c, 's, S>(charset: &'c [&'c str]) -> OneOf1<'c, S>
where
    S: Clone + SpannableUtf8<'s>,
{
    OneOf1 { charset, _s: PhantomData }
}
