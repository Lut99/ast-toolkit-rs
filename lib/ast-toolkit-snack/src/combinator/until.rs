//  UNTIL.rs
//    by Lut99
//
//  Created:
//    08 May 2025, 15:31:36
//  Last edited:
//    08 May 2025, 15:49:31
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`until()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::Combinator;
use crate::result::{Result as SResult, SnackError};


/***** FORMATTERS *****/
/// Defines the expectsformatter for the [`Until`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<F> {
    /// The formatter of the combinator that determines when we stop.
    pub fmt: F,
}
impl<F: crate::ExpectsFormatter> Display for ExpectsFormatter<F> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<F: crate::ExpectsFormatter> crate::ExpectsFormatter for ExpectsFormatter<F> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "anything until ")?;
        <F as crate::ExpectsFormatter>::expects_fmt(&self.fmt, f, indent)
    }
}





/***** COMBINATOR *****/
/// Actual implementation of the [`until()`]-combinator.
pub struct Until<C, S> {
    /// The combinator that determines when we're done.
    comb: C,
    _s:   PhantomData<S>,
}
impl<'c, 's, C, S> Combinator<'c, 's, S> for Until<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<C::ExpectsFormatter>;
    type Output = Span<S>;
    type Recoverable = Infallible;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects() } }

    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        let input_len: usize = input.len();
        let mut i: usize = 0;
        while i < input_len {
            let rem: Span<S> = input.slice(i..);
            match self.comb.parse(rem.clone()) {
                Ok((_, _)) => return Ok((rem, input.slice(..i))),
                Err(SnackError::Recoverable(_)) => {
                    i += 1;
                    continue;
                },
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(err)),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
            }
        }
        Ok((input.slice(input_len..), input.slice(..input_len)))
    }
}





/***** LIBRARY *****/
/// A combinator that will parse from the head of the input until some other combinator succeeds.
///
/// This is kind of like a meta-[`while0()`](crate::bytes::complete::while1()).
///
/// # Arguments
/// - `comb`: The other combinator to check for when we succeeded.
///
/// # Returns
/// A combinator that will parse the head of the input until `comb` succeeds. When it does, every-
/// thing _up until_ the combinator's parsed part is returned, but not including.
///
/// If `comb` never succeeds, the whole input is returned. Likewise, if it succeeds immediately,
/// then an empty slice is returned.
///
/// # Fails
/// The returned combinator never fails recoverably. It only fails fatally if the nested combinator
/// does so.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::until;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("abcd");
/// let span2 = Span::new("d");
/// let span3 = Span::new("abce");
/// let span4 = Span::new("");
///
/// let mut comb = until(tag("d"));
/// assert_eq!(comb.parse(span1).unwrap(), ((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2).unwrap(), ((span2.slice(0..), span2.slice(..0))));
/// assert_eq!(comb.parse(span3).unwrap(), ((span3.slice(4..), span3.slice(..4))));
/// assert_eq!(comb.parse(span4).unwrap(), ((span4.slice(0..), span4.slice(..0))));
/// ```
#[inline]
pub const fn until<'c, 's, C, S>(comb: C) -> Until<C, S>
where
    C: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    Until { comb, _s: PhantomData }
}
