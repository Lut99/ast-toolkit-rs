//  NOP.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 19:33:56
//  Last edited:
//    22 Apr 2025, 11:42:23
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`nop()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::result::Result as SResult;
use crate::span::Parsable;
use crate::{Combinator, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// Expectsformatter for the [`Nop`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter;
impl Display for ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl crate::ExpectsFormatter for ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "nothing") }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`nop()`]-combinator (insofar there is any).
pub struct Nop<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for Nop<S>
where
    S: Clone + Spannable<'s>,
    S::Slice: Parsable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = ();
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> { Ok((input, ())) }
}





/***** LIBRARY *****/
/// Implements a no-op combinator that doesn't consume anything.
///
/// This is useful in case you're working with more general combinators that you don't want to use
/// all features of. A common case is parsing parenthesis with nothing in between them.
///
/// # Returns
/// A combinator [`Nop`] that does not consume anything but always just returns `()`.
///
/// # Fails
/// The returned combinator never fails.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::combinator::nop;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb = nop();
/// assert_eq!(comb.parse(span1), Ok((span1, ())));
/// assert_eq!(comb.parse(span2), Ok((span2, ())));
/// ```
#[inline]
pub const fn nop<S>() -> Nop<S> { Nop { _s: PhantomData } }
