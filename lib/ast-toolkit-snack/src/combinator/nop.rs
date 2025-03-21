//  NOP.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 19:33:56
//  Last edited:
//    18 Jan 2025, 17:50:40
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`nop()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::Span;

use crate::result::Result as SResult;
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
pub struct Nop<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator<'static, F, S> for Nop<F, S> {
    type ExpectsFormatter = ExpectsFormatter;
    type Output = ();
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> { Ok((input, ())) }
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
/// let span1 = Span::new("<example>", "Hello, world!");
/// let span2 = Span::new("<example>", "Goodbye, world!");
///
/// let mut comb = nop();
/// assert_eq!(comb.parse(span1), Ok((span1, ())));
/// assert_eq!(comb.parse(span2), Ok((span2, ())));
/// ```
#[inline]
pub const fn nop<F, S>() -> Nop<F, S> { Nop { _f: PhantomData, _s: PhantomData } }
