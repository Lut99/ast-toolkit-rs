//  NOP.rs
//    by Lut99
//
//  Created:
//    03 Nov 2024, 19:33:56
//  Last edited:
//    03 Nov 2024, 19:37:37
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
use crate::{Combinator2, ExpectsFormatter};


/***** FORMATTERS *****/
/// Expectsformatter for the [`Nop`]-combinator.
#[derive(Debug)]
pub struct NopExpectsFormatter;
impl Display for NopExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl ExpectsFormatter for NopExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "nothing") }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`nop()`]-combinator (insofar there is any).
pub struct Nop<F, S> {
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<F, S> Combinator2<'static, F, S> for Nop<F, S> {
    type ExpectsFormatter = NopExpectsFormatter;
    type Output = ();
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { NopExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> { Ok((input, ())) }
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
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::combinator2::nop;
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