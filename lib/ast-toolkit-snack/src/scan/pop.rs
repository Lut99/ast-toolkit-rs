//  POP.rs
//    by Lut99
//
//  Description:
//!   Implements the [`pop()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

#[cfg(debug_assertions)]
use crate::asserts::assert_infallible_fatal_value;
use crate::auxillary::Expected;
use crate::scan::while1;
use crate::span::{Source, Span};
use crate::spec::{Combinator, SResult, SnackError};


/***** ERRORS *****/
/// Defines recoverable errors for the [`Pop`]-combinator.
pub type Recoverable = Expected<ExpectsFormatter>;





/***** EXPECTS FORMATTERS *****/
/// Defines the formatter of the expects-string for the [`Pop`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter;
impl Display for ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl crate::ExpectsFormatter for ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "anything") }
}





/***** COMBINATORS *****/
/// Actual implementation of [`pop()`].
pub struct Pop<S: ?Sized> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for Pop<S>
where
    S: 's + ?Sized + Source,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = &'s S::Elem;
    type Recoverable = Recoverable;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn try_parse(&mut self, input: Span<'s, S>) -> Result<SResult<'s, Self::Output, Self::Recoverable, Self::Fatal, S>, S::Error> {
        // Assert that the while1-combinator is not fallible in fatal
        let mut first = true;
        let mut comb = while1("<IGNORED>", |_| {
            if first {
                first = false;
                true
            } else {
                false
            }
        });
        #[cfg(debug_assertions)]
        assert_infallible_fatal_value(&comb);

        // Then parse using a predicate that pops at most one thing (and `while1` pops at least one)
        Ok(match comb.try_parse(input)? {
            // SAFETY: We unwrap this because we parsed something. But we don't unwrap it unsafely
            // because we're relying on a black box implementation to uphold this guarantee, one
            // which is NOT in control of the user.
            Ok((rem, _)) => Ok((rem, input.get(0)?.unwrap())),
            Err(SnackError::Recoverable(err)) => {
                Err(SnackError::Recoverable(Recoverable { fmt: self.expects(), fixable: Some(Some(1)), loc: err.loc }))
            },
        })
    }
}





/***** LIBRARY *****/
/// Pops \*any\* element from the input, but only one.
///
/// # Returns
/// A combinator that will simply advance the input by 1.
///
/// # Fails
/// The returned combinator only fails recoverably if the input is empty. It never fails fatally.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::scan::pop;
/// use ast_toolkit_snack::span::Span;
/// use ast_toolkit_snack::{Combinator as _, SnackError};
///
/// let span1 = Span::new("abc");
/// let span2 = Span::new("");
///
/// let mut comb = pop();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(1..), &b'a')));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(pop::Recoverable {
///         fmt:     pop::ExpectsFormatter,
///         fixable: Some(Some(1)),
///         loc:     span2.loc(),
///     }))
/// );
/// ```
///
/// Note that, in the case of strings, this function works on _bytes_, not graphemes. Hence:
/// ```rust
/// # use ast_toolkit_snack::Combinator as _;
/// # use ast_toolkit_snack::SnackError;
/// # use ast_toolkit_snack::scan::pop;
/// # use ast_toolkit_snack::span::Span;
/// let span = Span::new("ÿ");
///
/// let mut comb = pop();
/// // Neither are valid UTF-8 anymore
/// assert_eq!(comb.parse(span), Ok((span.slice(1..), &195)));
/// assert_eq!(span.slice(1..).value(), &[191]);
/// ```
#[inline]
pub const fn pop<S>() -> Pop<S>
where
    S: ?Sized + Source,
{
    Pop { _s: PhantomData }
}
