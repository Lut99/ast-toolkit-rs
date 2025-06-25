//  EXPECTED.rs
//    by Lut99
//
//  Description:
//!   Implements the [`expect()`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, SpanningInf as _};

use crate::result::{self, Result as SResult, SnackError};
use crate::{Combinator, ExpectsFormatter, ParseError as _};


/***** COMBINATORS *****/
/// Actual implementation of the [`expected()`]-combinator.
pub struct Expected<F, C, S> {
    /// The formatter to explain what we expected.
    fmt:  F,
    /// The wrapped combinator.
    comb: C,
    _s:   PhantomData<S>,
}
impl<'t, 's, F, C, S> Combinator<'t, 's, S> for Expected<F, C, S>
where
    F: Clone + ExpectsFormatter,
    C: Combinator<'t, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = F;
    type Output = C::Output;
    type Recoverable = result::Expected<F, S>;
    type Fatal = C::Fatal;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { self.fmt.clone() }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match self.comb.parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(result::Expected {
                fmt:     self.expects(),
                fixable: if err.more_might_fix() { Some(err.needed_to_fix()) } else { None },
                span:    err.span().into_owned(),
            })),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(err)),
        }
    }
}





/***** LIBRARY *****/
/// Combinator that will change the [`Recoverable`](Combinator::Recoverable) error type of the
/// given combinator into one that simply states something was expected.
///
/// This can greatly improve error messaging in case the parser reports a giant clutter of derived
/// expects strings.
///
/// # Arguments
/// - `fmt`: Some [`ExpectsFormatter`] that will detail what was expected instead.
/// - `comb`: Some [`Combinator`] to wrap.
///
/// # Returns
/// A combinator that will match the behaviour of `comb`.
///
/// # Fails
/// The returned combinator has the exact same failure behaviour as the given `comb`. However, if
/// `comb` fails recoverably, it returns an [`Expected`](result::Expected) instead of its original
/// error.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::error::expected;
/// use ast_toolkit_snack::result::{Expected, SnackError};
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("Hello, world!");
/// let span2 = Span::new("Goodbye, world!");
///
/// let mut comb1 = tag(b"Hello");
/// let mut comb2 = expected("Expected \"Hello\" as in, \"Hello, world!\"", tag(b"Hello"));
///
/// assert_eq!(comb1.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(comb2.parse(span1), Ok((span1.slice(5..), span1.slice(..5))));
/// assert_eq!(
///     comb1.parse(span2),
///     Err(SnackError::Recoverable(tag::Recoverable {
///         tag: b"Hello",
///         is_fixable: false,
///         span: span2,
///     }))
/// );
/// assert_eq!(
///     comb2.parse(span2),
///     Err(SnackError::Recoverable(Expected {
///         fmt:     "Expected \"Hello\" as in, \"Hello, world!\"",
///         fixable: None,
///         span:    span2,
///     }))
/// );
/// ```
#[inline]
pub const fn expected<'t, 's, F, C, S>(fmt: F, comb: C) -> Expected<F, C, S>
where
    F: Clone + ExpectsFormatter,
    C: Combinator<'t, 's, S>,
    S: Clone + Spannable<'s>,
{
    Expected { fmt, comb, _s: PhantomData }
}
