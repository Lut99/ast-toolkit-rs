//  BYTE.rs
//    by Lut99
//
//  Created:
//    30 Apr 2025, 09:15:34
//  Last edited:
//    08 May 2025, 11:31:43
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a combinator that will consume exactly one element if it
//!   matches a predicate.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::Combinator;
use crate::result::{Expected, Result as SResult, SnackError};


/***** TYPE ALIASES *****/
/// Defines recoverable errors emitted by the [`Elem`]-combinator.
pub type Recoverable<'c, S> = Expected<ExpectsFormatter<'c>, S>;





/***** EXPECTS ******/
/// Renders the expects-string for the [`Elem`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'c> {
    /// Something use-provided describing what was being looked for.
    pub what: &'c str,
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
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "one {}", self.what) }
}





/***** COMBINATOR *****/
/// Actual implementation of [`elem()`]-.
pub struct Elem<'c, P, S> {
    /// A string describing what was expected.
    what: &'c str,
    /// The predicate for matching elements.
    pred: P,
    _s:   PhantomData<S>,
}
impl<'s, 'c, 'a, P, S> Combinator<'a, 's, S> for Elem<'c, P, S>
where
    'c: 'a,
    P: FnMut(&'s S::Elem) -> bool,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'c, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Check first if there's *any* input to parse.
        if input.is_empty() {
            return Err(SnackError::Recoverable(Expected { fmt: self.expects(), fixable: Some(Some(1)), span: input }));
        }

        // Match the first element
        // SAFETY: We know it exists, because the span is non-empty
        let elem: &'s S::Elem = &input.as_slice()[0];
        if (self.pred)(elem) {
            Ok((input.slice(1..), input.slice(..1)))
        } else {
            Err(SnackError::Recoverable(Expected { fmt: self.expects(), fixable: None, span: input }))
        }
    }
}





/***** LIBRARY *****/
/// A combinator that will parse a single element matching a predicate.
///
/// This is very much like [`while1()`](super::while1()), except that it will not greedily match
/// more bytes beyond the first.
///
/// # Arguments
/// - `what`: Some user-friendly description of what this combinator expects. Should complete:
///   "Expected one ...".
/// - `pred`: Some [predicate](FnMut) that will be used to check if the element is what is
///   expected.
///
/// # Returns
/// A combinator that will parse an element if it matches the given `pred`icate.
///
/// # Fails
/// The returned combinator fails recoverably if the input is empty or the first byte does not
/// match the `pred`icate.
///
/// The returned combinator never fails fatally.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::elem;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("A");
/// let span2 = Span::new("a");
/// let span3 = Span::new("");
///
/// let mut comb = elem("uppercase letter", |b| *b >= b'A' && *b <= b'Z');
/// assert_eq!(comb.parse(span1), Ok((span1.slice(1..), span1.slice(..1))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(elem::Recoverable {
///         fmt:     elem::ExpectsFormatter { what: "uppercase letter" },
///         fixable: None,
///         span:    span2,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(elem::Recoverable {
///         fmt:     elem::ExpectsFormatter { what: "uppercase letter" },
///         fixable: Some(Some(1)),
///         span:    span3,
///     }))
/// );
/// ```
#[inline]
pub const fn elem<'s, 'c, P, S>(what: &'c str, pred: P) -> Elem<'c, P, S>
where
    P: FnMut(&'s S::Elem) -> bool,
    S: Clone + Spannable<'s>,
{
    Elem { what, pred, _s: PhantomData }
}
