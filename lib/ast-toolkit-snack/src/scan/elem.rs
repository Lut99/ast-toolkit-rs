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

use crate::auxillary::Expected;
use crate::scan::while1;
use crate::span::{Source, Span};
use crate::spec::{Combinator, SResult, SnackError};


/***** TYPE ALIASES *****/
/// Defines recoverable errors emitted by the [`Elem`]-combinator.
pub type Recoverable<'c> = Expected<ExpectsFormatter<'c>>;





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
pub struct Elem<'c, P, S: ?Sized> {
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
    S: 's + ?Sized + Source,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<'s, S>;
    type Recoverable = Recoverable<'c>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn try_parse(&mut self, input: Span<'s, S>) -> Result<SResult<'s, Self::Output, Self::Recoverable, Self::Fatal, S>, S::Error> {
        // Implemented as a while1() with a predicate that limits the consumption
        let mut first: bool = true;
        let pred = &mut self.pred;
        Ok(while1(self.what, |elem| {
            if first {
                first = false;
                pred(elem)
            } else {
                false
            }
        })
        .try_parse(input)?
        .map_err(|err| match err {
            SnackError::Recoverable(err) => SnackError::Recoverable(Recoverable { fmt: self.expects(), fixable: err.fixable, loc: err.loc }),
            SnackError::Fatal(_) => unreachable!(),
        }))
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
/// use ast_toolkit_snack::scan::elem;
/// use ast_toolkit_snack::span::Span;
/// use ast_toolkit_snack::{Combinator as _, SnackError};
///
/// let span1 = Span::new("A");
/// let span2 = Span::new("AA");
/// let span3 = Span::new("a");
/// let span4 = Span::new("");
///
/// let mut comb = elem("uppercase letter", |b| *b >= b'A' && *b <= b'Z');
/// assert_eq!(comb.parse(span1), Ok((span1.slice(1..), span1.slice(..1))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(elem::Recoverable {
///         fmt:     elem::ExpectsFormatter { what: "uppercase letter" },
///         fixable: None,
///         loc:     span3.loc(),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(elem::Recoverable {
///         fmt:     elem::ExpectsFormatter { what: "uppercase letter" },
///         fixable: Some(Some(1)),
///         loc:     span4.loc(),
///     }))
/// );
/// ```
#[inline]
pub const fn elem<'s, 'c, P, S>(what: &'c str, pred: P) -> Elem<'c, P, S>
where
    P: FnMut(&'s S::Elem) -> bool,
    S: 's + ?Sized + Source,
{
    Elem { what, pred, _s: PhantomData }
}
