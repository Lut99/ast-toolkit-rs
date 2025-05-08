//  GRAPH.rs
//    by Lut99
//
//  Created:
//    30 Apr 2025, 09:20:08
//  Last edited:
//    08 May 2025, 11:46:21
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a combinator that will consume exactly one character if it
//!   matches a predicate.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableUtf8};

use crate::Combinator;
use crate::result::{Expected, Result as SResult, SnackError};


/***** TYPE ALIASES *****/
/// Defines recoverable errors emitted by the [`Graph`]-combinator.
pub type Recoverable<'c, S> = Expected<ExpectsFormatter<'c>, S>;





/***** EXPECTS ******/
/// Renders the expects-string for the [`Graph`]-combinator.
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
/// Actual implementation of [`graph()`]-.
pub struct Graph<'c, P, S> {
    /// A string describing what was expected.
    what: &'c str,
    /// The predicate for matching bytes.
    pred: P,
    _s:   PhantomData<S>,
}
impl<'s, 'c, 'a, P, S> Combinator<'a, 's, S> for Graph<'c, P, S>
where
    'c: 'a,
    P: for<'b> FnMut(&'b str) -> bool,
    S: Clone + SpannableUtf8<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'c, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // It's like while but only once
        let mut first: bool = true;
        let split: usize = input.match_utf8_while(|c| {
            if first {
                first = false;
                (self.pred)(c)
            } else {
                false
            }
        });

        // Decide what to return
        if split > 0 {
            Ok((input.slice(split..), input.slice(..split)))
        } else {
            Err(SnackError::Recoverable(Expected { fmt: self.expects(), span: input }))
        }
    }
}





/***** LIBRARY *****/
/// A combinator that will parse a single character (grapheme) matching a predicate.
///
/// This is very much like [`while1()`](super::while1()), except that it will not greedily match
/// more characters beyond the first.
///
/// Note that this is the complete version of the parser. If you're intending to stream input
/// instead, see [`graph()`](super::super::streaming::graph()).
///
/// # Arguments
/// - `what`: Some user-friendly description of what this combinator expects. Should complete:
///   "Expected one ...".
/// - `pred`: Some [predicate](FnMut) that will be used to check if the parsed grapheme is what
///   should be matched.
///
/// # Returns
/// A combinator that will parse a grapheme matching the given `pred`icate.
///
/// # Fails
/// The returned combinator fails if the input is empty or the first character does not match the
/// `pred`icate.
///
/// The returned combinator never fails fatally.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::graph;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("A");
/// let span2 = Span::new("a");
/// let span3 = Span::new("");
///
/// let mut comb = graph("uppercase letter", |c| c == c.to_uppercase());
/// assert_eq!(comb.parse(span1), Ok((span1.slice(1..), span1.slice(..1))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(graph::Recoverable {
///         fmt:  graph::ExpectsFormatter { what: "uppercase letter" },
///         span: span2,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(graph::Recoverable {
///         fmt:  graph::ExpectsFormatter { what: "uppercase letter" },
///         span: span3,
///     }))
/// );
/// ```
#[inline]
pub const fn graph<'s, 'c, P, S>(what: &'c str, pred: P) -> Graph<'c, P, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    S: Clone + SpannableUtf8<'s>,
{
    Graph { what, pred, _s: PhantomData }
}
