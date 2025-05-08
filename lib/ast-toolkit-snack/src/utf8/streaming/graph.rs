//  GRAPH.rs
//    by Lut99
//
//  Created:
//    30 Apr 2025, 09:20:08
//  Last edited:
//    08 May 2025, 13:18:02
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a combinator that will consume exactly one character if it
//!   matches a predicate.
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpannableUtf8};

pub use super::super::complete::graph::{ExpectsFormatter, Recoverable, graph as graph_complete};
use crate::Combinator;
use crate::result::{Result as SResult, SnackError};


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
        // Check first if there's *any* input to parse.
        if input.is_empty() {
            return Err(SnackError::NotEnough { needed: None, span: input });
        }

        // Otherwise, continue as usual
        graph_complete(self.what, &mut self.pred).parse(input)
    }
}





/***** LIBRARY *****/
/// A combinator that will parse a single character (grapheme) matching a predicate.
///
/// This is very much like [`while1()`](super::while1()), except that it will not greedily match
/// more characters beyond the first.
///
/// Note that this is the streaming version of the parser, meaning it throws the special
/// [`SnackError::NotEnough`] when end-of-file is reached. If you're not interested instreaming,
/// see [`graph()`](super::super::complete::graph()) instead.
///
/// _Note:_ For the grapheme parser, the [`SnackError::NotEnough`] does not provide a size hint.
/// This because graphemes of arbitrary byte size may be expected by the `pred`icate. In general,
/// though, unicode characters are at most 4 bytes wide, so it's a safe default to assume (but it's
/// not a minimum).
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
/// use ast_toolkit_snack::utf8::streaming::graph;
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
/// assert_eq!(comb.parse(span3), Err(SnackError::NotEnough { needed: None, span: span3 }));
/// ```
#[inline]
pub const fn graph<'s, 'c, P, S>(what: &'c str, pred: P) -> Graph<'c, P, S>
where
    P: for<'a> FnMut(&'a str) -> bool,
    S: Clone + SpannableUtf8<'s>,
{
    Graph { what, pred, _s: PhantomData }
}
