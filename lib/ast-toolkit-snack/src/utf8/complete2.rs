//  COMPLETE 2.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:16:33
//  Last edited:
//    12 Sep 2024, 16:42:49
//  Auto updated?
//    Yes
//
//  Description:
//!   ```rust
//!   todo!();
//!   ```
//

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, Spanning};

use crate::result::{Error, Result as SResult, SnackError};
use crate::span::MatchBytes;
use crate::{Combinator2, Expects, ExpectsFormatter};


/***** LIBRARY *****/
pub struct ParseError<'t, F, S> {
    /// What we expected
    tag:  &'t str,
    /// Where we expected it
    span: Span<F, S>,
}
impl<'t, F, S> Debug for ParseError<'t, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let mut fmt = f.debug_struct("ParseError");
        fmt.field("tag", &self.tag);
        fmt.field("span", &self.span);
        fmt.finish()
    }
}
impl<'t, F, S> Display for ParseError<'t, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", TagExpectsFormatter { tag: self.tag }) }
}
impl<'t, F: Clone, S: Clone> Spanning<F, S> for ParseError<'t, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }
}
impl<'t, F: Clone, S: Clone> Error<F, S> for ParseError<'t, F, S> {}



/// Formats the expects string for the [`Tag`] combinator.
#[derive(Debug)]
pub struct TagExpectsFormatter<'t> {
    /// The string we expected.
    tag: &'t str,
}
impl<'t> Display for TagExpectsFormatter<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for TagExpectsFormatter<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "'{}'", self.tag) }
}



/// Combinator returned by [`tag()`].
pub struct Tag<'t, F, S> {
    _f:  PhantomData<F>,
    _s:  PhantomData<S>,
    tag: &'t str,
}
impl<'t, F, S> Expects<'t> for Tag<'t, F, S> {
    type Formatter = TagExpectsFormatter<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { TagExpectsFormatter { tag: self.tag } }
}
impl<'t, F, S> Combinator2<'t, F, S> for Tag<'t, F, S>
where
    F: Clone,
    S: Clone + MatchBytes,
{
    type Output = Span<F, S>;
    type Recoverable = ParseError<'t, F, S>;
    type Fatal = Infallible;

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // See if we can parse the input
        let tag: &'t [u8] = self.tag.as_bytes();
        let match_point: usize = input.match_bytes(SpanRange::Open, tag);
        if match_point >= tag.len() {
            // Matched the entire tag
            #[cfg(debug_assertions)]
            assert!(match_point == tag.len());
            Ok((input.slice(match_point..), input.slice(..match_point)))
        } else {
            // Didn't match the entire tag
            Err(SnackError::Recoverable(ParseError { tag: self.tag, span: input.start_onwards() }))
        }
    }
}
