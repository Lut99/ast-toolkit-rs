//  COMPLETE 2.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:16:33
//  Last edited:
//    16 Sep 2024, 11:31:22
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

use crate::result::{Error, SnackError};
use crate::span::MatchBytes;
use crate::utils::comb_impl;


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



comb_impl! {
    /// Matches a specific "tag", i.e., a sequence of UTF-8 characters.
    ///
    /// Useful for matching keywords.
    ///
    /// # Arguments
    /// - `tag`: The tag to match for.
    ///
    /// # Returns
    /// A combinator [`Tag`] that will match the prefix of input if it matches `tag`.
    ///
    /// # Fails
    /// The returned combinator fails if the prefix of the input was not `tag`.
    ///
    /// # Example
    /// ```rust
    /// use ast_toolkit_snack::error::{Common, Failure};
    /// use ast_toolkit_snack::utf8::complete::tag;
    /// use ast_toolkit_snack::{Combinator as _, Result as SResult};
    /// use ast_toolkit_span::Span;
    ///
    /// let span1 = Span::new("<example>", "Hello, world!");
    /// let span2 = Span::new("<example>", "Goodbye, world!");
    ///
    /// let mut comb = tag("Hello");
    /// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
    /// assert!(matches!(comb.parse(span2), SResult::Fail(Failure::Common(Common::TagUtf8 { .. }))));
    /// ```
    gen Tag<'t, F, S> {
        tag: &'t str,
        _f:  PhantomData<F>,
        _s:  PhantomData<S>,
    } impl {
        type Output = Span<F, S>;
        type Recoverable = ParseError<'t, F, S>;
        type Fatal = Infallible;

        gen Formatter<'t> {
            tag: &'t str,
        } impl {
            fn expects_fmt(&self, f: &mut Formatter, _indent: usize) {
                write!(f, "'{}'", self.tag)
            }
        }

        fn expects<'t>(&self) {
            TagExpectsFormatter { tag: self.tag }
        }

        fn parse<'t, F, S>(&mut self, input: Span<F, S>)
        where
            F: (Clone),
            S: (Clone + MatchBytes),
        {
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

        comb tag(tag: &'t str) {
            Tag { tag, _f: PhantomData, _s: PhantomData }
        }
    }
}
