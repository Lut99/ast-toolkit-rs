//  COMPLETE 2.rs
//    by Lut99
//
//  Created:
//    11 Sep 2024, 17:16:33
//  Last edited:
//    19 Sep 2024, 12:00:16
//  Auto updated?
//    Yes
//
//  Description:
//!   TODO
//

use std::convert::Infallible;
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::Span;

use crate::result::SnackError;
use crate::span::MatchBytes;
use crate::utils::{comb_impl, error_impl, fmt_impl};


/***** LIBRARY *****/
error_impl! {
    #[comb = "Tag"]
    gen struct TagRecoverable<'t, F, S> {
        /// What we expected
        tag:  &'t str,
        /// Where we expected it
        span: Span<F, S>,
    } impl {
        fn fmt(&self, f: &mut Formatter) {
            write!(f, "{}", TagExpectsFormatter { tag: self.tag })
        }

        fn span(&self) where F: (Clone), S: (Clone) {
            self.span.clone()
        }
    }
}

fmt_impl! {
    #[comb = "Tag"]
    gen TagExpectsFormatter<'t> {
        tag: &'t str,
    } impl {
        fn expects_fmt(&self, f: &mut Formatter, _indent: usize) {
            write!(f, "'{}'", self.tag)
        }
    }
}

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
    /// use ast_toolkit_snack::result::SnackError;
    /// use ast_toolkit_snack::utf8::complete2::{tag, TagRecoverable};
    /// use ast_toolkit_snack::Combinator2 as _;
    /// use ast_toolkit_span::Span;
    ///
    /// let span1 = Span::new("<example>", "Hello, world!");
    /// let span2 = Span::new("<example>", "Goodbye, world!");
    ///
    /// let mut comb = tag("Hello");
    /// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
    /// assert!(matches!(comb.parse(span2), Err(SnackError::Recoverable(TagRecoverable { .. }))));
    /// ```
    gen Tag<'t, F, S> {
        tag: &'t str,
        _f:  PhantomData<F>,
        _s:  PhantomData<S>,
    } impl {
        type Formatter = TagExpectsFormatter<'t>;
        type Output = Span<F, S>;
        type Recoverable = TagRecoverable<'t, F, S>;
        type Fatal = Infallible;


        fn<'t, F, S> Expects<'t>::expects(&self: Self<'t, F, S>) {
            TagExpectsFormatter { tag: self.tag }
        }

        fn<'t, F, S> Combinator<'t, F, S>::parse(&mut self: Self<'t, F, S>, input: Span<F, S>)
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
                Err(SnackError::Recoverable(TagRecoverable { tag: self.tag, span: input.start_onwards() }))
            }
        }

        comb<'t, F, S> tag(tag: &'t str) -> Self<'t, F, S> {
            Tag { tag, _f: PhantomData, _s: PhantomData }
        }
    }
}
