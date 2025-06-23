//  LIB.rs
//    by Lut99
//
//  Created:
//    25 Feb 2024, 11:11:33
//  Last edited:
//    08 May 2025, 10:25:38
//  Auto updated?
//    Yes
//
//  Description:
//!   Provides a [nom](https://github.com/rust-bakery/nom)-compatible Span
//!   that is used to track AST nodes to their location in the source
//!   text.
//!
//!   Used in various other parts of the ast-toolkit.
//

// Declare modules
pub mod range;
mod span;
mod spannable;
mod spanning;

// Bring some of it into the parent namespace
#[cfg(feature = "derive")]
pub use ast_toolkit_span_derive::{Spanning, SpanningInf, SpanningMut, SpanningRef};
pub use span::Span;
pub use spannable::{Spannable, SpannableBytes};
pub use spanning::{Spanning, SpanningInf, SpanningMut, SpanningRef};


/***** TESTS *****/
/// NOTE: Has to be here, because we need to resolve this crate things and we don't want a
/// mutually dependent relationship
#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::*;

    #[cfg(feature = "derive")]
    #[test]
    fn test_spanning_struct_nonspan_fields() {
        #[derive(Spanning, SpanningInf, SpanningMut, SpanningRef)]
        #[spanning(crate = crate)]
        #[allow(dead_code)]
        struct Test1<S> {
            nonspan: &'static str,
            span:    Span<S>,
        }

        #[derive(Spanning, SpanningInf, SpanningMut, SpanningRef)]
        #[spanning(crate = crate)]
        #[allow(dead_code)]
        struct Test2<S>(&'static str, #[span] Span<S>);

        let span = Span::new("test");
        assert_eq!(Test1 { nonspan: "Howdy", span: span.clone() }.get_span(), Some(Cow::Owned(span)));
        assert_eq!(Test2("Howdy", span.clone()).get_span(), Some(Cow::Owned(span)));
    }
}
