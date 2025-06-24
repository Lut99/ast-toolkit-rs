//  SPAN.rs
//    by Lut99
//
//  Description:
//!   Does [`Spanning`]-impls for the [`Punctuated`] list.
//

use std::borrow::Cow;

use ast_toolkit_span::{Span, Spannable, Spanning};

use super::Punctuated;


/***** IMPLEMENTATIONS *****/
impl<'s, V: Spanning<S>, P: Spanning<S>, S: Clone + Spannable<'s>> Spanning<S> for Punctuated<V, P> {
    #[inline]
    fn get_span(&self) -> Option<Cow<Span<S>>> {
        // Simply join everything in the land to be safe
        let mut res: Option<Cow<Span<S>>> = None;
        for (value, punct) in self {
            if let Some(span) = value.get_span() {
                res = res.and_then(|old| old.join(span.as_ref()).map(Cow::Owned));
            }
            if let Some(span) = punct.as_ref().and_then(Spanning::get_span) {
                res = res.and_then(|old| old.join(span.as_ref()).map(Cow::Owned));
            }
        }
        res
    }

    #[inline]
    fn take_span(self) -> Option<Span<S>> {
        // Simply join everything in the land to be safe
        let mut res: Option<Span<S>> = None;
        for (value, punct) in self {
            if let Some(span) = value.take_span() {
                res = res.and_then(|old| old.join(&span));
            }
            if let Some(span) = punct.as_ref().and_then(Spanning::take_span) {
                res = res.and_then(|old| old.join(&span));
            }
        }
        res
    }
}
