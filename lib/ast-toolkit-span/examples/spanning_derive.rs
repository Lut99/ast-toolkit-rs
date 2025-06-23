//  SPANNING DERIVE.rs
//    by Lut99
//
//  Description:
//!   Showcases using the [`ast_toolkit_snack_derive`] procedurarl macros for automatically
//!   generating [`Spanning`] impls.
//

use std::borrow::Cow;

use ast_toolkit_span::{Span, Spanning, SpanningInf, SpanningMut, SpanningRef};


/***** EXAMPLE AST *****/
/// The default usage. Simply annotate a struct with a `span` field, and the derive macro does the
/// rest!
#[derive(Spanning, SpanningInf, SpanningRef, SpanningMut)]
#[spanning(crate = ast_toolkit_span)]
pub struct Token1<S> {
    span: Span<S>,
}



/// You can also refer to a field with another name.
#[derive(Spanning, SpanningInf, SpanningRef, SpanningMut)]
#[spanning(crate = ast_toolkit_span)]
pub struct Token2<S> {
    #[span]
    inner_span: Span<S>,
}



/// Or unnamed fields!
#[derive(Spanning, SpanningInf, SpanningRef, SpanningMut)]
#[spanning(crate = ast_toolkit_span)]
pub struct Token3<S>(#[span] Span<S>);



/// This token is an amalgamation of different tokens
#[derive(Spanning)]
#[spanning(crate = ast_toolkit_span)]
pub struct Token4<S> {
    #[span]
    span1: Span<S>,
    #[span(join)]
    span2: Span<S>,
}



/// Actually works for anything `Spanning`
#[derive(Spanning, SpanningInf, SpanningRef, SpanningMut)]
#[spanning(crate = ast_toolkit_span)]
pub struct Token5<S> {
    #[span]
    token: Token3<S>,
}



/// Enums too!
#[derive(Spanning, SpanningInf, SpanningRef, SpanningMut)]
#[spanning(crate = ast_toolkit_span)]
pub enum Token6<S> {
    One(Span<S>),
    Two { span: Span<S> },
}



/// Finally, there is the special "staircase" impl. This will try all fields in-order, until we
/// find two (once from the front, once from the back) that _isn't_ `None`.
#[derive(Spanning)]
#[spanning(crate = ast_toolkit_span, staircase)]
pub struct Token7<S> {
    first:  Option<Span<S>>,
    second: Option<Span<S>>,
    third:  Option<Span<S>>,
}

/// You can skip fields if they aren't spans.
#[derive(Spanning)]
#[spanning(crate = ast_toolkit_span, staircase)]
pub struct Token8<S> {
    first:  Option<Span<S>>,
    #[span(skip)]
    #[allow(dead_code)]
    second: &'static str,
    third:  Option<Span<S>>,
}





/***** ENTRYPOINT *****/
fn main() {
    let span = Span::new("Hello, world!");
    let mut token = Token1 { span: span.clone() };
    assert_eq!(token.get_span(), Some(Cow::Borrowed(&span)));
    assert_eq!(token.span(), Cow::Borrowed(&span));
    assert_eq!(token.span_ref(), &span);
    assert_eq!(token.span_mut(), &span);

    let mut token = Token2 { inner_span: span.clone() };
    assert_eq!(token.get_span(), Some(Cow::Borrowed(&span)));
    assert_eq!(token.span(), Cow::Borrowed(&span));
    assert_eq!(token.span_ref(), &span);
    assert_eq!(token.span_mut(), &span);

    let mut token = Token3(span.clone());
    assert_eq!(token.get_span(), Some(Cow::Borrowed(&span)));
    assert_eq!(token.span(), Cow::Borrowed(&span));
    assert_eq!(token.span_ref(), &span);
    assert_eq!(token.span_mut(), &span);

    let token = Token4 { span1: span.slice(..5), span2: span.slice(12..13) };
    assert_eq!(token.get_span(), Some(Cow::Owned(span.slice(..13))));

    let mut token = Token5 { token: Token3(span.clone()) };
    assert_eq!(token.get_span(), Some(Cow::Borrowed(&span)));
    assert_eq!(token.span(), Cow::Borrowed(&span));
    assert_eq!(token.span_ref(), &span);
    assert_eq!(token.span_mut(), &span);

    let mut token = Token6::One(span.clone());
    assert_eq!(token.get_span(), Some(Cow::Borrowed(&span)));
    assert_eq!(token.span(), Cow::Borrowed(&span));
    assert_eq!(token.span_ref(), &span);
    assert_eq!(token.span_mut(), &span);

    let token = Token7 { first: None, second: Some(span.slice(..1)), third: Some(span.slice(4..5)) };
    assert_eq!(token.get_span(), Some(Cow::Owned(span.slice(..5))));

    let token = Token8 { first: None, second: "Hello", third: Some(span.slice(..5)) };
    assert_eq!(token.get_span(), Some(Cow::Owned(span.slice(..5))));
}
