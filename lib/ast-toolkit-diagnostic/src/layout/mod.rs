//  MOD.rs
//    by Lut99
//
//  Description:
//!   Defines everything relating to layout out source snippet annotations.
//!
//!   This is a _very_ complex process. To somewhat harness it, we've defined
//!   the following pipeline of operations to perform to make it happen:
//!   1. First, we cut up a main [`Span`](ast_toolkit_span::Span) into plain
//!      areas,
//!      [highlighted areas](crate::annotations::AnnotationInnerHighlight) and
//!      [suggestion areas](crate::annotations::AnnotationInnerSuggestion).
//!      Non-plain areas are annotated with information needed to render the
//!      annotation.
//!   2. Then we use a [`Layouter`] to render the source to some particular
//!      format, e.g. UTF-8 text or a hex view of some bytes, stored in a
//!      [`Buffer`]. It also applies suggestions.
//!   3. Then we use a generic annotation layouting algorithm to place all
//!      inline comments into the rendered visual area.
//

// Declare the modules
pub mod buffer;
pub mod buffers;
pub mod layouters;
