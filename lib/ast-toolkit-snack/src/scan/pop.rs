//  POP.rs
//    by Lut99
//
//  Description:
//!   Implements the [`pop()`]-combinator.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning, SpanningInf, SpanningMut, SpanningRef};

use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ParseError};


/***** ERRORS *****/
/// Defines recoverable errors for the [`Pop`]-combinator.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
pub struct Recoverable<S> {
    /// The span where the input ended.
    pub span: Span<S>,
}
impl<S> Display for Recoverable<S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter) }
}
impl<'s, S: Spannable<'s>> Error for Recoverable<S> {}
impl<'s, S: Clone + Spannable<'s>> Spanning<S> for Recoverable<S> {
    #[inline]
    fn get_span(&self) -> Option<Cow<Span<S>>> { Some(self.span()) }
    #[inline]
    fn take_span(self) -> Option<Span<S>> { Some(self.into_span()) }
}
impl<'s, S: Clone + Spannable<'s>> SpanningInf<S> for Recoverable<S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(self.span_ref()) }
    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}
impl<'s, S: Clone + Spannable<'s>> SpanningRef<S> for Recoverable<S> {
    #[inline]
    fn span_ref(&self) -> &Span<S> { &self.span }
}
impl<'s, S: Clone + Spannable<'s>> SpanningMut<S> for Recoverable<S> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> { &mut self.span }
}
impl<'s, S: Clone + Spannable<'s>> ParseError<S> for Recoverable<S> {
    #[inline]
    fn more_might_fix(&self) -> bool { true }
    #[inline]
    fn needed_to_fix(&self) -> Option<usize> { Some(1) }
}





/***** EXPECTS FORMATTERS *****/
/// Defines the formatter of the expects-string for the [`Pop`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter;
impl Display for ExpectsFormatter {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl crate::ExpectsFormatter for ExpectsFormatter {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "anything") }
}





/***** COMBINATORS *****/
/// Actual implementation of [`pop()`].
pub struct Pop<S> {
    _s: PhantomData<S>,
}
impl<'s, S> Combinator<'static, 's, S> for Pop<S>
where
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter;
    type Output = &'s S::Elem;
    type Recoverable = Recoverable<S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        if !input.is_empty() {
            // SAFETY: We can just take the element because we asserted we aren't empty.
            Ok((input.slice(1..), &input.as_slice()[0]))
        } else {
            Err(SnackError::Recoverable(Recoverable { span: input }))
        }
    }
}





/***** LIBRARY *****/
/// Pops \*any\* element from the input, but only one.
///
/// # Returns
/// A combinator that will simply advance the input by 1.
///
/// # Fails
/// The returned combinator only fails recoverably if the input is empty. It never fails fatally.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::pop;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("abc");
/// let span2 = Span::new("");
///
/// let mut comb = pop();
/// assert_eq!(comb.parse(span1), Ok((span1.slice(1..), &b'a')));
/// assert_eq!(comb.parse(span2), Err(SnackError::Recoverable(pop::Recoverable { span: span2 })));
/// ```
///
/// Note that, in the case of strings, this function works on _bytes_, not graphemes. Hence:
/// ```rust
/// # use ast_toolkit_snack::Combinator as _;
/// # use ast_toolkit_snack::result::SnackError;
/// # use ast_toolkit_snack::scan::pop;
/// # use ast_toolkit_span::Span;
/// let span = Span::new("ÿ");
///
/// let mut comb = pop();
/// // Neither are valid UTF-8 anymore
/// assert_eq!(comb.parse(span), Ok((span.slice(1..), &195)));
/// assert_eq!(span.slice(1..).value(), &[191]);
/// ```
#[inline]
pub const fn pop<'s, S>() -> Pop<S>
where
    S: Clone + Spannable<'s>,
{
    Pop { _s: PhantomData }
}
