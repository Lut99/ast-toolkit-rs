//  ONE OF 0.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:09:36
//  Last edited:
//    08 May 2025, 11:34:08
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`one_of0()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::fmt::{ElemDisplay, ElemDisplayFormatter};
use crate::result::Result as SResult;
use crate::{Combinator, ExpectsFormatter as _};


/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf0`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'b, T> {
    /// The set of elements we expect one of.
    pub set: &'b [T],
}
impl<'b, T: Debug + ElemDisplay> Display for ExpectsFormatter<'b, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'b, T: Debug + ElemDisplay> crate::ExpectsFormatter for ExpectsFormatter<'b, T> {
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "one of ")?;
        for i in 0..self.set.len() {
            if i == 0 {
                // SAFETY: Loops prevents us from going outside of byteset's length
                <T as ElemDisplay>::elem_fmt(unsafe { self.set.get_unchecked(i) }, f)?;
            } else if i < self.set.len() - 1 {
                // SAFETY: Loops prevents us from going outside of byteset's length
                write!(f, ", {}", ElemDisplayFormatter(unsafe { self.set.get_unchecked(i) }))?;
            } else {
                // SAFETY: Loops prevents us from going outside of byteset's length
                write!(f, " or {}", ElemDisplayFormatter(unsafe { self.set.get_unchecked(i) }))?;
            }
        }
        Ok(())
    }
}





/***** COMBINATORS *****/
/// Actually implements the [`one_of0()`]-combinator.
pub struct OneOf0<'c, T, S> {
    /// The set of elements to one of.
    set: &'c [T],
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
impl<'c, 's, 'a, S> Combinator<'a, 's, S> for OneOf0<'c, S::Elem, S>
where
    'c: 'a,
    S: Clone + Spannable<'s>,
    S::Elem: Debug + ElemDisplay + PartialEq,
{
    type ExpectsFormatter = ExpectsFormatter<'c, S::Elem>;
    type Output = Span<S>;
    type Recoverable = Infallible;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { set: self.set } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Return if there's at least one
        let split: usize = input.match_while(|elem| self.set.contains(elem));
        Ok((input.slice(split..), input.slice(..split)))
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many elements from the start of a span as possible, as long as those
/// elements are in the set of to-be-searched-for elements.
///
/// This version accepts matching none of them. See [`one_of1()`](super::one_of1()) to assert at
/// least something must be matched.
///
/// # Arguments
/// - `set`: An array(-like) that defines the set of elements we are looking for.
///
/// # Returns
/// A combinator [`OneOf0`] that will match the prefix of input as long as those elements are in
/// `set`.
///
/// # Fails
/// The returned combinator will never fail.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::one_of0;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("abcdefg");
/// let span2 = Span::new("cdefghi");
/// let span3 = Span::new("abÿcdef");
/// let span4 = Span::new("hijklmn");
/// let span5 = Span::new("");
///
/// // Note: the magic numbers below are the two bytes made up by "ÿ"
/// let mut comb = one_of0(&[b'a', b'b', b'c', 191, 195]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(comb.parse(span4), Ok((span4.slice(0..), span4.slice(..0))));
/// assert_eq!(comb.parse(span5), Ok((span5.slice(0..), span5.slice(..0))));
/// ```
#[inline]
pub const fn one_of0<'c, 's, S>(set: &'c [S::Elem]) -> OneOf0<'c, S::Elem, S>
where
    S: Clone + Spannable<'s>,
    S::Elem: Debug + ElemDisplay + PartialEq,
{
    OneOf0 { set, _s: PhantomData }
}
