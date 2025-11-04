//  ONE OF 1.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 22:34:02
//  Last edited:
//    08 May 2025, 11:31:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`one_of1()`]-combinator.
//

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

#[cfg(debug_assertions)]
use crate::asserts::assert_infallible_fatal_value;
use crate::auxillary::Expected;
use crate::scan::while1;
use crate::span::{Source, Span};
use crate::spec::{Combinator, SResult, SnackError};


/***** ERRORS *****/
/// Error thrown by the [`OneOf1`]-combinator that encodes that not even one of the expected
/// bytes was parsed.
pub type Recoverable<'c, T> = Expected<ExpectsFormatter<'c, T>>;





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`OneOf0`] combinator.
#[derive(Eq, PartialEq)]
pub struct ExpectsFormatter<'c, T> {
    /// The set of elements we expect one of.
    pub set: &'c [T],
}
impl<'c, T: Display> Debug for ExpectsFormatter<'c, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        struct ListDisplayFormatter<'c, T>(&'c [T]);
        impl<'c, T: Display> Debug for ListDisplayFormatter<'c, T> {
            #[inline]
            fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
                let mut fmt = f.debug_list();
                for entry in self.0 {
                    fmt.entry(&entry.to_string());
                }
                fmt.finish()
            }
        }


        let Self { set } = self;
        let mut fmt = f.debug_struct(std::any::type_name::<Self>());
        fmt.field("set", &ListDisplayFormatter(set));
        fmt.finish()
    }
}
impl<'c, T: Display> Display for ExpectsFormatter<'c, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::spec::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<'c, T: Display> crate::spec::ExpectsFormatter for ExpectsFormatter<'c, T> {
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "at least one of ")?;
        for i in 0..self.set.len() {
            if i > 0 {
            } else if i < self.set.len() - 1 {
                // SAFETY: Loops prevents us from going outside of byteset's length
                write!(f, ", ")?;
            } else {
                // SAFETY: Loops prevents us from going outside of byteset's length
                write!(f, " or ")?;
            }
            // SAFETY: Loops prevents us from going outside of byteset's length
            <T as Display>::fmt(unsafe { self.set.get_unchecked(i) }, f)?;
        }
        Ok(())
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`one_of1()`]-combinator.
pub struct OneOf1<'c, T, S: ?Sized> {
    /// The set of elements to one of.
    set: &'c [T],
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s:  PhantomData<S>,
}
// NOTE: This lifetime trick will tell Rust that the impl is actually not invariant, but accepts
// any smaller lifetime than `'b`.
impl<'c, 's, 'a, S> Combinator<'a, 's, S> for OneOf1<'c, S::Elem, S>
where
    'c: 'a,
    S: 's + ?Sized + Source,
    S::Elem: Display + PartialEq,
{
    type ExpectsFormatter = ExpectsFormatter<'c, S::Elem>;
    type Output = Span<'s, S>;
    type Recoverable = Recoverable<'c, S::Elem>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { set: self.set } }

    #[inline]
    fn try_parse(&mut self, input: Span<'s, S>) -> Result<SResult<'s, Self::Output, Self::Recoverable, Self::Fatal, S>, S::Error> {
        // Assert it is indeed infallible
        let mut comb = while1("<IGNORED>", |elem| self.set.contains(elem));
        #[cfg(debug_assertions)]
        assert_infallible_fatal_value(&comb);

        // Wrap a while as one_of
        Ok(comb.try_parse(input)?.map_err(|err| match err {
            SnackError::Recoverable(err) => SnackError::Recoverable(Recoverable {
                fmt:     self.expects(),
                fixable: if input.is_empty() { Some(Some(1)) } else { None },
                loc:     err.loc,
            }),
        }))
    }
}





/***** LIBRARY *****/
/// Will attempt to match as many elements from the start of a span as possible, as long as those
/// are in the set of to-be-searched-for elements.
///
/// This version does _not_ accept matching none of them. See [`one_of0()`](super::one_of0()) to
/// also allow finding none.
///
/// # Arguments
/// - `set`: An array(-like) that defines the set of elements we are looking for.
///
/// # Returns
/// A combinator [`OneOf1`] that will match the prefix of input as long as those elements are in
/// `set`.
///
/// # Fails
/// The returned combinator fails if it did not match at least one element.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::scan::one_of1;
/// use ast_toolkit_snack::span::Span;
/// use ast_toolkit_snack::{Combinator as _, ParseError as _, SnackError};
///
/// let span1 = Span::new("abcdefg");
/// let span2 = Span::new("cdefghi");
/// let span3 = Span::new("abÿcdef");
/// let span4 = Span::new("hijklmn");
/// let span5 = Span::new("");
///
/// // Note: the magic numbers below are the two bytes made up by "ÿ"
/// let mut comb = one_of1(&[b'a', b'b', b'c', 191, 195]);
/// assert_eq!(comb.parse(span1), Ok((span1.slice(3..), span1.slice(..3))));
/// assert_eq!(comb.parse(span2), Ok((span2.slice(1..), span2.slice(..1))));
/// assert_eq!(comb.parse(span3), Ok((span3.slice(5..), span3.slice(..5))));
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(one_of1::Recoverable {
///         fmt:     one_of1::ExpectsFormatter { set: &[b'a', b'b', b'c', 191, 195] },
///         fixable: None,
///         loc:     span4.loc(),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Recoverable(one_of1::Recoverable {
///         fmt:     one_of1::ExpectsFormatter { set: &[b'a', b'b', b'c', 191, 195] },
///         fixable: Some(Some(1)),
///         loc:     span5.loc(),
///     }))
/// );
/// ```
#[inline]
pub const fn one_of1<'c, 's, S>(set: &'c [S::Elem]) -> OneOf1<'c, S::Elem, S>
where
    S: 's + ?Sized + Source,
    S::Elem: Display + PartialEq,
{
    OneOf1 { set, _s: PhantomData }
}
