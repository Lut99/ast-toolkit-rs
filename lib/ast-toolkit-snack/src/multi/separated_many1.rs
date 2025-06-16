//  SEPARATED MOST 1.rs
//    by Lut99
//
//  Created:
//    07 Mar 2025, 11:58:12
//  Last edited:
//    08 May 2025, 11:20:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`separated_many1()`]-combinator.
//

use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

pub use super::separated_many0::Fatal;
use crate::combinator::recognize;
use crate::result::{Expected, Result as SResult, SnackError};
use crate::{Combinator, ExpectsFormatter as _, ParseError};


/***** TYPE ALIASES *****/
/// The recoverable error returned by [`Most1`].
pub type Recoverable<O1, O2, S> = Expected<ExpectsFormatter<O1, O2>, S>;





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`SeparatedMany1`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<O1, O2> {
    /// The thing we expect multiple times.
    pub fmt: O1,
    /// The thing interleaving the thing we expected multiple times.
    pub sep: O2,
}
impl<O1: crate::ExpectsFormatter, O2: crate::ExpectsFormatter> Display for ExpectsFormatter<O1, O2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<O1: crate::ExpectsFormatter, O2: crate::ExpectsFormatter> crate::ExpectsFormatter for ExpectsFormatter<O1, O2> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "at least one repetitions of ")?;
        self.fmt.expects_fmt(f, indent)?;
        write!(f, " interleaved with ")?;
        self.sep.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`separated_many1()`]-combinator.
pub struct SeparatedMany1<C1, C2, S> {
    comb: C1,
    sep:  C2,
    _s:   PhantomData<S>,
}
impl<'c, 's, C1, C2, S> Combinator<'c, 's, S> for SeparatedMany1<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = Vec<C1::Output>;
    type Recoverable = Recoverable<C1::ExpectsFormatter, C2::ExpectsFormatter, S>;
    type Fatal = Fatal<C1::Fatal, C2::Fatal, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects(), sep: self.sep.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Parse the first element
        let mut res: Vec<C1::Output> = Vec::new();
        let mut rem: Span<S> = match self.comb.parse(input.clone()) {
            Ok((rem, elem)) => {
                if res.len() >= res.capacity() {
                    res.reserve(1 + res.len())
                }
                res.push(elem);
                rem
            },
            Err(SnackError::Recoverable(err)) => {
                return Err(SnackError::Recoverable(Expected {
                    fmt:     self.expects(),
                    fixable: if err.more_might_fix() { Some(err.needed_to_fix()) } else { None },
                    span:    input,
                }));
            },
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Comb(err))),
        };

        // Then parse as long as there are commas
        loop {
            // Try the comma first
            let sep: Span<S> = match recognize(&mut self.sep).parse(rem.clone()) {
                Ok((rem2, sep)) => {
                    rem = rem2;
                    sep
                },
                Err(SnackError::Recoverable(_)) => return Ok((rem, res)),
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Separator(err))),
            };

            // Parse the element
            match self.comb.parse(rem) {
                Ok((rem2, elem)) => {
                    if res.len() >= res.capacity() {
                        res.reserve(1 + res.len())
                    }
                    res.push(elem);
                    rem = rem2;
                },
                Err(SnackError::Recoverable(_)) => return Err(SnackError::Fatal(Fatal::TrailingSeparator { span: sep })),
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Comb(err))),
            }
        }
    }
}





/***** LIBRARY *****/
/// Applies some combinator interleaved by some separator as many times as possible until it fails,
/// greedily parsing multiple instances of the same input.
///
/// Note that this combinator requires at least 1 occurrence of the chosen combinator. If you want
/// a version that also accepts parsing none, see [`separated_many0()`](super::separated_many0())
/// instead.
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`SeparatedMany1`] that applies the given `comb`inator until it fails.
///
/// It will return the input as a [`Vec`].
///
/// # Fails
/// The returned combinator cannot fail recoverably. However, if the given `comb`inator fails
/// fatally, that error is propagated up.
///
/// # Examples
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::multi::separated_many1;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hello,hello,hellogoodbye");
/// let span2 = Span::new("hellogoodbye");
/// let span3 = Span::new("goodbye");
/// let span4 = Span::new(",hello");
/// let span5 = Span::new("hello,helgoodbye");
/// let span6 = Span::new("hel");
///
/// let mut comb = separated_many1(tag(b"hello"), tag(b","));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(17..), vec![span1.slice(..5), span1.slice(6..11), span1.slice(12..17)]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), vec![span2.slice(..5)])));
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(separated_many1::Recoverable {
///         fmt:     separated_many1::ExpectsFormatter {
///             fmt: tag::ExpectsFormatter { tag: b"hello" },
///             sep: tag::ExpectsFormatter { tag: b"," },
///         },
///         fixable: None,
///         span:    span3,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Recoverable(separated_many1::Recoverable {
///         fmt:     separated_many1::ExpectsFormatter {
///             fmt: tag::ExpectsFormatter { tag: b"hello" },
///             sep: tag::ExpectsFormatter { tag: b"," },
///         },
///         fixable: None,
///         span:    span4,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Fatal(separated_many1::Fatal::TrailingSeparator {
///         span: span5.slice(5..6),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span6),
///     Err(SnackError::Recoverable(separated_many1::Recoverable {
///         fmt:     separated_many1::ExpectsFormatter {
///             fmt: tag::ExpectsFormatter { tag: b"hello" },
///             sep: tag::ExpectsFormatter { tag: b"," },
///         },
///         fixable: Some(Some(2)),
///         span:    span6,
///     }))
/// );
/// ```
#[inline]
pub const fn separated_many1<'c, 's, C1, C2, S>(comb: C1, sep: C2) -> SeparatedMany1<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    SeparatedMany1 { comb, sep, _s: PhantomData }
}
