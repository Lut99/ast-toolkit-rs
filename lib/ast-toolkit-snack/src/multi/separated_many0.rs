//  SEPARATED MOST 0.rs
//    by Lut99
//
//  Created:
//    18 Jan 2025, 18:56:39
//  Last edited:
//    08 May 2025, 11:20:53
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`separated_many0()`]-combinator.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, Spanning, SpanningInf, SpanningMut, SpanningRef};

use super::super::combinator::recognize;
use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ExpectsFormatter as _, ParseError};


/***** ERRORS *****/
/// Defines the fatal errors thrown by [`SeparatedMany0`].
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'s, E1, E2, S>, bound = (E1: r#trait, E2: r#trait, S: Spannable<'s>))]
pub enum Fatal<E1, E2, S> {
    /// The element-combinator failed fatally.
    Comb(E1),
    /// The separator-combinator failed fatally.
    Separator(E2),
    /// Lint that will explain the user they added an incorrect trailing separator.
    TrailingSeparator { span: Span<S> },
}
impl<E1: Display, E2: Display, S> Display for Fatal<E1, E2, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Comb(err) => err.fmt(f),
            Self::Separator(err) => err.fmt(f),
            Self::TrailingSeparator { span: _ } => write!(f, "Encountered trailing separator"),
        }
    }
}
impl<'s, E1: error::Error, E2: error::Error, S: Spannable<'s>> error::Error for Fatal<E1, E2, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::Comb(err) => err.source(),
            Self::Separator(err) => err.source(),
            Self::TrailingSeparator { span: _ } => None,
        }
    }
}
impl<E1: Spanning<S>, E2: Spanning<S>, S: Clone> Spanning<S> for Fatal<E1, E2, S> {
    #[inline]
    fn get_span(&self) -> Option<Cow<Span<S>>> {
        match self {
            Self::Comb(err) => err.get_span(),
            Self::Separator(err) => err.get_span(),
            Self::TrailingSeparator { span } => Some(Cow::Borrowed(span)),
        }
    }

    #[inline]
    fn take_span(self) -> Option<Span<S>> {
        match self {
            Self::Comb(err) => err.take_span(),
            Self::Separator(err) => err.take_span(),
            Self::TrailingSeparator { span } => Some(span),
        }
    }
}
impl<E1: SpanningInf<S>, E2: SpanningInf<S>, S: Clone> SpanningInf<S> for Fatal<E1, E2, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> {
        match self {
            Self::Comb(err) => err.span(),
            Self::Separator(err) => err.span(),
            Self::TrailingSeparator { span } => Cow::Borrowed(span),
        }
    }

    #[inline]
    fn into_span(self) -> Span<S> {
        match self {
            Self::Comb(err) => err.into_span(),
            Self::Separator(err) => err.into_span(),
            Self::TrailingSeparator { span } => span,
        }
    }
}
impl<E1: SpanningRef<S>, E2: SpanningRef<S>, S: Clone> SpanningRef<S> for Fatal<E1, E2, S> {
    #[inline]
    fn span_ref(&self) -> &Span<S> {
        match self {
            Self::Comb(err) => err.span_ref(),
            Self::Separator(err) => err.span_ref(),
            Self::TrailingSeparator { span } => span,
        }
    }
}
impl<E1: SpanningMut<S>, E2: SpanningMut<S>, S: Clone> SpanningMut<S> for Fatal<E1, E2, S> {
    #[inline]
    fn span_mut(&mut self) -> &mut Span<S> {
        match self {
            Self::Comb(err) => err.span_mut(),
            Self::Separator(err) => err.span_mut(),
            Self::TrailingSeparator { span } => span,
        }
    }
}
impl<'s, E1: ParseError<S>, E2: ParseError<S>, S: Clone + Spannable<'s>> ParseError<S> for Fatal<E1, E2, S> {
    #[inline]
    #[track_caller]
    fn more_might_fix(&self) -> bool {
        match self {
            Self::Comb(err) => err.more_might_fix(),
            Self::Separator(err) => err.more_might_fix(),
            Self::TrailingSeparator { span: _ } => false,
        }
    }

    #[inline]
    #[track_caller]
    fn needed_to_fix(&self) -> Option<usize> {
        match self {
            Self::Comb(err) => err.needed_to_fix(),
            Self::Separator(err) => err.needed_to_fix(),
            Self::TrailingSeparator { span: _ } => None,
        }
    }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`SeparatedMany0`] combinator.
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
        write!(f, "multiple repetitions of ")?;
        self.fmt.expects_fmt(f, indent)?;
        write!(f, " interleaved with ")?;
        self.sep.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`separated_many0()`]-combinator.
pub struct SeparatedMany0<C1, C2, S> {
    comb: C1,
    sep:  C2,
    _s:   PhantomData<S>,
}
impl<'c, 's, C1, C2, S> Combinator<'c, 's, S> for SeparatedMany0<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = Vec<C1::Output>;
    type Recoverable = Infallible;
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
            Err(SnackError::Recoverable(_)) => return Ok((input, res)),
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
/// Note that this combinator is OK with matching no input, and can therefore itself not fail.
/// If you want at least one, see [`separated_many1()`](super::separated_many1()) instead.
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`SeparatedMany0`] that applies the given `comb`inator until it fails.
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
/// use ast_toolkit_snack::multi::separated_many0;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("hello,hello,hellogoodbye");
/// let span2 = Span::new("hellogoodbye");
/// let span3 = Span::new("goodbye");
/// let span4 = Span::new(",hello");
/// let span5 = Span::new("hello,helgoodbye");
///
/// let mut comb = separated_many0(tag(b"hello"), tag(b","));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(17..), vec![span1.slice(..5), span1.slice(6..11), span1.slice(12..17)]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), vec![span2.slice(..5)])));
/// assert_eq!(comb.parse(span3), Ok((span3, vec![])));
/// assert_eq!(comb.parse(span4), Ok((span4, vec![])));
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Fatal(separated_many0::Fatal::TrailingSeparator {
///         span: span5.slice(5..6),
///     }))
/// );
/// ```
#[inline]
pub const fn separated_many0<'c, 's, C1, C2, S>(comb: C1, sep: C2) -> SeparatedMany0<C1, C2, S>
where
    C1: Combinator<'c, 's, S>,
    C2: Combinator<'c, 's, S>,
    S: Clone + Spannable<'s>,
{
    SeparatedMany0 { comb, sep, _s: PhantomData }
}
