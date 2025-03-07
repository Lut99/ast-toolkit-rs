//  PUNCTUATED MOST 0.rs
//    by Lut99
//
//  Created:
//    07 Mar 2025, 17:15:43
//  Last edited:
//    07 Mar 2025, 17:18:24
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`punctuated_most0()`]-combinator.
//

use std::convert::Infallible;
use std::error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use better_derive::{Debug, Eq, PartialEq};
use snack::combinator::recognize;
use snack::result::{Result as SResult, SnackError};
use snack::{Combinator, ExpectsFormatter as _};
use span::{Span, Spanning};

use crate::Punctuated;


/***** ERRORS *****/
/// Defines the fatal errors thrown by [`PunctuatedMost0`].
#[derive(Debug, Eq, PartialEq)]
pub enum Fatal<E1, E2, F, S> {
    /// The element-combinator failed fatally.
    Comb(E1),
    /// The separator-combinator failed fatally.
    Separator(E2),
    /// Lint that will explain the user they added an incorrect trailing separator.
    TrailingSeparator { span: Span<F, S> },
}
impl<E1: Display, E2: Display, F, S> Display for Fatal<E1, E2, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Comb(err) => err.fmt(f),
            Self::Separator(err) => err.fmt(f),
            Self::TrailingSeparator { span: _ } => write!(f, "Encountered trailing separator"),
        }
    }
}
impl<E1: error::Error, E2: error::Error, F, S> error::Error for Fatal<E1, E2, F, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::Comb(err) => err.source(),
            Self::Separator(err) => err.source(),
            Self::TrailingSeparator { span: _ } => None,
        }
    }
}
impl<E1: Spanning<F, S>, E2: Spanning<F, S>, F: Clone, S: Clone> Spanning<F, S> for Fatal<E1, E2, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Comb(err) => err.span(),
            Self::Separator(err) => err.span(),
            Self::TrailingSeparator { span } => span.clone(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<F, S> {
        match self {
            Self::Comb(err) => err.into_span(),
            Self::Separator(err) => err.into_span(),
            Self::TrailingSeparator { span } => span,
        }
    }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`PunctuatedMost0`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<O1, O2> {
    /// The thing we expect multiple times.
    pub fmt: O1,
    /// The thing interleaving the thing we expected multiple times.
    pub sep: O2,
}
impl<O1: snack::ExpectsFormatter, O2: snack::ExpectsFormatter> Display for ExpectsFormatter<O1, O2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<O1: snack::ExpectsFormatter, O2: snack::ExpectsFormatter> snack::ExpectsFormatter for ExpectsFormatter<O1, O2> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "multiple repetitions of ")?;
        self.fmt.expects_fmt(f, indent)?;
        write!(f, " interleaved with ")?;
        self.sep.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`punctuated_most0()`]-combinator.
pub struct PunctuatedMost0<C1, C2, F, S> {
    comb: C1,
    sep:  C2,
    _f:   PhantomData<F>,
    _s:   PhantomData<S>,
}
impl<'t, C1, C2, F, S> Combinator<'t, F, S> for PunctuatedMost0<C1, C2, F, S>
where
    C1: Combinator<'t, F, S>,
    C2: Combinator<'t, F, S>,
    F: Clone,
    S: Clone,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = Punctuated<C1::Output, C2::Output>;
    type Recoverable = Infallible;
    type Fatal = Fatal<C1::Fatal, C2::Fatal, F, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects(), sep: self.sep.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        // Parse the first element
        let mut res: Punctuated<C1::Output, C2::Output> = Punctuated::new();
        let mut rem: Span<F, S> = match self.comb.parse(input.clone()) {
            Ok((rem, elem)) => {
                if res.len() >= res.capacity() {
                    res.reserve(1 + res.len())
                }
                res.push_first(elem);
                rem
            },
            Err(SnackError::Recoverable(_)) => return Ok((input, res)),
            Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Comb(err))),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Then parse as long as there are commas
        loop {
            // Try the comma first
            let sep: Span<F, S> = match recognize(&mut self.sep).parse(rem.clone()) {
                Ok((rem2, sep)) => {
                    rem = rem2;
                    sep
                },
                Err(SnackError::Recoverable(_)) => return Ok((rem, res)),
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Separator(err))),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
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
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
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
/// # Streaming
/// The punctuated_most0-combinator's streamingness comes from using a streamed version of the
/// nested combinator or not. Being greedy, if no input is left after a successful parse of `comb`,
/// this will _still_ return a [`SnackError::NotEnough`]. If you want the combinator to stop
/// parsing in such a scenario instead, consider using
/// [`separated_few0()`](super::separated_few0()) instead.
///
/// As a rule of thumb, use the `most`-combinators when the user indicates the end of the
/// repetitions by something concrete (e.g., expressions wrapped in parenthesis).
///
/// # Arguments
/// - `comb`: The combinator to repeatedly apply until it fails.
///
/// # Returns
/// A combinator [`PunctuatedMost0`] that applies the given `comb`inator until it fails.
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
/// use ast_toolkit_snack::multi::punctuated_most0;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::complete::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello,hellogoodbye");
/// let span2 = Span::new("<example>", "hellogoodbye");
/// let span3 = Span::new("<example>", "goodbye");
/// let span4 = Span::new("<example>", ",hello");
/// let span5 = Span::new("<example>", "hello,helgoodbye");
///
/// let mut comb = punctuated_most0(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(17..), vec![span1.slice(..5), span1.slice(6..11), span1.slice(12..17)]))
/// );
/// assert_eq!(comb.parse(span2), Ok((span2.slice(5..), vec![span2.slice(..5)])));
/// assert_eq!(comb.parse(span3), Ok((span3, vec![])));
/// assert_eq!(comb.parse(span4), Ok((span4, vec![])));
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Fatal(punctuated_most0::Fatal::TrailingSeparator {
///         span: span5.slice(5..6),
///     }))
/// );
/// ```
///
/// Another example which shows the usage w.r.t. unexpected end-of-files in streaming contexts:
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::multi::punctuated_most0;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::utf8::streaming::tag;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new("<example>", "hello,hello");
/// let span2 = Span::new("<example>", "hello,hel");
/// let span3 = Span::new("<example>", "");
///
/// let mut comb = punctuated_most0(tag("hello"), tag(","));
/// assert_eq!(
///     comb.parse(span1),
///     Err(SnackError::NotEnough { needed: Some(1), span: span1.slice(11..) })
/// );
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::NotEnough { needed: Some(2), span: span2.slice(9..) })
/// );
/// assert_eq!(comb.parse(span3), Err(SnackError::NotEnough { needed: Some(5), span: span3 }));
/// ```
#[inline]
pub const fn punctuated_most0<'t, C1, C2, F, S>(comb: C1, sep: C2) -> PunctuatedMost0<C1, C2, F, S>
where
    C1: Combinator<'t, F, S>,
    C2: Combinator<'t, F, S>,
    F: Clone,
    S: Clone,
{
    PunctuatedMost0 { comb, sep, _f: PhantomData, _s: PhantomData }
}
