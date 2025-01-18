//  ESCAPED.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 23:00:24
//  Last edited:
//    18 Jan 2025, 17:46:01
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`escaped()`]-combinator.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, Spannable, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::result::{Result as SResult, SnackError};
use crate::span::{MatchBytes, NextChar, ToStr, WhileUtf8};
use crate::{Combinator2, ExpectsFormatter as _, utf82};


/***** ERRORS *****/
/// Defines the recoverable error of the [`Escaped`]-combinator.
///
/// This error means that no opening delimiter was found.
#[derive(Debug, Eq, PartialEq)]
pub struct Recoverable<'t, F, S> {
    /// Some character acting as the opening/closing character (e.g., '"').
    pub delim:   &'t str,
    /// Some character acting as the escape character.
    pub escaper: &'t str,
    /// The span where the error occurred.
    pub span:    Span<F, S>,
}
impl<'t, F, S> Display for Recoverable<'t, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter { delim: self.delim, escaper: self.escaper }) }
}
impl<'t, F, S> Error for Recoverable<'t, F, S> {}
impl<'t, F: Clone, S: Clone> Spanning<F, S> for Recoverable<'t, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> { self.span.clone() }

    #[inline]
    fn into_span(self) -> Span<F, S> { self.span }
}

/// Defines the fatal errors of the [`Escaped`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub enum Fatal<'t, E, F, S> {
    /// Failed to find the matching closing delimiter.
    DelimClose { delim: &'t str, escaper: &'t str, span: Span<F, S> },
    /// An escapee was illegal by the user's closure.
    IllegalEscapee { err: E },
    /// An escape-character (e.g., `\`) was given without an escapee.
    OrphanEscaper { escaper: &'t str, span: Span<F, S> },
}
impl<'t, E: Display, F, S> Display for Fatal<'t, E, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::DelimClose { delim, escaper, .. } => write!(f, "Expected either escaper ({escaper:?}) or closing delimiter ({delim:?})"),
            Self::IllegalEscapee { err } => err.fmt(f),
            Self::OrphanEscaper { escaper, .. } => write!(f, "Expected a character to escape after escaper {escaper:?}"),
        }
    }
}
impl<'t, E: Error, F, S> Error for Fatal<'t, E, F, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::IllegalEscapee { err } => err.source(),
            Self::DelimClose { .. } | Self::OrphanEscaper { .. } => None,
        }
    }
}
impl<'t, E: Spanning<F, S>, F: Clone, S: Clone> Spanning<F, S> for Fatal<'t, E, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::DelimClose { span, .. } => span.clone(),
            Self::IllegalEscapee { err } => err.span(),
            Self::OrphanEscaper { span, .. } => span.clone(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<F, S> {
        match self {
            Self::DelimClose { span, .. } => span,
            Self::IllegalEscapee { err } => err.into_span(),
            Self::OrphanEscaper { span, .. } => span,
        }
    }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Escaped`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'t> {
    /// The opening/closing character
    pub delim:   &'t str,
    /// The character that escapes other characters.
    pub escaper: &'t str,
}
impl<'t> Display for ExpectsFormatter<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> crate::ExpectsFormatter for ExpectsFormatter<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "a {}-string with {}-escaped characters", self.delim, self.escaper)
    }
}





/***** AUXILLARY *****/
/// Represents the result of the [`escaped`]-combinator.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EscapedString<F, S> {
    /// Represents the delimited quotes (opening and closing, respectively).
    pub delim: (Span<F, S>, Span<F, S>),
    /// Represents the span of the literal itself, _excluding_ the quotes.
    pub span:  Span<F, S>,
    /// If the literal includes escapes, then this is the resolved value after processing them.
    pub value: Option<String>,
}





/***** COMBINATORS *****/
/// The combinator returned by [`escaped()`].
pub struct Escaped<'t, P, F, S> {
    /// Some character acting as the opening/closing character (e.g., '"').
    delim: &'t str,
    /// Some character acting as the escape character.
    escaper: &'t str,
    /// Some closure that determines what to do with escaped characters.
    callback: P,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<'t, P, E, F, S> Combinator2<'t, F, S> for Escaped<'t, P, F, S>
where
    P: for<'a> FnMut(&'a str) -> Result<Cow<'a, str>, E>,
    E: 't + Error,
    F: Clone,
    S: Clone + MatchBytes + NextChar + Spannable + ToStr + WhileUtf8,
{
    type ExpectsFormatter = ExpectsFormatter<'t>;
    type Output = EscapedString<F, S>;
    type Recoverable = Recoverable<'t, F, S>;
    type Fatal = Fatal<'t, E, F, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { delim: self.delim, escaper: self.escaper } }

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // Step 1: Match the opening delimiter
        let (mut rem, open): (Span<F, S>, Span<F, S>) = match utf82::complete::tag(self.delim).parse(input) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => {
                return Err(SnackError::Recoverable(Recoverable { delim: self.delim, escaper: self.escaper, span: err.into_span() }));
            },
            Err(SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
        };

        // Step 2: Match the middle bit as anything _but_ any escaped string _and_ the delimiter
        let mut span: Span<F, S> = Span::empty(open.from_ref().clone(), open.source_ref().clone());
        let mut value: Option<String> = None;
        loop {
            // Parse anything in the middle
            match utf82::while0("", |c: &str| -> bool { c != self.delim && c != self.escaper }).parse(rem) {
                Ok((mrem, mres)) => {
                    // Add whatever we've parsed to the middle bit
                    if !span.join_mut(&mres) {
                        panic!("Spans are not of the same source - unexpectedly??");
                    }
                    rem = mrem;

                    // Also add it to the value string if we have one
                    if let Some(value) = &mut value {
                        value.push_str(mres.to_str(SpanRange::Open).as_ref());
                    }
                },
                Err(SnackError::Recoverable(_) | SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
            }

            // Next, try to parse the delimiter
            match utf82::complete::tag(self.delim).parse(rem.clone()) {
                Ok((mrem, close)) => {
                    return Ok((mrem, EscapedString { delim: (open, close), span, value }));
                },
                Err(SnackError::Recoverable(_)) => {},
                Err(SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
            }

            // If we didn't close, then we expect an escaped string
            match utf82::complete::tag(self.escaper).parse(rem) {
                Ok((mrem, mres)) => {
                    // First, bring the value-string up-to-speed to what we parsed so far
                    let value: &mut String = match &mut value {
                        Some(value) => value,
                        None => {
                            value = Some(span.to_str(SpanRange::Open).into_owned());
                            value.as_mut().unwrap()
                        },
                    };

                    // Add it to the parsed span - but not the value
                    if !span.join_mut(&mres) {
                        panic!("Spans are not of the same source - unexpectedly??");
                    }

                    // Get the parsed character
                    let c: &str = match mrem.next_char(SpanRange::Open) {
                        Some(c) => c,
                        None => return Err(SnackError::Fatal(Fatal::OrphanEscaper { escaper: self.escaper, span: mrem })),
                    };

                    // Run the closure to process the escaped character
                    match (self.callback)(c) {
                        Ok(c) => value.push_str(c.as_ref()),
                        Err(err) => return Err(SnackError::Fatal(Fatal::IllegalEscapee { err })),
                    }

                    // Extend `middle` to match
                    if !span.join_mut(&mrem.slice(..c.len())) {
                        panic!("Spans are not of the same source - unexpectedly??");
                    }
                    rem = mrem.slice(c.len()..);

                    // OK, continue
                    continue;
                },
                Err(SnackError::Recoverable(err)) => {
                    return Err(SnackError::Fatal(Fatal::DelimClose { delim: self.delim, escaper: self.escaper, span: err.into_span() }));
                },
                Err(SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
            }

            // We parsed an escaped character. Try again to parse more.
        }
    }
}





/***** LIBRARY *****/
/// Parses an escaped string, a lá C-style languages.
///
/// In particular, parses a string that:
/// - starts and ends with some delimiter;
/// - may feature some special character (escaper); such that
/// - any character following the escaper has special treatment.
///
/// The main use-case for escaping would be to encode the delimiting character: for example, to
/// encode a C-string consisting of a quote, we can write:
/// ```c
/// "\""
/// ```
/// Here, `"` is the delimiter and `\` the escaper. The second quote is escaped by the escaper, and
/// given special meaning; it's this case, it's that the quotes do not delimit.
///
/// # Arguments
/// - `delim`: The delimiting character to use (e.g., `"` for C).
/// - `escaper`: The escape character to use (e.g., `\` for C).
/// - `callback`: Some callback that processes escaped characters. In particular, it will receive
///   the character following the `escaper` and can then return a replacement value (for the escape
///   as a whole; so e.g. `\\` could return `\`) or `Err` to indicate that it's an illegal escape.
///   Note that the function is an [`FnMut`], so side-effects are possible.
///
/// # Returns
/// A [`Escaped`]-combinator that will parse escaped strings.
///
/// # Fails
/// The returned combinator fails if the input string does not start with a delimiter. If it
/// _does_, then it may error if there's no closing delimiter or if there is no character following
/// the escaper. Similarly, `callback`-errors cause errors for the combinator too (i.e., non-
/// recoverable).
///
/// Note that some errors thrown by this combinator are fatal. In particular, any error that
/// indicates an ill-formed string (no closing delimiter, illegal escapee, ...) is fatal. If there
/// is grammar in your language that needs to disambiguated beyond the initial quote, use the
/// [`uncut()`](crate::error2::unct())-combinator to turn them into recoverable errors.
///
/// # Example
/// ```rust
/// use std::error;
/// use std::fmt::{Display, Formatter, Result as FResult};
///
/// use ast_toolkit_snack::Combinator2 as _;
/// use ast_toolkit_snack::c2::complete::escaped;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// #[derive(Debug, Eq, PartialEq)]
/// struct IllegalEscapee;
/// impl Display for IllegalEscapee {
///     #[inline]
///     fn fmt(&self, f: &mut Formatter) -> FResult { write!(f, "Cannot escape '!'") }
/// }
/// impl error::Error for IllegalEscapee {}
///
/// let span1 = Span::new("<example>", r#""Hello, there!\n""#);
/// let span2 = Span::new("<example>", r#""My my, don't I love my \"es""#);
/// let span3 = Span::new("<example>", r#"Not a string :("#);
/// let span4 = Span::new("<example>", r#""A string with no end"#);
/// let span5 = Span::new("<example>", r#""An escaper without escapee\"#);
/// let span6 = Span::new("<example>", r#""Illegal escaper\!"#);
///
/// let mut comb =
///     escaped("\"", "\\", |c: &str| if c != "!" { Ok(c.into()) } else { Err(IllegalEscapee) });
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(17..), escaped::EscapedString {
///         delim: (span1.slice(0..1), span1.slice(16..17)),
///         span:  span1.slice(1..16),
///         value: Some("Hello, there!n".into()),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span2).unwrap(),
///     (span2.slice(29..), escaped::EscapedString {
///         delim: (span2.slice(0..1), span2.slice(28..29)),
///         span:  span2.slice(1..28),
///         value: Some("My my, don't I love my \"es".into()),
///     })
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(escaped::Recoverable {
///         delim:   "\"",
///         escaper: "\\",
///         span:    span3,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span4),
///     Err(SnackError::Fatal(escaped::Fatal::DelimClose {
///         delim:   "\"",
///         escaper: "\\",
///         span:    span4.slice(21..),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::Fatal(escaped::Fatal::OrphanEscaper {
///         escaper: "\\",
///         span:    span5.slice(28..),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span6),
///     Err(SnackError::Fatal(escaped::Fatal::IllegalEscapee { err: IllegalEscapee }))
/// );
/// ```
pub const fn escaped<'t, P, E, F, S>(delim: &'t str, escaper: &'t str, callback: P) -> Escaped<'t, P, F, S>
where
    P: for<'a> FnMut(&'a str) -> Result<Cow<'a, str>, E>,
    E: 't + Error,
    F: Clone,
    S: Clone + MatchBytes + NextChar + Spannable + ToStr + WhileUtf8,
{
    Escaped { delim, escaper, callback, _f: PhantomData, _s: PhantomData }
}
