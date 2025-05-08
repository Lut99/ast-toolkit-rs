//  ESCAPED.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 23:00:24
//  Last edited:
//    08 May 2025, 13:11:54
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

use ast_toolkit_span::{Span, Spannable, SpannableUtf8, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use crate::result::{Result as SResult, SnackError, SpanningError};
use crate::{Combinator, ExpectsFormatter as _, utf8};


/***** ERRORS *****/
/// Defines the recoverable error of the [`Escaped`]-combinator.
///
/// This error means that no opening delimiter was found.
#[derive(Debug, Eq, PartialEq)]
pub struct Recoverable<'c, S> {
    /// Some character acting as the opening/closing character (e.g., '"').
    pub delim:   &'c str,
    /// Some character acting as the escape character.
    pub escaper: &'c str,
    /// The span where the error occurred.
    pub span:    Span<S>,
}
impl<'c, S> Display for Recoverable<'c, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult { write!(f, "{}", ExpectsFormatter { delim: self.delim, escaper: self.escaper }) }
}
impl<'c, 's, S: Spannable<'s>> Error for Recoverable<'c, S> {}
impl<'c, S: Clone> Spanning<S> for Recoverable<'c, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> { Cow::Borrowed(&self.span) }

    #[inline]
    fn into_span(self) -> Span<S> { self.span }
}

/// Defines the fatal errors of the [`Escaped`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub enum Fatal<'c, E, S> {
    /// Failed to find the matching closing delimiter.
    DelimClose { delim: &'c str, escaper: &'c str, span: Span<S> },
    /// An escapee was illegal by the user's closure.
    IllegalEscapee { err: SpanningError<E, S> },
    /// An escape-character (e.g., `\`) was given without an escapee.
    OrphanEscaper { escaper: &'c str, span: Span<S> },
}
impl<'c, E: Display, S> Display for Fatal<'c, E, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::DelimClose { delim, escaper, .. } => write!(f, "Expected either escaper ({escaper:?}) or closing delimiter ({delim:?})"),
            Self::IllegalEscapee { err } => err.fmt(f),
            Self::OrphanEscaper { escaper, .. } => write!(f, "Expected a character to escape after escaper {escaper:?}"),
        }
    }
}
impl<'c, 's, E: Error, S: Spannable<'s>> Error for Fatal<'c, E, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::IllegalEscapee { err } => err.source(),
            Self::DelimClose { .. } | Self::OrphanEscaper { .. } => None,
        }
    }
}
impl<'c, E, S: Clone> Spanning<S> for Fatal<'c, E, S> {
    #[inline]
    fn span(&self) -> Cow<Span<S>> {
        match self {
            Self::DelimClose { span, .. } => Cow::Borrowed(span),
            Self::IllegalEscapee { err } => err.span(),
            Self::OrphanEscaper { span, .. } => Cow::Borrowed(span),
        }
    }

    #[inline]
    fn into_span(self) -> Span<S> {
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
pub struct ExpectsFormatter<'c> {
    /// The opening/closing character
    pub delim:   &'c str,
    /// The character that escapes other characters.
    pub escaper: &'c str,
}
impl<'c> Display for ExpectsFormatter<'c> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'c> crate::ExpectsFormatter for ExpectsFormatter<'c> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "a {}-string with {}-escaped characters", self.delim, self.escaper)
    }
}





/***** HELPERS *****/
/// Lil' formalism for the parsing state machine states.
enum State {
    /// The main string body state
    Body,
    /// We found an escaper.
    Escaped,
}





/***** AUXILLARY *****/
/// Represents the result of the [`escaped`]-combinator.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EscapedString<S> {
    /// The value of the escaped string.
    pub value: String,
    /// Represents the delimited quotes (opening and closing, respectively).
    pub delim: (Span<S>, Span<S>),
    /// Represents the span of the literal itself, _excluding_ the quotes.
    pub span:  Span<S>,
}
impl<'s, S: Clone + Spannable<'s>> Spanning<S> for EscapedString<S> {
    /// This returns the span of the ENTIRE object, not just the string literal value.
    #[inline]
    #[track_caller]
    fn span(&self) -> Cow<Span<S>> {
        Cow::Owned(self.delim.0.join(&self.delim.1).unwrap_or_else(|| {
            panic!(
                "Attempted to join left and right delimiters, but they are from different sources (left: {:?}, right: {:?})",
                self.delim.0.source_id(),
                self.delim.1.source_id()
            )
        }))
    }

    /// This returns the span of the ENTIRE object, not just the string literal value.
    #[inline]
    fn into_span(self) -> Span<S> { self.span().into_owned() }
}





/***** COMBINATORS *****/
/// The combinator returned by [`escaped()`].
pub struct Escaped<'c, P, S> {
    /// Some character acting as the opening/closing character (e.g., '"').
    delim: &'c str,
    /// Some character acting as the escape character.
    escaper: &'c str,
    /// Some closure that determines what to do with escaped characters.
    callback: P,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<'c, 's, 'a, P, E, S> Combinator<'a, 's, S> for Escaped<'c, P, S>
where
    'c: 'a,
    P: for<'b> FnMut(&'b str) -> Result<Cow<'b, str>, E>,
    E: 'c + Error,
    S: Clone + SpannableUtf8<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = EscapedString<S>;
    type Recoverable = Recoverable<'c, S>;
    type Fatal = Fatal<'c, E, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { delim: self.delim, escaper: self.escaper } }

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Step 1: Match the opening delimiter
        let (rem, open): (Span<S>, Span<S>) = match utf8::complete::tag(self.delim).parse(input) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => {
                return Err(SnackError::Recoverable(Recoverable { delim: self.delim, escaper: self.escaper, span: err.into_span() }));
            },
            Err(SnackError::Fatal(_) | SnackError::NotEnough { .. }) => unreachable!(),
        };

        // Step 2: Parse the middle bit using a lil' state maching
        let mut state = State::Body;
        let mut i: usize = 0;
        let mut close: Option<Span<S>> = None;
        let mut value = String::new();
        let mut ret_err: Option<Self::Fatal> = None;
        rem.match_utf8_while(|c| {
            match state {
                State::Body => {
                    // Either we parse a closing delim OR an escapee
                    if c == self.delim {
                        // OK! That's it folks!
                        close = Some(rem.slice(i..i + c.len()));
                        i += c.len();
                        false
                    } else if c == self.escaper {
                        // Switch to escaper mode
                        i += c.len();
                        state = State::Escaped;
                        true
                    } else {
                        // As we are
                        i += c.len();
                        if 1 + value.len() >= value.capacity() {
                            value.reserve(1 + value.len());
                        }
                        value.push_str(c);
                        true
                    }
                },

                State::Escaped => {
                    // Process the character
                    match (self.callback)(c) {
                        Ok(val) => {
                            i += c.len();
                            value.push_str(val.as_ref());
                            state = State::Body;
                            true
                        },
                        Err(err) => {
                            // A bit hacky, but best we can do while working in a closure
                            ret_err = Some(Fatal::IllegalEscapee { err: SpanningError { err, span: rem.slice(i..i + c.len()) } });
                            false
                        },
                    }
                },
            }
        });
        // Quit if there was an error
        if let Some(err) = ret_err {
            return Err(SnackError::Fatal(err));
        }

        // Do some error catching
        if !matches!(state, State::Body) {
            return Err(SnackError::Fatal(Fatal::OrphanEscaper { escaper: self.escaper, span: rem.slice(i - self.escaper.len()..i) }));
        }
        let close: Span<S> = match close {
            Some(close) => close,
            None => return Err(SnackError::Fatal(Fatal::DelimClose { delim: self.delim, escaper: self.escaper, span: rem.slice(i..) })),
        };

        // Step 3: Enjoy
        Ok((rem.slice(i..), EscapedString { value, delim: (open, close), span: rem.slice(..i - self.delim.len()) }))
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
/// [`uncut()`](crate::error::unct())-combinator to turn them into recoverable errors.
///
/// # Example
/// ```rust
/// use std::error;
/// use std::fmt::{Display, Formatter, Result as FResult};
///
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::c::complete::escaped;
/// use ast_toolkit_snack::result::{SnackError, SpanningError};
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
/// let span1 = Span::new(r#""Hello, there!\n""#);
/// let span2 = Span::new(r#""My my, don't I love my \"es""#);
/// let span3 = Span::new(r#"Not a string :("#);
/// let span4 = Span::new(r#""A string with no end"#);
/// let span5 = Span::new(r#""An escaper without escapee\"#);
/// let span6 = Span::new(r#""Illegal escaper\!"#);
///
/// let mut comb =
///     escaped("\"", "\\", |c: &str| if c != "!" { Ok(c.into()) } else { Err(IllegalEscapee) });
/// assert_eq!(
///     comb.parse(span1),
///     Ok((span1.slice(17..), escaped::EscapedString {
///         delim: (span1.slice(..1), span1.slice(16..17)),
///         span:  span1.slice(1..16),
///         value: "Hello, there!n".into(),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span2).unwrap(),
///     (span2.slice(29..), escaped::EscapedString {
///         delim: (span2.slice(..1), span2.slice(28..29)),
///         span:  span2.slice(1..28),
///         value: "My my, don't I love my \"es".into(),
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
///         span:    span5.slice(27..28),
///     }))
/// );
/// assert_eq!(
///     comb.parse(span6),
///     Err(SnackError::Fatal(escaped::Fatal::IllegalEscapee {
///         err: SpanningError { err: IllegalEscapee, span: span6.slice(17..18) },
///     }))
/// );
/// ```
pub const fn escaped<'c, 's, P, E, S>(delim: &'c str, escaper: &'c str, callback: P) -> Escaped<'c, P, S>
where
    P: for<'a> FnMut(&'a str) -> Result<Cow<'a, str>, E>,
    E: 'c + Error,
    S: Clone + SpannableUtf8<'s>,
{
    Escaped { delim, escaper, callback, _s: PhantomData }
}
