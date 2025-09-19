//  ESCAPED BYTES.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 23:00:24
//  Last edited:
//    08 May 2025, 13:12:13
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`escaped()`]-combinator.
//

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable, SpannableBytes, SpannableUtf8, Spanning};

use crate::fmt::{ElemDisplay, ElemDisplayIterFormatter};
use crate::result::{Expected, Result as SResult, SnackError, SpanningError};
use crate::{Combinator, ParseError, scan};


/***** ERRORS *****/
/// Defines the recoverable error of the [`Escaped`]-combinator.
///
/// This error means that no opening delimiter was found.
type Recoverable<'c, T, S> = Expected<ExpectsFormatter<'c, T>, S>;

/// Defines the fatal errors of the [`Escaped`]-combinator.
#[derive(better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'c, 's, T, E, S>, bound = (T: r#trait, E: r#trait, S: Spannable<'s>))]
pub enum Fatal<'c, T, E, S> {
    /// Failed to find the matching closing delimiter.
    DelimClose { close: &'c [T], escaper: &'c [T], span: Span<S> },
    /// An escapee was illegal by the user's closure.
    IllegalEscapee { err: SpanningError<E, S> },
    /// An escape-character (e.g., `\`) was given without an escapee.
    OrphanEscaper { escaper: &'c [T], span: Span<S> },
}
impl<'c, T: Debug + ElemDisplay, E: Display, S> Display for Fatal<'c, T, E, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::DelimClose { close, escaper, .. } => write!(
                f,
                "Expected either escaper ({}) or closing delimiter ({})",
                ElemDisplayIterFormatter(*close),
                ElemDisplayIterFormatter(*escaper)
            ),
            Self::IllegalEscapee { err } => err.fmt(f),
            Self::OrphanEscaper { escaper, .. } => write!(f, "Expected a character to escape after escaper {}", ElemDisplayIterFormatter(*escaper)),
        }
    }
}
impl<'c, 's, T: Debug + ElemDisplay, E: Error, S: Spannable<'s>> Error for Fatal<'c, T, E, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::IllegalEscapee { err } => err.source(),
            Self::DelimClose { .. } | Self::OrphanEscaper { .. } => None,
        }
    }
}
impl<'c, T, E, S: Clone> Spanning<S> for Fatal<'c, T, E, S> {
    #[inline]
    fn get_span(&self) -> Cow<'_, Span<S>> {
        match self {
            Self::DelimClose { span, .. } => Cow::Borrowed(span),
            Self::IllegalEscapee { err } => err.get_span(),
            Self::OrphanEscaper { span, .. } => Cow::Borrowed(span),
        }
    }

    #[inline]
    fn take_span(self) -> Span<S> {
        match self {
            Self::DelimClose { span, .. } => span,
            Self::IllegalEscapee { err } => err.take_span(),
            Self::OrphanEscaper { span, .. } => span,
        }
    }
}
impl<'s, 'c, T: Debug + ElemDisplay, E: Error, S: Clone + Spannable<'s>> ParseError<S> for Fatal<'c, T, E, S> {
    #[inline]
    fn more_might_fix(&self) -> bool { todo!() }

    #[inline]
    fn needed_to_fix(&self) -> Option<usize> { todo!() }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`Escaped`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'c, T> {
    /// The opening character
    pub open:    &'c [T],
    /// The closing character
    pub close:   &'c [T],
    /// The character that escapes other characters.
    pub escaper: &'c [T],
}
impl<'c, T: Debug + ElemDisplay> Display for ExpectsFormatter<'c, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<'c, T: Debug + ElemDisplay> crate::ExpectsFormatter for ExpectsFormatter<'c, T> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(
            f,
            "a {}{}-string with {}-escaped characters",
            ElemDisplayIterFormatter(self.open),
            ElemDisplayIterFormatter(self.close),
            ElemDisplayIterFormatter(self.escaper)
        )
    }
}





/***** AUXILLARY *****/
/// Represents the result of the [`escaped`]-combinator.
#[derive(Clone, better_derive::Debug, better_derive::Eq, better_derive::PartialEq)]
#[better_derive(impl_gen = <'c, 's, S>, bound = (S: Spannable<'s>))]
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
    fn get_span(&self) -> Cow<'_, Span<S>> {
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
    fn take_span(self) -> Span<S> { self.get_span().into_owned() }
}





/***** COMBINATORS *****/
/// The combinator returned by [`escaped()`].
pub struct Escaped<'c, T, P, S> {
    /// Some character sequence acting as the opening character (e.g., '"').
    open: &'c [T],
    /// Some character sequence acting as the closing character (e.g., '"').
    close: &'c [T],
    /// Some character sequence acting as the escape character.
    escaper: &'c [T],
    /// Some closure that determines what to do with escaped characters.
    callback: P,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<'c, 's, 'a, P, E, S> Combinator<'a, 's, S> for Escaped<'c, S::Elem, P, S>
where
    'c: 'a,
    P: FnMut(&'s S::Elem) -> Result<Option<S::Elem>, E>,
    E: 'c + Error,
    S: Clone + Spannable<'s>,
    S::Elem: Debug + ElemDisplay + PartialEq,
{
    type ExpectsFormatter = ExpectsFormatter<'c, S::Elem>;
    type Output = EscapedString<S>;
    type Recoverable = Recoverable<'c, S::Elem, S>;
    type Fatal = Fatal<'c, S::Elem, E, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { open: self.open, close: self.close, escaper: self.escaper } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        /// Lil' formalism for the parsing state machine states.
        enum State {
            /// The main string body state
            Body,
            /// We found an escaper.
            Escaped,
        }


        // Step 1: Match the opening delimiter
        let (rem, open): (Span<S>, Span<S>) = match scan::tag(self.open).parse(input) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => {
                return Err(SnackError::Recoverable(Recoverable {
                    fmt:     self.expects(),
                    fixable: if err.more_might_fix() { Some(err.needed_to_fix()) } else { None },
                    span:    err.take_span(),
                }));
            },
            Err(SnackError::Fatal(_)) => unreachable!(),
        };

        // Step 2: Parse the middle bit using a lil' state maching
        let mut state = State::Body;
        let mut i: usize = 0;
        let mut close: Option<Span<S>> = None;
        let mut value = Vec::new();
        let mut ret_err: Option<Self::Fatal> = None;
        rem.match_while(|elem| {
            match state {
                State::Body => {
                    // Either we parse a closing delim OR an escapee
                    if elem == self.close {
                        // OK! That's it folks!
                        close = Some(rem.slice(i..i + 1));
                        i += 1;
                        false
                    } else if c == self.escaper {
                        // Switch to escaper mode
                        i += 1;
                        state = State::Escaped;
                        true
                    } else {
                        // As we are
                        i += 1;
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
            if i < rem.len() {
                return Err(SnackError::Fatal(Fatal::OrphanEscaper { escaper: self.escaper, span: rem.slice(i - self.escaper.len()..i) }));
            } else {
                return Err(SnackError::NotEnough { needed: Some(self.delim.len()), span: rem.slice(i..) });
            }
        }
        let close: Span<S> = match close {
            Some(close) => close,
            None => {
                if i < rem.len() {
                    return Err(SnackError::Fatal(Fatal::DelimClose { delim: self.delim, escaper: self.escaper, span: rem.slice(i..) }));
                } else {
                    return Err(SnackError::NotEnough { needed: Some(1 + self.delim.len()), span: rem.slice(i..) });
                }
            },
        };

        // Step 3: Enjoy
        Ok((rem.slice(i..), EscapedString { value, delim: (open, close), span: rem.slice(..i - self.delim.len()) }))
    }
}





/***** LIBRARY *****/
/// Parses an escaped string, a lá C-style languages.
///
/// In particular, parses a sequence of bytes that:
/// - starts and closes with some delimiter;
/// - may feature some special character (escaper); such that
/// - any character following the escaper has special treatment.
///
/// The main use-case for escaping would be to encode the delimiting character: for example, to
/// encode a C-string consisting of a quote, we can write:
/// ```c
/// "\""
/// ```
/// Here, `"` is the opening- and closing delimiter, and `\` the escaper. The second quote is
/// escaped by the escaper, and given special meaning; it's this case, it's that the quote does
/// not delimit.
///
/// This function will assume that every byte is a standalone character. If you're working with
/// full UTF-8 support, see [`escaped_utf8()`](super::escaped_utf8()) instead to assume that every
/// grapheme is a standalone character.
///
/// # Arguments
/// - `open`: The opening delimiting byte sequence to use (e.g., `"` for C).
/// - `close`: The closing delimiting byte sequence to use (e.g., `"` for C).
/// - `escaper`: The escape byte sequence to use (e.g., `\` for C).
/// - `callback`: Some callback that processes escaped characters. In particular, it will receive
///   the byte following the `escaper` and can then return a replacement value (for the escape
///   as a whole; so e.g. `\\` could return `\`), `None` to indicate it won't be replaced or
///   [`Err`] to indicate that it's an illegal escape. Note that the function is an [`FnMut`], so
///   side-effects are possible.
///
/// # Returns
/// A [`Escaped`]-combinator that will parse escaped string in byte sequences.
///
/// # Fails
/// The returned combinator fails recoverably if the input string does not start with a delimiter.
/// If it _does_, then it may fail fatally if there's no closing delimiter or if there is no
/// byte following the escaper. Similarly, `callback`-errors cause fatal errors for the combinator
/// too (i.e., non-recoverable).
///
/// If there is grammar in your language that needs to disambiguated beyond the initial quote, use
/// the [`uncut()`](crate::error::uncut())-combinator to turn fatal errors thrown (the input was
/// malformed) into recoverable errors (the input was not recognized as an escaped string).
///
/// # Example
/// ```rust
/// use std::error;
/// use std::fmt::{Display, Formatter, Result as FResult};
///
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::extra::escaped;
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
///     escaped(b"\"", b"\\", |b: u8| if b != b'!' { Ok(Some(b)) } else { Err(IllegalEscapee) });
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
///     Err(SnackError::NotEnough { needed: Some(2), span: span4.slice(21..) })
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::NotEnough { needed: Some(1), span: span5.slice(28..) })
/// );
/// assert_eq!(
///     comb.parse(span6),
///     Err(SnackError::Fatal(escaped::Fatal::IllegalEscapee {
///         err: SpanningError { err: IllegalEscapee, span: span6.slice(17..18) },
///     }))
/// );
/// ```
pub const fn escaped<'c, 's, P, E, S>(open: &'c [S::Elem], close: &'c [S::Elem], escaper: &'c [S::Elem], callback: P) -> Escaped<'c, S::Elem, P, S>
where
    P: FnMut(&'s S::Elem) -> Result<Option<S::Elem>, E>,
    E: 'c + Error,
    S: Clone + SpannableBytes<'s>,
{
    Escaped { open, close, escaper, callback, _s: PhantomData }
}
