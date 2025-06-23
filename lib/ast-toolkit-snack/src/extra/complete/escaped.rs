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

use crate::result::{Result as SResult, SnackError, SpanningError};
use crate::{Combinator, ExpectsFormatter as _, utf8};

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
                return Err(SnackError::Recoverable(Recoverable { delim: self.delim, escaper: self.escaper, span: err.take_span() }));
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
