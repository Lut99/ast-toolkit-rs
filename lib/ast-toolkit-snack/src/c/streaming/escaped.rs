//  ESCAPED.rs
//    by Lut99
//
//  Created:
//    30 Nov 2024, 23:00:24
//  Last edited:
//    13 Mar 2025, 21:33:17
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`escaped()`]-combinator.
//

use std::borrow::Cow;
use std::error::Error;
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, Spannable, Spanning};

pub use super::super::complete::escaped::{EscapedString, ExpectsFormatter, Fatal, Recoverable};
use crate::result::{Result as SResult, SnackError};
use crate::span::{MatchBytes, NextChar, ToStr, WhileUtf8};
use crate::{Combinator, utf8};


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
impl<'t, P, E, F, S> Combinator<'t, F, S> for Escaped<'t, P, F, S>
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
    fn parse(&mut self, input: ast_toolkit_span::Span<F, S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, F, S> {
        // Step 1: Match the opening delimiter
        let (mut rem, open): (Span<F, S>, Span<F, S>) = match utf8::streaming::tag(self.delim).parse(input) {
            Ok(res) => res,
            Err(SnackError::Recoverable(err)) => {
                return Err(SnackError::Recoverable(Recoverable { delim: self.delim, escaper: self.escaper, span: err.into_span() }));
            },
            Err(SnackError::Fatal(_)) => unreachable!(),
            Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
        };

        // Step 2: Match the middle bit as anything _but_ any escaped string _and_ the delimiter
        let mut span: Span<F, S> = Span::empty(open.from_ref().clone(), open.source_ref().clone());
        let mut value: Option<String> = None;
        loop {
            // Parse anything in the middle
            match utf8::while0("", |c: &str| -> bool { c != self.delim && c != self.escaper }).parse(rem) {
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
            match utf8::streaming::tag(self.delim).parse(rem.clone()) {
                Ok((mrem, close)) => {
                    return Ok((mrem, EscapedString { delim: (open, close), span, value }));
                },
                Err(SnackError::Recoverable(_)) => {},
                Err(SnackError::Fatal(_)) => unreachable!(),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
            }

            // If we didn't close, then we expect an escaped string
            match utf8::streaming::tag(self.escaper).parse(rem.clone()) {
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
                        None => return Err(SnackError::NotEnough { needed: Some(1), span: mrem }),
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
                Err(SnackError::Fatal(_)) => unreachable!(),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
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
/// [`uncut()`](crate::error::unct())-combinator to turn them into recoverable errors.
///
/// # Example
/// ```rust
/// use std::error;
/// use std::fmt::{Display, Formatter, Result as FResult};
///
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::c::streaming::escaped;
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
///     Err(SnackError::NotEnough { needed: Some(1), span: span4.slice(21..) })
/// );
/// assert_eq!(
///     comb.parse(span5),
///     Err(SnackError::NotEnough { needed: Some(1), span: span5.slice(28..) })
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
