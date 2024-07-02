//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    28 Jun 2024, 15:22:37
//  Last edited:
//    02 Jul 2024, 13:43:34
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines complete (i.e., non-streaming) versions of C-syntax
//!   combinators.
//

use std::borrow::Cow;
use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::range::SpanRange;
use ast_toolkit_span::{Span, Spannable, Spanning as _};

use crate::error::{Common, Error, Failure};
use crate::span::{MatchBytes, NextChar, ToStr, WhileUtf8};
use crate::utf8::complete::tag;
use crate::utf8::while0;
use crate::{Combinator, Expects, ExpectsFormatter, Result};


/***** AUXILLARY *****/
/// Represents the result of the [`escaped`]-combinator.
#[derive(Clone, Debug)]
pub struct EscapedString<F, S> {
    /// Represents the delimited quotes (opening and closing, respectively).
    pub delim: (Span<F, S>, Span<F, S>),
    /// Represents the span of the literal itself, _excluding_ the quotes.
    pub span:  Span<F, S>,
    /// If the literal includes escapes, then this is the resolved value after processing them.
    pub value: Option<String>,
}





/***** LIBRARY FUNCTIONS *****/
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
/// # Example
/// ```rust
/// use std::error;
/// use std::fmt::{Display, Formatter, Result as FResult};
///
/// use ast_toolkit_snack::c::complete::escaped;
/// use ast_toolkit_snack::error::{Common, Error, Failure};
/// use ast_toolkit_snack::{Combinator as _, Result};
/// use ast_toolkit_span::Span;
///
/// #[derive(Debug)]
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
///     escaped("\"", "\\", |c| if c != "!" { Ok(c.into()) } else { Err(IllegalEscapee) });
/// assert_eq!(
///     comb.parse(span1).unwrap(),
///     (span1.slice(17..), ("Hello, there!n".to_string(), span1.slice(..)))
/// );
/// assert_eq!(
///     comb.parse(span2).unwrap(),
///     (span2.slice(29..), ("My my, don't I love my \"es".to_string(), span2.slice(..)))
/// );
/// assert!(matches!(comb.parse(span3), Result::Fail(Failure::Common(Common::EscapedOpen { .. }))));
/// assert!(matches!(comb.parse(span4), Result::Error(Error::Common(Common::EscapedClose { .. }))));
/// assert!(matches!(
///     comb.parse(span5),
///     Result::Error(Error::Common(Common::EscapedOrphanEscaper { .. }))
/// ));
/// assert!(matches!(
///     comb.parse(span6),
///     Result::Error(Error::Common(Common::EscapedIllegalEscapee { .. }))
/// ));
/// ```
pub const fn escaped<'t, F, S, E, E2>(delim: &'t str, escaper: &'t str, callback: E) -> Escaped<'t, F, S, E>
where
    F: Clone,
    S: Clone + MatchBytes + NextChar + Spannable + ToStr + WhileUtf8,
    E: for<'a> FnMut(&'a str) -> std::result::Result<Cow<'a, str>, E2>,
    E2: 't + std::error::Error,
{
    Escaped { delim, escaper, callback, _f: PhantomData, _s: PhantomData }
}





/***** LIBRARY FORMATTERS *****/
/// ExpectsFormatter for the [`Escaped`] combinator.
#[derive(Debug)]
pub struct EscapedExpects<'t> {
    /// The opening/closing character
    pub delim:   &'t str,
    /// The character that escapes other characters.
    pub escaper: &'t str,
}
impl<'t> Display for EscapedExpects<'t> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<'t> ExpectsFormatter for EscapedExpects<'t> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult {
        write!(f, "a {}-string with {}-escaped characters", self.delim, self.escaper)
    }
}





/***** LIBRARY COMBINATORS *****/
/// The combinator returned by [`escaped()`].
pub struct Escaped<'t, F, S, E> {
    /// Some character acting as the opening/closing character (e.g., '"').
    delim: &'t str,
    /// Some character acting as the escape character.
    escaper: &'t str,
    /// Some closure that determines what to do with escaped characters.
    callback: E,
    /// Store the target `F`rom string type in this struct in order to be much nicer to type deduction.
    _f: PhantomData<F>,
    /// Store the target `S`ource string type in this struct in order to be much nicer to type deduction.
    _s: PhantomData<S>,
}
impl<'t, F, S, E> Expects<'t> for Escaped<'t, F, S, E> {
    type Formatter = EscapedExpects<'t>;

    #[inline]
    fn expects(&self) -> Self::Formatter { EscapedExpects { delim: self.delim, escaper: self.escaper } }
}
impl<'t, F, S, E, E2> Combinator<'t, F, S> for Escaped<'t, F, S, E>
where
    F: Clone,
    S: Clone + MatchBytes + NextChar + Spannable + ToStr + WhileUtf8,
    E: for<'a> FnMut(&'a str) -> std::result::Result<Cow<'a, str>, E2>,
    E2: 't + std::error::Error,
{
    type Output = EscapedString<F, S>;
    type Error = Infallible;

    #[inline]
    fn parse(&mut self, input: ast_toolkit_span::Span<F, S>) -> crate::Result<'t, Self::Output, F, S, Self::Error> {
        // Step 1: Match the opening delimiter
        let (mut rem, open): (Span<F, S>, Span<F, S>) = match tag(self.delim).parse(input) {
            Result::Ok(rem, res) => (rem, res),
            Result::Fail(fail) => {
                return Result::Fail(Failure::Common(Common::EscapedOpen { delim: self.delim, escaper: self.escaper, span: fail.span() }));
            },
            Result::Error(_) => unreachable!(),
        };

        // Step 2: Match the middle bit as anything _but_ any escaped string _and_ the delimiter
        let mut span: Span<F, S> = Span::empty(open.from_ref().clone(), open.source_ref().clone());
        let mut value: Option<String> = None;
        loop {
            // Parse anything in the middle
            match while0(|c: &str| -> bool { c != self.delim && c != self.escaper }).parse(rem) {
                Result::Ok(mrem, mres) => {
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
                Result::Fail(_) => unreachable!(),
                Result::Error(_) => unreachable!(),
            }

            // Next, try to parse the delimiter
            match tag(self.delim).parse(rem.clone()) {
                Result::Ok(mrem, close) => {
                    return Result::Ok(mrem, EscapedString { delim: (open, close), span, value });
                },
                Result::Fail(_) => {},
                Result::Error(_) => unreachable!(),
            }

            // If we didn't close, then we expect an escaped string
            match tag(self.escaper).parse(rem) {
                Result::Ok(mrem, mres) => {
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
                        None => return Result::Error(Error::Common(Common::EscapedOrphanEscaper { escaper: self.escaper, span: mrem })),
                    };

                    // Run the closure to process the escaped character
                    match (self.callback)(c) {
                        Ok(c) => value.push_str(c.as_ref()),
                        Err(err) => return Result::Error(Error::Common(Common::EscapedIllegalEscapee { err: Box::new(err), span: mrem })),
                    }

                    // Extend `middle` to match
                    if !span.join_mut(&mrem.slice(..c.len())) {
                        panic!("Spans are not of the same source - unexpectedly??");
                    }
                    rem = mrem.slice(c.len()..);

                    // OK, continue
                    continue;
                },
                Result::Fail(fail) => {
                    return Result::Error(Error::Common(Common::EscapedClose { delim: self.delim, escaper: self.escaper, span: fail.span() }));
                },
                Result::Error(_) => unreachable!(),
            }

            // We parsed an escaped character. Try again to parse more.
        }
    }
}
