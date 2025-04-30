//  BYTE.rs
//    by Lut99
//
//  Created:
//    30 Apr 2025, 08:59:24
//  Last edited:
//    30 Apr 2025, 09:17:28
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements a combinator that will consume exactly one byte if it
//!   matches a predicate.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::Combinator;
use crate::result::{Expected, Result as SResult, SnackError};
use crate::span::BytesParsable;


/***** TYPE ALIASES *****/
/// Defines recoverable errors emitted by the [`Byte`]-combinator.
pub type Recoverable<'c, S> = Expected<ExpectsFormatter<'c>, S>;





/***** EXPECTS ******/
/// Renders the expects-string for the [`Byte`]-combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<'c> {
    /// Something use-provided describing what was being looked for.
    pub what: &'c str,
}
impl<'c> Display for ExpectsFormatter<'c> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
    }
}
impl<'c> crate::ExpectsFormatter for ExpectsFormatter<'c> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> FResult { write!(f, "one {}", self.what) }
}





/***** COMBINATOR *****/
/// Actual implementation of [`byte()`]-.
pub struct Byte<'c, P, S> {
    /// A string describing what was expected.
    what: &'c str,
    /// The predicate for matching bytes.
    pred: P,
    _s:   PhantomData<S>,
}
impl<'s, 'c, 'a, P, S> Combinator<'a, 's, S> for Byte<'c, P, S>
where
    'c: 'a,
    P: FnMut(u8) -> bool,
    S: Clone + Spannable<'s>,
    S::Slice: BytesParsable<'s>,
{
    type ExpectsFormatter = ExpectsFormatter<'c>;
    type Output = Span<S>;
    type Recoverable = Recoverable<'c, S>;
    type Fatal = Infallible;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { what: self.what } }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        // Affirm there is a byte
        let b: u8 = match input.bytes().next() {
            Some(b) => b,
            None => return Err(SnackError::Recoverable(Expected { fmt: self.expects(), span: input })),
        };

        // Check it matches
        if (self.pred)(b) {
            Ok((input.slice(1..), input.slice(..1)))
        } else {
            Err(SnackError::Recoverable(Expected { fmt: self.expects(), span: input }))
        }
    }
}





/***** LIBRARY *****/
/// A combinator that will parse a single byte matching a predicate.
///
/// This is very much like [`while1()`](super::while1()), except that it will not greedily match
/// more bytes beyond the first.
///
/// Note that this is the complete version of the parser. If you're intending to stream input
/// instead, see [`byte()`](super::super::streaming::byte()).
///
/// # Arguments
/// - `what`: Some user-friendly description of what this combinator expects. Should complete:
///   "Expected one ...".
/// - `pred`: Some [predicate](FnMut) that will be used to check if the parsed byte is what should
///   be matched.
///
/// # Returns
/// A combinator that will parse a byte matching the given `pred`icate.
///
/// # Fails
/// The returned combinator fails if the input is empty or the first byte does not match the
/// `pred`icate.
///
/// The returned combinator never fails fatally.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::Combinator as _;
/// use ast_toolkit_snack::bytes::complete::byte;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_span::Span;
///
/// let span1 = Span::new(b"A".as_slice());
/// let span2 = Span::new(b"a".as_slice());
/// let span3 = Span::new(b"".as_slice());
///
/// let mut comb = byte("uppercase letter", |b| b >= b'A' && b <= b'Z');
/// assert_eq!(comb.parse(span1), Ok((span1.slice(1..), span1.slice(..1))));
/// assert_eq!(
///     comb.parse(span2),
///     Err(SnackError::Recoverable(byte::Recoverable {
///         fmt:  byte::ExpectsFormatter { what: "uppercase letter" },
///         span: span2,
///     }))
/// );
/// assert_eq!(
///     comb.parse(span3),
///     Err(SnackError::Recoverable(byte::Recoverable {
///         fmt:  byte::ExpectsFormatter { what: "uppercase letter" },
///         span: span3,
///     }))
/// );
/// ```
#[inline]
pub const fn byte<'s, 'c, P, S>(what: &'c str, pred: P) -> Byte<'c, P, S>
where
    P: FnMut(u8) -> bool,
    S: Clone + Spannable<'s>,
    S::Slice: BytesParsable<'s>,
{
    Byte { what, pred, _s: PhantomData }
}
