//  COMPLETE.rs
//    by Lut99
// 
//  Created:
//    13 Sep 2023, 17:27:53
//  Last edited:
//    18 Sep 2023, 16:37:02
//  Auto updated?
//    Yes
// 
//  Description:
//!   Groups shadows of combinators that are shadowing combinators in
//!   [`nom`](::nom)'s [`character::complete`](::nom::character::complete) module.
// 

use std::ops::{Range, RangeFrom, RangeTo};

use nom::{AsChar, Compare, CompareResult, Err, InputIter, InputLength, IResult, Slice};

use crate::nom::{ErrorKind, NomError};


/***** LIRBARY *****/
/// Recognizes a string of any char except '\r\n' or '\n'.
///
/// *Complete version*: Will return an error if there's not enough input data.
/// # Example
///
/// ```
/// # use nom::{Err, IResult, Needed};
/// # use ast_toolkit::diagnostic::Diagnosticable as _;
/// # use ast_toolkit::nom::{ErrorKind, NomError};
/// # use ast_toolkit::nom::character::complete::not_line_ending;
/// # use ast_toolkit::span::Span;
/// fn parser<'f, 's>(input: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<Span<'f, 's>>> {
///     not_line_ending(input)
/// }
///
/// let input1 = Span::new("<example>", "ab\r\nc");
/// let input2 = Span::new("<example>", "ab\nc");
/// let input3 = Span::new("<example>", "abc");
/// let input4 = Span::new("<example>", "");
/// let input5 = Span::new("<example>", "a\rb\nc");
/// let input6 = Span::new("<example>", "a\rbc");
/// 
/// assert_eq!(parser(input1), Ok((input1.range(2..), input1.range(..2))));
/// assert_eq!(parser(input2), Ok((input2.range(2..), input1.range(..2))));
/// assert_eq!(parser(input3), Ok((input3.emptied(), input3)));
/// assert_eq!(parser(input4), Ok((input4.emptied(), input4.emptied())));
/// assert_eq!(parser(input5).unwrap_err().into_diag().message(), "Expected '\\n' in CRLF-style line ending");
/// assert_eq!(parser(input6).unwrap_err().into_diag().message(), "Expected '\\n' in CRLF-style line ending");
/// ```
pub fn not_line_ending<T>(input: T) -> IResult<T, T, NomError<T>>
where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength,
    T: Compare<&'static str>,
    <T as InputIter>::Item: AsChar,
    <T as InputIter>::Item: AsChar,
{
    match input.position(|item| {
        let c = item.as_char();
        c == '\r' || c == '\n'
    }) {
        None => Ok((input.slice(input.input_len()..), input)),
        Some(index) => {
            let mut it = input.slice(index..).iter_elements();
            let nth = it.next().unwrap().as_char();
            if nth == '\r' {
                let sliced = input.slice(index..);
                let comp = sliced.compare("\r\n");
                match comp {
                    //FIXME: calculate the right index
                    CompareResult::Ok => Ok((input.slice(index..), input.slice(..index))),
                    _ => {
                        let e: ErrorKind = ErrorKind::tag("'\\n' in CRLF-style line ending", true);
                        Err(Err::Error(NomError::error_kind(input, e)))
                    }
                }
            } else {
                Ok((input.slice(index..), input.slice(..index)))
            }
        }
    }
}
