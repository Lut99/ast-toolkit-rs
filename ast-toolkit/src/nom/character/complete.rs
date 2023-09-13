//  COMPLETE.rs
//    by Lut99
// 
//  Created:
//    13 Sep 2023, 17:27:53
//  Last edited:
//    13 Sep 2023, 17:35:44
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
/// # use nom::{Err, error::{Error, ErrorKind}, IResult, Needed};
/// # use ast_toolkit::nom::character::complete::not_line_ending;
/// fn parser(input: &str) -> IResult<&str, &str> {
///     not_line_ending(input)
/// }
///
/// assert_eq!(parser("ab\r\nc"), Ok(("\r\nc", "ab")));
/// assert_eq!(parser("ab\nc"), Ok(("\nc", "ab")));
/// assert_eq!(parser("abc"), Ok(("", "abc")));
/// assert_eq!(parser(""), Ok(("", "")));
/// assert_eq!(parser("a\rb\nc"), Err(Err::Error(Error { input: "a\rb\nc", code: ErrorKind::Tag })));
/// assert_eq!(parser("a\rbc"), Err(Err::Error(Error { input: "a\rbc", code: ErrorKind::Tag })));
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
