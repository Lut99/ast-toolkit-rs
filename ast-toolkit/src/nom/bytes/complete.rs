//  COMPLETE.rs
//    by Lut99
// 
//  Created:
//    12 Sep 2023, 15:49:44
//  Last edited:
//    18 Sep 2023, 17:00:55
//  Auto updated?
//    Yes
// 
//  Description:
//!   Groups shadows of combinators that are shadowing combinators in
//!   [`nom`](::nom)'s [`bytes::complete`](::nom::bytes::complete) module.
// 

use std::fmt::Display;

use nom::{Compare, CompareResult, Err, FindToken, InputLength, InputTake, InputTakeAtPosition, IResult};

use crate::nom::{ErrorKind, NomError, NomErrorKind};


/***** LIBRARY *****/
/// Recognizes a pattern.
///
/// The input data will be compared to the tag combinator's argument and will return the part of
/// the input that matches the argument
///
/// It will return `Err(Err::Error((_, ErrorKind::Tag)))` if the input doesn't match the pattern
/// # Example
/// ```rust
/// # use nom::{Err, Needed, IResult};
/// # use ast_toolkit::diagnostic::Diagnosticable as _;
/// # use ast_toolkit::span::Span;
/// use ast_toolkit::nom::{ErrorKind, NomError};
/// use ast_toolkit::nom::bytes::complete::tag;
///
/// fn parser<'f, 's>(s: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<Span<'f, 's>>> {
///   tag("Hello")(s)
/// }
/// 
/// let input1 = Span::new("<example>", "Hello, world!");
/// let input2 = Span::new("<example>", "Something");
/// let input3 = Span::new("<example>", "");
/// 
/// assert_eq!(parser(input1), Ok((input1.range(5..), input1.range(..5))));
/// assert_eq!(parser(input2).unwrap_err().into_diag().message(), "Expected 'Hello'");
/// assert_eq!(parser(input3).unwrap_err().into_diag().message(), "Expected 'Hello'");
/// ```
pub fn tag<T, Input>(tag: T) -> impl Fn(Input) -> IResult<Input, Input, NomError<Input>>
where
    Input: InputTake + Compare<T>,
    T: InputLength + Clone + Display,
{
    move |i: Input| {
        let tag_len = tag.input_len();
        let t = tag.clone();
        let res: IResult<_, _, NomError<Input>> = match i.compare(t) {
            CompareResult::Ok => Ok(i.take_split(tag_len)),
            _ => {
                let e: ErrorKind = ErrorKind::tag(format!("'{tag}'"), true);
                Err(Err::Error(NomError::error_kind(i, e)))
            }
        };
        res
    }
}

/// Recognizes a case insensitive pattern.
///
/// The input data will be compared to the tag combinator's argument and will return the part of
/// the input that matches the argument with no regard to case.
///
/// It will return `Err(Err::Error((_, ErrorKind::Tag)))` if the input doesn't match the pattern.
/// # Example
/// ```rust
/// # use nom::{Err, Needed, IResult};
/// # use ast_toolkit::diagnostic::Diagnosticable as _;
/// # use ast_toolkit::span::Span;
/// use ast_toolkit::nom::{ErrorKind, NomError};
/// use ast_toolkit::nom::bytes::complete::tag_no_case;
///
/// fn parser<'f, 's>(s: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<Span<'f, 's>>> {
///   tag_no_case("hello")(s)
/// }
/// 
/// let input1 = Span::new("<example>", "Hello, world!");
/// let input2 = Span::new("<example>", "hello, world!");
/// let input3 = Span::new("<example>", "HeLlO, world!");
/// let input4 = Span::new("<example>", "Something");
/// let input5 = Span::new("<example>", "");
/// 
/// assert_eq!(parser(input1), Ok((input1.range(5..), input1.range(..5))));
/// assert_eq!(parser(input2), Ok((input2.range(5..), input2.range(..5))));
/// assert_eq!(parser(input3), Ok((input3.range(5..), input3.range(..5))));
/// assert_eq!(parser(input4).unwrap_err().into_diag().message(), "Expected 'hello' (case insensitive)");
/// assert_eq!(parser(input5).unwrap_err().into_diag().message(), "Expected 'hello' (case insensitive)");
/// ```
pub fn tag_no_case<T, Input>(tag: T) -> impl Fn(Input) -> IResult<Input, Input, NomError<Input>>
where
    Input: InputTake + Compare<T>,
    T: InputLength + Clone + Display,
{
    move |i: Input| {
        let tag_len = tag.input_len();
        let t = tag.clone();
        
        let res: IResult<_, _, NomError<Input>> = match (i).compare_no_case(t) {
            CompareResult::Ok => Ok(i.take_split(tag_len)),
            _ => {
                let e: ErrorKind = ErrorKind::tag(format!("'{tag}'"), false);
                Err(Err::Error(NomError::error_kind(i, e)))
            }
        };
        res
    }
}



/// Returns the longest slice of the matches the pattern.
///
/// The parser will return the longest slice consisting of the characters in provided in the
/// combinator's argument.
///
/// It will return a `Err(Err::Error((_, ErrorKind::IsA)))` if the pattern wasn't met.
/// # Example
/// ```rust
/// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
/// use nom::bytes::complete::is_a;
///
/// fn hex(s: &str) -> IResult<&str, &str> {
///   is_a("1234567890ABCDEF")(s)
/// }
///
/// assert_eq!(hex("123 and voila"), Ok((" and voila", "123")));
/// assert_eq!(hex("DEADBEEF and others"), Ok((" and others", "DEADBEEF")));
/// assert_eq!(hex("BADBABEsomething"), Ok(("something", "BADBABE")));
/// assert_eq!(hex("D15EA5E"), Ok(("", "D15EA5E")));
/// assert_eq!(hex(""), Err(Err::Error(Error::new("", ErrorKind::IsA))));
/// ```
pub fn is_a<T, Input>(arr: T) -> impl Fn(Input) -> IResult<Input, Input, NomError<Input>>
where
    Input: InputTakeAtPosition,
    T: Display + FindToken<<Input as InputTakeAtPosition>::Item>,
{
    move |i: Input| {
        i.split_at_position1_complete(|c| !arr.find_token(c), nom::error::ErrorKind::IsA)
            // Inject the context into the error before returning it (if it errorred)
            .map_err(|mut err: nom::Err<NomError<Input>>| {
                // Unpack the error
                match &mut err {
                    Err::Error(err) | Err::Failure(err) => match &mut err.kind {
                        NomErrorKind::ErrorKind(_, e) => match e {
                            // Update the context
                            ErrorKind::IsA(context) => { *context = Some(format!("'{arr}'")); },

                            // Else, nothing to do
                            _ => {},
                        },
                        _ => {},
                    },
                    _ => {},
                };

                // Done, propagate
                err
            })
    }
}
