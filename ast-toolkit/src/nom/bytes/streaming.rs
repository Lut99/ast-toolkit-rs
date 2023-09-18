//  STREAMING.rs
//    by Lut99
// 
//  Created:
//    13 Sep 2023, 17:14:56
//  Last edited:
//    18 Sep 2023, 16:43:30
//  Auto updated?
//    Yes
// 
//  Description:
//!   Groups shadows of combinators that are shadowing combinators in
//!   [`nom`](::nom)'s [`bytes::streaming`](::nom::bytes::streaming) module.
// 

use std::fmt::Display;

use nom::{Compare, CompareResult, Err, InputLength, InputTake, IResult, Needed};

use crate::nom::{ErrorKind, NomError};


/***** LIBRARY *****/
/// Recognizes a pattern.
///
/// The input data will be compared to the tag combinator's argument and will return the part of
/// the input that matches the argument.
/// # Example
/// ```rust
/// # use nom::{Err, Needed, IResult};
/// # use ast_toolkit::diagnostic::Diagnosticable as _;
/// # use ast_toolkit::span::Span;
/// use ast_toolkit::nom::{ErrorKind, NomError};
/// use ast_toolkit::nom::bytes::streaming::tag;
///
/// fn parser<'f, 's>(s: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<Span<'f, 's>>> {
///   tag("Hello")(s)
/// }
/// 
/// let input1 = Span::new("<example>", "Hello, world!");
/// let input2 = Span::new("<example>", "Something");
/// let input3 = Span::new("<example>", "S");
/// let input4 = Span::new("<example>", "H");
/// 
/// assert_eq!(parser(input1), Ok((input1.range(5..), input1.range(..5))));
/// assert_eq!(parser(input2).unwrap_err().into_diag().message(), "Expected 'Hello'");
/// assert_eq!(parser(input3).unwrap_err().into_diag().message(), "Expected 'Hello'");
/// assert_eq!(parser(input4).unwrap_err().into_diag().message(), "Given text parses OK but is incomplete");
/// ```
pub fn tag<T, Input>(tag: T) -> impl Fn(Input) -> IResult<Input, Input, NomError<Input>>
where
    Input: InputTake + InputLength + Compare<T>,
    T: InputLength + Clone + Display,
{
    move |i: Input| {
        let tag_len = tag.input_len();
        let t = tag.clone();
        
        let res: IResult<_, _, NomError<Input>> = match i.compare(t) {
            CompareResult::Ok => Ok(i.take_split(tag_len)),
            CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(tag_len - i.input_len()))),
            CompareResult::Error => {
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
/// # Example
/// ```rust
/// # use nom::{Err, Needed, IResult};
/// # use ast_toolkit::diagnostic::Diagnosticable as _;
/// # use ast_toolkit::span::Span;
/// use ast_toolkit::nom::{ErrorKind, NomError};
/// use ast_toolkit::nom::bytes::streaming::tag_no_case;
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
/// assert_eq!(parser(input5).unwrap_err().into_diag().message(), "Given text parses OK but is incomplete");
/// ```
pub fn tag_no_case<T, Input>(tag: T) -> impl Fn(Input) -> IResult<Input, Input, NomError<Input>>
where
    Input: InputTake + InputLength + Compare<T>,
    T: InputLength + Clone + Display,
{
    move |i: Input| {
        let tag_len = tag.input_len();
        let t = tag.clone();
        
        let res: IResult<_, _, NomError<Input>> = match (i).compare_no_case(t) {
            CompareResult::Ok => Ok(i.take_split(tag_len)),
            CompareResult::Incomplete => Err(Err::Incomplete(Needed::new(tag_len - i.input_len()))),
            CompareResult::Error => {
                let e: ErrorKind = ErrorKind::tag(format!("'{tag}'"), false);
                Err(Err::Error(NomError::error_kind(i, e)))
            }
        };
        res
    }
}
