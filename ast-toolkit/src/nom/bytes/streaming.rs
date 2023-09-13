//  STREAMING.rs
//    by Lut99
// 
//  Created:
//    13 Sep 2023, 17:14:56
//  Last edited:
//    13 Sep 2023, 17:36:50
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
/// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
/// use ast_toolkit::nom::bytes::streaming::tag;
///
/// fn parser(s: &str) -> IResult<&str, &str> {
///   tag("Hello")(s)
/// }
///
/// assert_eq!(parser("Hello, World!"), Ok((", World!", "Hello")));
/// assert_eq!(parser("Something"), Err(Err::Error(Error::new("Something", ErrorKind::Tag))));
/// assert_eq!(parser("S"), Err(Err::Error(Error::new("S", ErrorKind::Tag))));
/// assert_eq!(parser("H"), Err(Err::Incomplete(Needed::new(4))));
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
/// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
/// use ast_toolkit::nom::bytes::streaming::tag_no_case;
///
/// fn parser(s: &str) -> IResult<&str, &str> {
///   tag_no_case("hello")(s)
/// }
///
/// assert_eq!(parser("Hello, World!"), Ok((", World!", "Hello")));
/// assert_eq!(parser("hello, World!"), Ok((", World!", "hello")));
/// assert_eq!(parser("HeLlO, World!"), Ok((", World!", "HeLlO")));
/// assert_eq!(parser("Something"), Err(Err::Error(Error::new("Something", ErrorKind::Tag))));
/// assert_eq!(parser(""), Err(Err::Incomplete(Needed::new(5))));
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
