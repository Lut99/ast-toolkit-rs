//  COMPLETE.rs
//    by Lut99
// 
//  Created:
//    12 Sep 2023, 15:49:44
//  Last edited:
//    13 Sep 2023, 17:36:38
//  Auto updated?
//    Yes
// 
//  Description:
//!   Groups shadows of combinators that are shadowing combinators in
//!   [`nom`](::nom)'s [`bytes::complete`](::nom::bytes::complete) module.
// 

use std::fmt::Display;

use nom::{Compare, CompareResult, Err, InputLength, InputTake, IResult};

use crate::nom::{ErrorKind, NomError};


/***** LIBRARY *****/
/// Recognizes a pattern.
///
/// The input data will be compared to the tag combinator's argument and will return the part of
/// the input that matches the argument
///
/// It will return `Err(Err::Error((_, ErrorKind::Tag)))` if the input doesn't match the pattern
/// # Example
/// ```rust
/// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
/// use ast_toolkit::nom::bytes::complete::tag;
///
/// fn parser(s: &str) -> IResult<&str, &str> {
///   tag("Hello")(s)
/// }
///
/// assert_eq!(parser("Hello, World!"), Ok((", World!", "Hello")));
/// assert_eq!(parser("Something"), Err(Err::Error(Error::new("Something", ErrorKind::Tag))));
/// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Tag))));
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
/// # use nom::{Err, error::{Error, ErrorKind}, Needed, IResult};
/// use ast_toolkit::nom::bytes::complete::tag_no_case;
///
/// fn parser(s: &str) -> IResult<&str, &str> {
///   tag_no_case("hello")(s)
/// }
///
/// assert_eq!(parser("Hello, World!"), Ok((", World!", "Hello")));
/// assert_eq!(parser("hello, World!"), Ok((", World!", "hello")));
/// assert_eq!(parser("HeLlO, World!"), Ok((", World!", "HeLlO")));
/// assert_eq!(parser("Something"), Err(Err::Error(Error::new("Something", ErrorKind::Tag))));
/// assert_eq!(parser(""), Err(Err::Error(Error::new("", ErrorKind::Tag))));
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