//  MULTI.rs
//    by Lut99
// 
//  Created:
//    09 Sep 2023, 12:58:13
//  Last edited:
//    18 Sep 2023, 16:31:34
//  Auto updated?
//    Yes
// 
//  Description:
//!   Groups shadows of combinators that are shadowing combinators in
//!   [`nom`](::nom)'s [`multi`](::nom::multi) module.
// 

use ::nom::{Err, IResult, Parser};

use super::{ErrorKind, NomError};


/***** CONSTANTS *****/
/// Don't pre-allocate more than 64KiB when calling `Vec::with_capacity`.
///
/// Pre-allocating memory is a nice optimization but count fields can't
/// always be trusted. We should clamp initial capacities to some reasonable
/// amount. This reduces the risk of a bogus count value triggering a panic
/// due to an OOM error.
///
/// This does not affect correctness. Nom will always read the full number
/// of elements regardless of the capacity cap.
const MAX_INITIAL_CAPACITY_BYTES: usize = 65536;





/***** LIBRARY *****/
/// Runs the embedded parser `count` times, gathering the results in a `Vec`.
/// 
/// _Note: This version is identical to the [original one](::nom::multi::count()), except that it returns the more verbose [`ErrorKind`] in the [`ast_toolkit`](crate)._
///
/// # Arguments
/// * `f` The parser to apply.
/// * `count` How often to apply the parser.
/// ```rust
/// # use nom::{Err, Needed, IResult};
/// # use ast_toolkit::diagnostic::Diagnosticable as _;
/// # use ast_toolkit::span::Span;
/// use ast_toolkit::nom::{ErrorKind, NomError};
/// use ast_toolkit::nom::{bytes::complete::tag, multi::count};
///
/// fn parser<'f, 's>(s: Span<'f, 's>) -> IResult<Span<'f, 's>, Vec<Span<'f, 's>>, NomError<Span<'f, 's>>> {
///   count(tag("abc"), 2)(s)
/// }
/// 
/// let input1 = Span::new("<example>", "abcabc");
/// let input2 = Span::new("<example>", "abc123");
/// let input3 = Span::new("<example>", "123123");
/// let input4 = Span::new("<example>", "");
/// let input5 = Span::new("<example>", "abcabcabc");
///
/// assert_eq!(parser(input1), Ok((input1.emptied(), vec![input1.range(0..3), input1.range(3..6)])));
/// assert_eq!(parser(input2).unwrap_err().into_diag().message(), "Expected 'abc'");
/// assert_eq!(parser(input3).unwrap_err().into_diag().message(), "Expected 'abc'");
/// assert_eq!(parser(input4).unwrap_err().into_diag().message(), "Expected 'abc'");
/// assert_eq!(parser(input5), Ok((input5.range(6..9), vec![input5.range(0..3), input5.range(3..6)])));
/// ```
pub fn count<I, O, F>(mut f: F, count: usize) -> impl FnMut(I) -> IResult<I, Vec<O>, NomError<I>>
where
  I: Clone + PartialEq,
  F: Parser<I, O, NomError<I>>,
{
    move |i: I| {
        let mut input = i.clone();
        let max_initial_capacity = MAX_INITIAL_CAPACITY_BYTES / ::nom::lib::std::mem::size_of::<O>().max(1);
        let mut res =::nom::lib::std::vec::Vec::with_capacity(count.min(max_initial_capacity));
    
        for c in 0..count {
            let input_ = input.clone();
            match f.parse(input_) {
                Ok((i, o)) => {
                    res.push(o);
                    input = i;
                }
                Err(Err::Error(e)) => {
                    return Err(Err::Error(NomError::append_kind(i, ErrorKind::count(c, count), e)));
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Ok((input, res))
    }
}



/// Runs the embedded parser repeatedly, filling the given slice with results.
///
/// This parser fails if the input runs out before the given slice is full.
/// 
/// _Note: This version is identical to the [original one](::nom::multi::fill()), except that it returns the more verbose [`ErrorKind`] in the [`ast_toolkit`](crate)._
///
/// # Arguments
/// * `f` The parser to apply.
/// * `buf` The slice to fill
/// ```rust
/// # use nom::{Err, Needed, IResult};
/// # use ast_toolkit::diagnostic::Diagnosticable as _;
/// # use ast_toolkit::span::Span;
/// use ast_toolkit::nom::{ErrorKind, NomError};
/// use ast_toolkit::nom::{bytes::complete::tag, multi::fill};
///
/// fn parser<'f, 's>(s: Span<'f, 's>) -> IResult<Span<'f, 's>, [Span<'f, 's>; 2], NomError<Span<'f, 's>>> {
///   let mut buf = [Span::new("", ""), Span::new("", "")];
///   let (rest, ()) = fill(tag("abc"), &mut buf)(s)?;
///   Ok((rest, buf))
/// }
/// 
/// let input1: Span = Span::new("<example>", "abcabc");
/// let input2: Span = Span::new("<example>", "abc123");
/// let input3: Span = Span::new("<example>", "123123");
/// let input4: Span = Span::new("<example>", "");
/// let input5: Span = Span::new("<example>", "abcabcabc");
///
/// assert_eq!(parser(input1), Ok((input1.emptied(), [input1.range(0..3), input1.range(3..6)])));
/// assert_eq!(parser(input2).unwrap_err().into_diag().message(), "Expected 'abc'");
/// assert_eq!(parser(input3).unwrap_err().into_diag().message(), "Expected 'abc'");
/// assert_eq!(parser(input4).unwrap_err().into_diag().message(), "Expected 'abc'");
/// assert_eq!(parser(input5), Ok((input5.range(6..9), [input5.range(0..3), input5.range(3..6)])));
/// ```
pub fn fill<'a, I, O, F>(f: F, buf: &'a mut [O]) -> impl FnMut(I) -> IResult<I, (), NomError<I>> + 'a
where
  I: Clone + PartialEq,
  F: Fn(I) -> IResult<I, O, NomError<I>> + 'a,
{
    move |i: I| {
        let mut input = i.clone();

        let buf_len: usize = buf.len();
        for (n, elem) in buf.iter_mut().enumerate() {
            let input_ = input.clone();
            match f(input_) {
                Ok((i, o)) => {
                    *elem = o;
                    input = i;
                }
                Err(Err::Error(e)) => {
                    // Inject the extra-contextful Count error
                    return Err(Err::Error(NomError::append_kind(i, ErrorKind::count(n, buf_len), e)));
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Ok((input, ()))
    }
}



// /// Gets a number from the first parser,
// /// then applies the second parser that many times.
// /// 
// /// _Note: This version is identical to the [original one](::nom::multi::length_count()), except that it returns the more verbose [`ErrorKind`] in the [`ast_toolkit`](crate)._
// /// 
// /// # Arguments
// /// * `f` The parser to apply to obtain the count.
// /// * `g` The parser to apply repeatedly.
// /// ```rust
// /// # use nom::{Err, Needed, IResult};
// /// use nom::number::complete::u8;
// /// use nom::combinator::map;
// /// use ast_toolkit::nom::{ErrorKind, NomError};
// /// use ast_toolkit::nom::{bytes::complete::tag, multi::length_count};
// ///
// /// fn parser(s: &[u8]) -> IResult<&[u8], Vec<&[u8]>, NomError<Span<'f, 's>> {
// ///   length_count(map(u8, |i| {
// ///      println!("got number: {}", i);
// ///      i
// ///   }), tag("abc"))(s)
// /// }
// ///
// /// assert_eq!(parser(&b"\x02abcabcabc"[..]), Ok(((&b"abc"[..], vec![&b"abc"[..], &b"abc"[..]]))));
// /// assert_eq!(parser(b"\x03123123123"), Err(Err::Error(Error::new(&b"123123123"[..], ErrorKind::Tag))));
// /// ```
// pub fn length_count<I, O, N, F, G>(mut f: F, mut g: G) -> impl FnMut(I) -> IResult<I, Vec<O>, NomError<I>>
// where
//   I: Clone,
//   N: ToUsize,
//   F: Parser<I, N, NomError<I>>,
//   G: Parser<I, O, NomError<I>>,
// {
//     move |i: I| {
//         let (i, count) = f.parse(i)?;
//         let mut input = i.clone();
//         let mut res = Vec::new();

//         let count: usize = count.to_usize();
//         for n in 0..count {
//             let input_ = input.clone();
//             match g.parse(input_) {
//                 Ok((i, o)) => {
//                     res.push(o);
//                     input = i;
//                 }
//                 Err(Err::Error(e)) => {
//                     // Inject the extra-contextful Count error
//                     return Err(Err::Error(NomError::append_kind(i, ErrorKind::count(n, count), e)));
//                 }
//                 Err(e) => {
//                     return Err(e);
//                 }
//             }
//         }

//         Ok((input, res))
//     }
// }
