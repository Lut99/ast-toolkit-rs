//  NOM ERRORS.rs
//    by Lut99
// 
//  Created:
//    09 Sep 2023, 13:33:26
//  Last edited:
//    09 Sep 2023, 13:41:36
//  Auto updated?
//    Yes
// 
//  Description:
//!   Shows how the more verbose [`NomError`] works.
// 

use ast_toolkit::{Diagnosticable as _, Span};
use ast_toolkit::nom::multi::count;
use nom::IResult;
use nom::bytes::complete as bc;

type NomError<'f, 's> = ast_toolkit::NomError<Span<'f, 's>>;


/***** PARSERS *****/
/// Triggers a [`count()`](::nom::multi::count()) error, but more verbose.
/// 
/// # Arguments
/// - `input`: The input [`Span`] to parse.
/// 
/// # Returns
/// The remainder input span and the parsed repetitions.
/// 
/// # Errors
/// This function may error if the given input was not what we expected.
#[inline]
fn parse_foo_5_times<'f, 's>(input: Span<'f, 's>) -> IResult<Span<'f, 's>, Vec<Span<'f, 's>>, NomError<'f, 's>> {
    count(bc::tag("foo"), 5)(input)
}





/***** ENTRYPOINT *****/
fn main() {
    // Show `count()` parsing
    parse_foo_5_times(Span::new("<example>", "Hello, world!")).unwrap_err().into_diag().emit();
}
