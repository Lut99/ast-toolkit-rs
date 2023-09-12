//  NOM ERRORS.rs
//    by Lut99
// 
//  Created:
//    09 Sep 2023, 13:33:26
//  Last edited:
//    12 Sep 2023, 16:06:52
//  Auto updated?
//    Yes
// 
//  Description:
//!   Shows how the more verbose [`NomError`] works.
// 

#[cfg(not(feature = "nom-combinators"))]
compile_error!("You must enable the `nom-combinators`-feature to compile the `nom_errors.rs` example");

use ast_toolkit::{Diagnosticable as _, Span};
use ast_toolkit::nom::bytes::complete::{tag, tag_no_case};
use ast_toolkit::nom::multi::count;
use nom::IResult;

type NomError<'f, 's> = ast_toolkit::NomError<Span<'f, 's>>;


/***** PARSERS *****/
/// Triggers a [`tag()`](::nom::bytes::complete::tag()) error, but more verbose.
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
fn parse_tag<'f, 's>(input: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<'f, 's>> {
    tag("foo")(input)
}
/// Triggers a [`tag()`](::nom::bytes::complete::tag()) error but case insensitive, and more verbose.
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
fn parse_tag_caseless<'f, 's>(input: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<'f, 's>> {
    tag_no_case("foo")(input)
}

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
    count(parse_tag, 5)(input)
}





/***** ENTRYPOINT *****/
fn main() {
    // Show `tag()` parsing
    parse_tag(Span::new("<example>", "Hello, world!")).unwrap_err().into_diag().emit();
    parse_tag(Span::new("<example>", "foO")).unwrap_err().into_diag().emit();
    parse_tag_caseless(Span::new("<example>", "Hello, world!")).unwrap_err().into_diag().emit();

    // Show `count()` parsing
    parse_foo_5_times(Span::new("<example>", "Hello, world!")).unwrap_err().into_diag().emit();
    parse_foo_5_times(Span::new("<example>", "foofoobar")).unwrap_err().into_diag().emit();
    parse_foo_5_times(Span::new("<example>", "foofoofoofoofob")).unwrap_err().into_diag().emit();
}
