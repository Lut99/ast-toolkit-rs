//  NOM ERRORS.rs
//    by Lut99
// 
//  Created:
//    09 Sep 2023, 13:33:26
//  Last edited:
//    13 Sep 2023, 17:35:56
//  Auto updated?
//    Yes
// 
//  Description:
//!   Shows how the more verbose [`NomError`] works.
// 

#[cfg(not(feature = "nom-combinators"))]
compile_error!("You must enable the `nom-combinators`-feature to compile the `nom_errors.rs` example");

use ast_toolkit::{Diagnosticable as _, Span};
use ast_toolkit::nom::{bytes::complete as bc, bytes::streaming as bs, character::complete as cc, multi};
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
fn parse_tag_com<'f, 's>(input: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<'f, 's>> {
    bc::tag("foo")(input)
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
fn parse_tag_com_caseless<'f, 's>(input: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<'f, 's>> {
    bc::tag_no_case("foo")(input)
}
/// Triggers a [`not_line_ending()`](::nom::character::complete::not_line_ending()) error, but more verbose.
/// 
/// # Arguments
/// - `input`: The input [`Span`] to parse.
/// 
/// # Errors
/// This function may error if the given input was not what we expected.
#[inline]
fn parse_not_line_ending<'f, 's>(input: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<'f, 's>> {
    cc::not_line_ending(input)
}

/// Triggers a [`tag()`](::nom::bytes::streaming::tag()) error, but more verbose.
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
fn parse_tag_str<'f, 's>(input: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<'f, 's>> {
    bs::tag("foo")(input)
}
/// Triggers a [`tag()`](::nom::bytes::streaming::tag()) error but case insensitive, and more verbose.
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
fn parse_tag_str_caseless<'f, 's>(input: Span<'f, 's>) -> IResult<Span<'f, 's>, Span<'f, 's>, NomError<'f, 's>> {
    bs::tag_no_case("foo")(input)
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
    multi::count(parse_tag_com, 5)(input)
}





/***** ENTRYPOINT *****/
fn main() {
    let sep: String = (0..80).map(|_| '-').collect();

    // Show `tag()` parsing (complete)
    println!();
    println!("{sep}");
    parse_tag_com(Span::new("<example>", "Hello, world!")).unwrap_err().into_diag().emit();
    parse_tag_com(Span::new("<example>", "fo")).unwrap_err().into_diag().emit();
    parse_tag_com(Span::new("<example>", "foO")).unwrap_err().into_diag().emit();
    parse_tag_com_caseless(Span::new("<example>", "Hello, world!")).unwrap_err().into_diag().emit();

    // Show `tag()` parsing (streaming)
    println!();
    println!("{sep}");
    parse_tag_str(Span::new("<example>", "Hello, world!")).unwrap_err().into_diag().emit();
    parse_tag_str(Span::new("<example>", "fo")).unwrap_err().into_diag().emit();
    parse_tag_str(Span::new("<example>", "foO")).unwrap_err().into_diag().emit();
    parse_tag_str_caseless(Span::new("<example>", "Hello, world!")).unwrap_err().into_diag().emit();

    // Show `not_line_ending()` parsing (complete)
    println!();
    println!("{sep}");
    parse_not_line_ending(Span::new("<example>", "Hello, world!\r")).unwrap_err().into_diag().emit();

    // Show `count()` parsing
    println!();
    println!("{sep}");
    parse_foo_5_times(Span::new("<example>", "Hello, world!")).unwrap_err().into_diag().emit();
    parse_foo_5_times(Span::new("<example>", "foofoo")).unwrap_err().into_diag().emit();
    parse_foo_5_times(Span::new("<example>", "foofoobar")).unwrap_err().into_diag().emit();
    parse_foo_5_times(Span::new("<example>", "foofoofoofoofob")).unwrap_err().into_diag().emit();
    println!("{sep}");
    println!();
}
