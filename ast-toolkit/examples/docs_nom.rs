//  DOCS NOM.rs
//    by Lut99
// 
//  Created:
//    03 Sep 2023, 16:17:27
//  Last edited:
//    03 Sep 2023, 16:33:45
//  Auto updated?
//    Yes
// 
//  Description:
//!   Showcases the examples of the docs that require the `nom` feature.
// 

#[cfg(not(feature = "nom"))]
compile_error!("You must enable the `nom`-feature to compile the `docs_nom.rs` example");

use std::str::FromStr as _;

use nom::IResult;
use nom::error::ErrorKind;
use nom::{bytes::complete as bc, character::complete as cc, combinator as comb};
use ast_toolkit::{Diagnostic, NomError, Span, SpanningExt as _};

type Input<'f, 's> = Span<'f, 's>;
type Output<'f, 's, O> = IResult<Input<'f, 's>, O, NomError<Input<'f, 's>>>;


/***** PARSING FUNCTIONS *****/
/// Example parser for showing how the [`NomError`] works for errors only given an [`ErrorKind`](nom::error::ErrorKind).
fn err_kind_parser<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, Span<'f, 's>> {
    bc::tag("Hello, world!")(input)
}

/// Example parser for showing how the [`NomError`] works for errors given an expected character.
fn char_parser<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, char> {
    comb::cut(cc::char('H'))(input)
}

/// Example parser for showing how the [`NomError`] works for external errors.
fn ext_err_parser<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, i64> {
    match i64::from_str(input.text()) {
        Ok(val)  => Ok((Span::empty(input.file, input.source), val)),
        Err(err) => Err(nom::Err::Error(NomError::external_error(input, ErrorKind::Fail, err))),
    }
}





/***** ENTRYPOINT *****/
fn main() {
    // Shows an ErrorKind error
    let err: nom::Err<NomError<Span>> = err_kind_parser(Span::new("<farewell>", "Goodbye, world!")).unwrap_err();
    Diagnostic::from(err).emit();

    // Shows a char error
    let err: nom::Err<NomError<Span>> = char_parser(Span::new("<farewell>", "Goodbye, world!")).unwrap_err();
    Diagnostic::from(err).emit();

    // Shows an external error
    let err: nom::Err<NomError<Span>> = ext_err_parser(Span::new("<farewell>", "Goodbye, world!")).unwrap_err();
    Diagnostic::from(err).emit();
}
