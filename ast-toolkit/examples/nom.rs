//  NOM.rs
//    by Lut99
// 
//  Created:
//    22 Jul 2023, 12:35:42
//  Last edited:
//    31 Aug 2023, 23:31:14
//  Auto updated?
//    Yes
// 
//  Description:
//!   Shows an example for using the [`ast_toolkit::Span`] with [`nom`].
//!   
//!   Note: you manually need to enable the `nom`-feature for this:
//!   ```bash
//!   cargo run --example nom --features nom
//!   ```
//!   
//!   In this example, we parse a highly simple language of functions applied
//!   over literals, e.g.,:
//!   ```
//!   hello_world();
//!   hello_x("world");
//!   add(42,42);
//!   ```
//!   
//!   Where we only define strings and constants as literals.
// 

#[cfg(not(feature = "nom"))]
compile_error!("You must enable the `nom`-feature to compile the `nom.rs` example");

use std::str::FromStr as _;

use nom::{IResult, Parser};
use nom::error::{ErrorKind, FromExternalError as _};
use nom::{branch, bytes::complete as bc, character::complete as cc, combinator as comb, error, multi, sequence as seq};
use unicode_segmentation::UnicodeSegmentation as _;

use ast_toolkit::{Diagnostic, NomError, Span, SpanningExt as _};


/***** AST *****/
mod ast {
    use ast_toolkit::Span;


    /// Defines the toplevel program node.
    #[derive(Clone, Debug)]
    pub struct Program<'f, 's> {
        /// The list of function calls we parsed.
        pub calls : Vec<FunctionCall<'f, 's>>,
        /// The span for the entire (parsed) program
        pub span  : Option<Span<'f, 's>>,
    }

    /// Defines a statement in our highly simple language, which is a function call over literals
    #[derive(Clone, Debug)]
    pub struct FunctionCall<'f, 's> {
        /// The name of the function to call
        pub name : String,
        /// The arguments of the call
        pub args : Vec<Literal<'f, 's>>,
        /// The span for this call
        pub span : Span<'f, 's>,
    }

    /// Defines the literals we can parse.
    #[derive(Clone, Debug)]
    pub struct Literal<'f, 's> {
        /// Any variant specifics
        pub variant : LiteralVariant,
        /// The span for this literal
        pub span    : Span<'f, 's>,
    }

    /// Actually defines the possible literal variants
    #[derive(Clone, Debug)]
    pub enum LiteralVariant {
        /// String literals
        String(String),
        /// Integral literals
        Integer(i64),
    }
}





/***** PARSING FUNCTIONS *****/
/// Defines the input of the parsing functions.
type Input<'f, 's> = Span<'f, 's>;

/// Defines the output of the parsing functions.
type Output<'f, 's, T> = IResult<Span<'f, 's>, T, NomError<'f, Span<'f, 's>>>;



/// Parses the toplevel program for our simple language.
/// 
/// # Arguments
/// - `input`: The input [`Span`] to parse.
/// 
/// # Returns
/// The parsed program and any part of the `input` that wasn't parsed, as a tuple.
/// 
/// # Errors
/// This function can error if we failed to parse any particular input text.
fn parse_program<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, ast::Program<'f, 's>> where 's: 'f {
    // Parse a list of calls and process that into a thing
    comb::map(
        // multi::separated_list0(cc::multispace0, parse_call),
        multi::many0(whitespace(parse_call)),
        |calls: Vec<ast::FunctionCall<'f, 's>>| -> ast::Program<'f, 's> {
            // Deduce the over-arching span
            let span: Option<Span<'f, 's>> = match (calls.first(), calls.last()) {
                (Some(c1), Some(c2)) => Some(Span::combined(c1.span, c2.span)),
                _ => None,
            };

            // Done
            ast::Program {
                calls,
                span,
            }
        },
    )(input)
}

/// Parses a statement (i.e., function call) for our simple language.
/// 
/// # Arguments
/// - `input`: The input [`Span`] to parse.
/// 
/// # Returns
/// The parsed function call and any part of the `input` that wasn't parsed, as a tuple.
/// 
/// # Errors
/// This function can error if we failed to parse any particular input text.
fn parse_call<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, ast::FunctionCall<'f, 's>> where 's: 'f {
    // Parse an identifier, a left brace, a comma-separated list of literals, a right brace and a semicolon
    error::context("function call", comb::map(
        seq::pair(
            comb::recognize(seq::pair(branch::alt((cc::alpha1, bc::tag("_"))), multi::many0(branch::alt((cc::alphanumeric1, bc::tag("_")))))),
            comb::cut(seq::pair(
                seq::delimited(
                    cc::char('('),
                    multi::separated_list0(cc::char(','), parse_lit),
                    cc::char(')'),
                ),
                bc::tag(";"),
            )),
        ),
        |(id, (args, semicolon)): (Span<'f, 's>, (Vec<ast::Literal<'f, 's>>, Span<'f, 's>))| -> ast::FunctionCall<'f, 's> {
            ast::FunctionCall {
                name : id.text().into(),
                args,
                span : Span::combined(id, semicolon),
            }
        },
    ))(input)
}

/// Parses a call argument (i.e., a literal) for our simple language
/// 
/// # Arguments
/// - `input`: The input [`Span`] to parse.
/// 
/// # Returns
/// The parsed literal and any part of the `input` that wasn't parsed, as a tuple.
/// 
/// # Errors
/// This function can error if we failed to parse any particular input text.
fn parse_lit<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, ast::Literal<'f, 's>> where 's: 'f {
    branch::alt((
        parse_string,
        parse_integer,
    ))(input)
}

/// Parses a string literal for our simple language
/// 
/// # Arguments
/// - `input`: The input [`Span`] to parse.
/// 
/// # Returns
/// The parsed string (as a literal) and any part of the `input` that wasn't parsed, as a tuple.
/// 
/// # Errors
/// This function can error if we failed to parse any particular input text.
fn parse_string<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, ast::Literal<'f, 's>> {
    error::context("string literal",
        comb::map(
            comb::recognize(seq::pair(
                cc::char('\"'),
                comb::cut(seq::pair(
                    // bc::escaped(bc::is_not("\"\\"), '\\', cc::one_of("\"ntr\\\'")),
                    bc::is_not("\"\\"),
                    cc::char('\"'),
                )),
            )),
            |lit: Span<'f, 's>| -> ast::Literal<'f, 's> {
                // Get the string without quotes
                let text: &str = &lit.text()[1..lit.len() - 1];

                // Tie the contents together
                let mut value: String = String::new();
                let mut escaped: bool = false;
                for c in text.graphemes(true) {
                    if !escaped && c == "\n" {
                        escaped = true;
                    } else if escaped {
                        match c {
                            "\"" => value.push('\"'),
                            "n"  => value.push('\n'),
                            "t"  => value.push('\t'),
                            "r"  => value.push('\r'),
                            "'"  => value.push('\''),
                            _    => { unreachable!(); },
                        }
                    } else {
                        value.push_str(c);
                    }
                }

                // Return the literal
                ast::Literal {
                    variant : ast::LiteralVariant::String(value),
                    span    : lit,
                }
            }
        )
    )(input)
}

/// Parses an integer literal for our simple language
/// 
/// # Arguments
/// - `input`: The input [`Span`] to parse.
/// 
/// # Returns
/// The parsed integer (as a literal) and any part of the `input` that wasn't parsed, as a tuple.
/// 
/// # Errors
/// This function can error if we failed to parse any particular input text.
fn parse_integer<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, ast::Literal<'f, 's>> {
    comb::map(
        comb::recognize(seq::pair(
            comb::opt(bc::tag("-")),
            cc::digit1,
        )),
        |parsed: Span<'f, 's>| -> ast::Literal<'f, 's> {
            ast::Literal {
                variant : ast::LiteralVariant::Integer(i64::from_str(parsed.text()).unwrap()),
                span    : parsed,
            }
        }
    )(input)
}

/// Meta-parser that parses the given parser with optional surrounding whitespace.
/// 
/// # Arguments
/// - `parser`: The parser to parse.
/// 
/// # Returns
/// A new parser that parsers this parser with optional surrounding whitespace.
fn whitespace<'f, 's, O>(mut parser: impl Parser<Input<'f, 's>, O, NomError<'f, Span<'f, 's>>>) -> impl Parser<Input<'f, 's>, O, NomError<'f, Span<'f, 's>>> {
    move |input: Input<'f, 's>| -> Output<'f, 's, O> {
        // Parse the whitespace first
        let (rem, _) = cc::multispace0.parse(input)?;
        // Parse the middle bit
        let (rem, res) = parser.parse(rem)?;
        // Parse the whitespace again
        let (rem, _) = cc::multispace0.parse(rem)?;
        Ok((rem, res))
    }
}



/// A parser for showing a specific error kind error.
fn trigger_err_kind<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, Vec<Span<'f, 's>>> {
    multi::many1(comb::peek(bc::tag("Hello")))(input)
}

/// A parser for showing a custom error.
fn trigger_custom_error<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, i64> {
    // Simply attempt to parse the input
    match i64::from_str(input.text()) {
        Ok(value) => Ok((Span::empty(input.file, input.source), value)),
        Err(err)  => Err(nom::Err::Error(NomError::from_external_error(input, ErrorKind::Fail, err))),
    }
}





/***** ENTYRPOINT *****/
fn main() {
    let file: &str = "<hardcoded>";
    let source: &str = "hello_world();\nhello_x(\"world\");\nadd(42,42);";
    let input: Span = Span::new(file, source);

    // Parse it
    let (_, value) = parse_program(input).unwrap();

    // We parsed it successfully! We can get what we like
    println!();
    for call in &value.calls {
        println!("User used function '{}'!", call.name);
        for (i, arg) in call.args.iter().enumerate() {
            println!(" - Argument {}: {:?}", i + 1, arg.variant);
        }
        println!();
    }

    // Let us examine a few errors!
    // A specific error kind (not very verbose, since nom does not give us a lot to work with...)
    Diagnostic::from(trigger_err_kind(Span::new("<error>", "Hello, world!")).unwrap_err()).emit();
    // Unexpected character
    Diagnostic::from(parse_program(Span::new("<error>", "hello_world!();")).unwrap_err()).emit();
    // External error
    Diagnostic::from(trigger_custom_error(Span::new("<error>", "42#")).unwrap_err()).emit();
}
