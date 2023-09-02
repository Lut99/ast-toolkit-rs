//  NOM.rs
//    by Lut99
// 
//  Created:
//    22 Jul 2023, 12:35:42
//  Last edited:
//    02 Sep 2023, 12:15:10
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

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::str::FromStr as _;

use nom::{IResult, Parser};
use nom::error::{ErrorKind, FromExternalError as _};
use nom::{branch, bytes::complete as bc, character::complete as cc, combinator as comb, error, multi, sequence as seq};
use unicode_segmentation::UnicodeSegmentation as _;

use ast_toolkit::{Combining as _, Diagnostic, NomError, Span, SpanningExt as _};


/***** AST *****/
mod ast {
    use std::fmt::{Display, Formatter, Result as FResult};
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



    /// Seperate "node" that defines the possible data types.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub enum DataType {
        /// It evaluates to a string of characters.
        String,
        /// It evaluates to a number.
        Integer,
    }
    impl Display for DataType {
        fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
            match self {
                DataType::String  => write!(f, "string"),
                DataType::Integer => write!(f, "integer"),
            }
        }
    }
}





/***** ERRORS *****/
/// Describes anything that goes wrong during type analysis.
#[derive(Debug, Diagnostic)]
enum TypeError<'f, 's> {
    /// The given call arguments was incorrect for the given call
    #[diag(error, span=arg_span)]
    #[diag(note, message="Function '{func}' defined here", span=call_span)]
    CallArgument { func: String, i: usize, got: ast::DataType, expected: ast::DataType, call_span: Span<'f, 's>, arg_span: Span<'f, 's> },
}
impl<'f, 's> Display for TypeError<'f, 's> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use TypeError::*;
        match self {
            CallArgument { func, i, got, expected, .. } => write!(f, "Argument {i} to function {func} has type {got} but needs to be type {expected}"),
        }
    }
}
impl<'f, 's> Error for TypeError<'f, 's> {}





/***** PARSING FUNCTIONS *****/
/// Defines the input of the parsing functions.
type Input<'f, 's> = Span<'f, 's>;

/// Defines the output of the parsing functions.
type Output<'f, 's, T> = IResult<Span<'f, 's>, T, NomError<Span<'f, 's>>>;



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
fn parse_program<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, ast::Program<'f, 's>> {
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
fn parse_call<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, ast::FunctionCall<'f, 's>> {
    // Parse an identifier, a left brace, a comma-separated list of literals, a right brace and a semicolon
    error::context("a function call", comb::map(
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
fn parse_lit<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, ast::Literal<'f, 's>> {
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
    error::context("a string literal",
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
fn whitespace<'f, 's, O>(mut parser: impl Parser<Input<'f, 's>, O, NomError<Span<'f, 's>>>) -> impl Parser<Input<'f, 's>, O, NomError<Span<'f, 's>>> {
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

/// A parser for showing a trace of multiple errors.
fn trigger_stack_error<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, Vec<i64>> {
    // Simply attempt to parse the input, but in a multi::many kind of situation
    multi::many1(trigger_custom_error)(input)
}

/// A parser for showing a branch of possible things tried.
fn trigger_branch_error<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, Span<'f, 's>> {
    // Simply attempt to parse the input, but in a multi::many kind of situation
    branch::alt((comb::recognize(trigger_custom_error), bc::tag("test")))(input)
}

/// A parser for showing an end-of-file error (warning, really)
fn trigger_eof_warn<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, Span<'f, 's>> {
    nom::bytes::streaming::tag("test")(input)
}




/***** TRAVERSAL FUNCTIONS *****/
/// Verifies if the types are correct on a program level.
/// 
/// # Arguments
/// - `program`: The [`Program`](ast::Program) to analyse.
/// 
/// # Errors
/// This function errors if there were inconsistencies in the typing.
fn type_program<'f, 's>(program: &ast::Program<'f, 's>) -> Result<(), TypeError<'f, 's>> {
    // Go through the calls and validate them
    for call in &program.calls {
        type_call(call)?;
    }
    Ok(())
}

/// Verifies if the types are correct on a call level.
/// 
/// For demonstration purposes, this simply compares hardcoded function call identifiers with expected types.
/// 
/// # Arguments
/// - `call`: The [`FunctionCall`](ast::FunctionCall) to analyse.
/// 
/// # Errors
/// This function errors if there were inconsistencies in the typing.
fn type_call<'f, 's>(call: &ast::FunctionCall<'f, 's>) -> Result<(), TypeError<'f, 's>> {
    // Check the name of the function
    match call.name.as_str() {
        "add" => {
            // You can only add numbers
            for (i, arg) in call.args.iter().enumerate() {
                let arg_type: ast::DataType = get_lit_type(arg);
                if arg_type != ast::DataType::Integer { return Err(TypeError::CallArgument { func: call.name.clone(), i, got: arg_type, expected: ast::DataType::Integer, call_span: call.span, arg_span: arg.span }); }
            }
            Ok(())
        },

        // The rest is up for grabs
        _ => Ok(()),
    }
}

/// Returns the type of the given literal.
/// 
/// # Arguments
/// - `lit`: The [`Literal`](ast::Literal) to analyse.
/// 
/// # Returns
/// The [`DataType`](ast::DataType) of the literal.
fn get_lit_type(lit: &ast::Literal) -> ast::DataType {
    match &lit.variant {
        ast::LiteralVariant::String(_)  => ast::DataType::String,
        ast::LiteralVariant::Integer(_) => ast::DataType::Integer,
    }
}





/***** ENTYRPOINT *****/
fn main() {
    let file: &str = "<hardcoded>";
    let source: &str = "hello_world();\nhello_x(\"world\");\nadd(42,42);";
    let input: Span = Span::new(file, source);

    // Parse it
    let (_, program) = parse_program(input).unwrap();

    // We parsed it successfully! We can get what we like
    println!();
    for call in &program.calls {
        println!("User used function '{}'!", call.name);
        for (i, arg) in call.args.iter().enumerate() {
            println!(" - Argument {}: {:?}", i + 1, arg.variant);
        }
        println!();
    }
    eprintln!();
    eprintln!("{}", (0..80).map(|_| '-').collect::<String>());
    eprintln!();
    eprintln!();



    /* SYNTAX ERRORS */
    // A specific error kind (not very verbose, since nom does not give us a lot to work with...)
    Diagnostic::from(trigger_err_kind(Span::new("<error>", "Hello, world!")).unwrap_err()).emit();
    // Unexpected character
    Diagnostic::from(parse_program(Span::new("<error>", "hello_world!();")).unwrap_err()).emit();
    // External error
    Diagnostic::from(trigger_custom_error(Span::new("<error>", "42#")).unwrap_err()).emit();

    // A whole sequence of errors
    Diagnostic::from(trigger_stack_error(Span::new("<error>", "42#")).unwrap_err()).emit();
    // A branching error
    Diagnostic::from(trigger_branch_error(Span::new("<error>", "42#")).unwrap_err()).emit();

    // Finally, end-of-file.
    Diagnostic::from(trigger_eof_warn(Span::new("<warn>", "tes")).unwrap_err()).emit();

    eprintln!("{}", (0..80).map(|_| '-').collect::<String>());
    eprintln!();
    eprintln!();



    /* COMPILE ERRORS */
    // Let's say we attempt to verify the types of function calls
    let wrong_source: &str = "add(\"Hello there!\");";
    let (_, wrong_program) = parse_program(Span::new(file, wrong_source)).unwrap();
    if let Err(err) = type_program(&wrong_program) {
        Diagnostic::from(err).emit();
    }
}
