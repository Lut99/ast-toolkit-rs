//  NOM.rs
//    by Lut99
// 
//  Created:
//    22 Jul 2023, 12:35:42
//  Last edited:
//    08 Aug 2023, 15:59:09
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
//!   add(42, 42);
//!   ```
//!   
//!   Where we only define strings and constants as literals.
// 

#[cfg(not(feature = "nom"))]
compile_error!("You must enable the `nom`-feature to compile the `nom.rs` example");

use std::ops::Deref;
use std::str::FromStr as _;

use nom::IResult;
use nom::{bytes::complete as bc, character::complete as cc, combinator as comb, multi, sequence as seq};

use ast_toolkit::Span;


/***** AST *****/
mod ast {
    use ast_toolkit::Span;


    /// Defines the toplevel program node.
    #[derive(Clone, Debug)]
    pub struct Program<F, S> {
        /// The list of function calls we parsed.
        pub calls : Vec<FunctionCall<F, S>>,
        /// The span for the entire (parsed) program
        pub span  : Option<Span<F, S>>,
    }

    /// Defines a statement in our highly simple language, which is a function call over literals
    #[derive(Clone, Debug)]
    pub struct FunctionCall<F, S> {
        /// The name of the function to call
        pub name : String,
        /// The arguments of the call
        pub args : Vec<Literal<F, S>>,
        /// The span for this call
        pub span : Span<F, S>,
    }

    /// Defines the literals we can parse.
    #[derive(Clone, Debug)]
    pub struct Literal<F, S> {
        /// Any variant specifics
        pub variant : LiteralVariant,
        /// The span for this literal
        pub span    : Span<F, S>,
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
type Input<F, S> = Span<F, S>;

/// Defines the output of the parsing functions.
type Output<F, S, T> = IResult<Span<F, S>, T, nom::error::Error<Span<F, S>>>;



// /// Parses the toplevel program for our simple language.
// /// 
// /// # Arguments
// /// - `input`: The input [`Span`] to parse.
// /// 
// /// # Returns
// /// The parsed program and any part of the `input` that wasn't parsed, as a tuple.
// /// 
// /// # Errors
// /// This function can error if we failed to parse any particular input text.
// fn parse_program<F: Clone + PartialEq, S: Clone + PartialEq>(input: Input<F, S>) -> Output<F, S, ast::Program<F, S>> {
//     // Parse a list of calls and process that into a thing
//     comb::map(
//         multi::many0(parse_call),
//         |calls: Vec<ast::FunctionCall<F, S>>| -> ast::Program<F, S> {
//             // Deduce the over-arching span
//             let span: Option<Span<F, S>> = match (calls.first(), calls.last()) {
//                 (Some(c1), Some(c2)) => Some(Span::combined(&c1.span, &c2.span)),
//                 _ => None,
//             };

//             // Done
//             ast::Program {
//                 calls,
//                 span,
//             }
//         },
//     )(input)
// }

// /// Parses a statement (i.e., function call) for our simple language.
// /// 
// /// # Arguments
// /// - `input`: The input [`Span`] to parse.
// /// 
// /// # Returns
// /// The parsed function call and any part of the `input` that wasn't parsed, as a tuple.
// /// 
// /// # Errors
// /// This function can error if we failed to parse any particular input text.
// fn parse_call<F, S>(input: Input<F, S>) -> Output<F, S, ast::FunctionCall<F, S>> {
//     // Parse an identifier, a left brace, a comma-separated list of literals, a right brace and a semicolon
//     comb::map(
//         seq::tuple((
            
//         )),
//         |(): ()| -> ast::FunctionCall<F, S> {

//         },
//     )(input)
// }

// /// Parses a call argument (i.e., a literal) for our simple language
// /// 
// /// # Arguments
// /// - `input`: The input [`Span`] to parse.
// /// 
// /// # Returns
// /// The parsed function call and any part of the `input` that wasn't parsed, as a tuple.
// /// 
// /// # Errors
// /// This function can error if we failed to parse any particular input text.
// fn parse_lit<F, S>(input: Input<F, S>) -> Output<F, S, ast::FunctionCall<F, S>> {

// }

// /// Parses a call argument (i.e., a literal) for our simple language
// /// 
// /// # Arguments
// /// - `input`: The input [`Span`] to parse.
// /// 
// /// # Returns
// /// The parsed function call and any part of the `input` that wasn't parsed, as a tuple.
// /// 
// /// # Errors
// /// This function can error if we failed to parse any particular input text.
// fn parse_lit<F, S>(input: Input<F, S>) -> Output<F, S, ast::FunctionCall<F, S>> {

// }

// /// Parses an integer literal for our simple language
// /// 
// /// # Arguments
// /// - `input`: The input [`Span`] to parse.
// /// 
// /// # Returns
// /// The parsed value and any part of the `input` that wasn't parsed, as a tuple.
// /// 
// /// # Errors
// /// This function can error if we failed to parse any particular input text.
// fn parse_integer<F: Clone, S: Clone + Deref<Target = str>>(input: Input<F, S>) -> Output<F, S, i64> {
//     comb::map(
//         comb::recognize(seq::pair(
//             comb::opt(bc::tag("-")),
//             cc::digit0,
//         )),
//         |parsed: Span<F, S>| -> i64 { i64::from_str(&parsed).unwrap() }
//     )(input)
// }





/***** ENTYRPOINT *****/
fn main() {
    
}