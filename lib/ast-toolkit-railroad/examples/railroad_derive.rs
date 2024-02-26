//  RAILROAD DERIVE.rs
//    by Lut99
//
//  Created:
//    22 Feb 2024, 14:26:07
//  Last edited:
//    26 Feb 2024, 13:25:18
//  Auto updated?
//    Yes
//
//  Description:
//!   Showcases generating railroad diagrams based on ASTs using the
//!   `railroad`-feature with macros provided by the `derive`-feature.
//

#![allow(dead_code)]

#[cfg(not(feature = "derive"))]
compile_error!("Please enable the 'derive' feature when running the 'railroad_derive' example.");

use ast_toolkit_railroad::{diagram_svg, railroad as rr, ToDelimNode, ToNode, ToNonTerm};
use rr::Diagram;


/***** EXAMPLE AST *****/
/// Root node for a simple expression language.
///
/// NOTE: We derive [`ToNonTerm`] instead of [`ToNode`] because we want to show this node at the toplevel.  
/// This is usually done for nodes that are either toplevel nodes, or often used/recursive.
///
/// The printed name is always equal to the type's name.
#[derive(ToNonTerm)]
struct Program {
    /// expressions
    #[railroad(optional)]
    exprs: Vec<Expr>,
}

/// Expression for a simple expression language.
///
/// NOTE: We derive [`ToNonTerm`] instead of [`ToNode`] because we want to show this node at the toplevel.  
/// This is usually done for nodes that are either toplevel nodes, or often used/recursive (all cases hold for Expr).
///
/// The printed name is always equal to the type's name.
#[derive(ToNonTerm)]
enum Expr {
    /// Parenthesis
    Paren(ExprParen),
    /// Parenthesis, but optional
    OptParen(ExprOptParen),

    /// Addition
    Add(ExprBinOp<Plus>),
    /// Subtraction
    Sub(ExprBinOp<Minus>),
    /// Multiplication
    Mul(ExprBinOp<Star>),
    /// Division
    Div(ExprBinOp<Slash>),

    /// Literals
    Lit(Lit),
}



/// Represents an [`Expr`] wrapped in parenthesis.
///
/// This derives as a [`railroad::Sequence`] for all fields.
///
/// NOTE: We derive [`ToDelimNode`] instead of [`ToNode`] because we want to be able to express that it's actually two tokens with stuff in between.  
/// This is usually done for nodes that are either toplevel nodes, or often used/recursive (all cases hold for Expr).
#[derive(ToNode)]
struct ExprParen {
    /// A token representing the parenthesis.
    ///
    /// NOTE: We use `#[delim(expr)]` here to represent which other parts of the expression are delimited.  
    /// Given as a comma-separated list, but now with only one entry.
    #[railroad(delim(expr))]
    paren_token: Paren,
    /// The expression wrapped in parenthesis.
    expr: Box<Expr>,
}

/// Represents an [`Expr`] wrapped in parenthesis.
///
/// This derives as a [`railroad::Sequence`] for all fields.
///
/// NOTE: We derive [`ToDelimNode`] instead of [`ToNode`] because we want to be able to express that it's actually two tokens with stuff in between.  
/// This is usually done for nodes that are either toplevel nodes, or often used/recursive (all cases hold for Expr).
#[derive(ToNode)]
struct ExprOptParen {
    /// A token representing the parenthesis.
    ///
    /// NOTE: We use `#[delim(expr)]` here to represent which other parts of the expression are delimited.  
    /// Given as a comma-separated list, but now with only one entry.
    #[railroad(delim(expr), optional, comment = "optional comment")]
    paren_token: Option<Paren>,
    /// The expression wrapped in parenthesis.
    expr: Box<Expr>,
}

/// Represents `()` tokens.
///
/// We choose to model delimited token as one object to assert their parity.  
/// As such, we derive [`ToDelimNode`] instead of [`ToNode`] to express this difference.
///
/// Note that this variant is extremely simple. It always derives an implementation that generates terminals with the delimiters.
#[derive(ToDelimNode)]
#[railroad(open = "(", close = ")")]
struct Paren;



/// Represents a binary operation in the expression language.
///
/// This derives as a [`railroad::Sequence`] for all fields.
#[derive(ToNode)]
struct ExprBinOp<O: ToNode> {
    /// The lefthand-side of the operation.
    lhs: Box<Expr>,
    /// The operation to apply.
    op:  O,
    /// The righthand-side of the operation.
    #[railroad(comment = "rhs")]
    rhs: Box<Expr>,
}

/// Represents a `+`.
#[derive(ToNode)]
#[railroad(token = "+")]
struct Plus;
/// Represents a `-`.
#[derive(ToNode)]
#[railroad(token = "-")]
struct Minus;
/// Represents a `*`.
#[derive(ToNode)]
#[railroad(token = "*")]
struct Star;
/// Represents a `/`.
#[derive(ToNode)]
#[railroad(token = "/")]
struct Slash;



/// Represents a literal number.
#[derive(ToNode)]
#[railroad(regex = "^[0-9]+$")]
struct Lit(i64);





/***** ENTRYPOINT *****/
fn main() {
    // Build a diagram out of it!
    let diagram: Diagram<_> = diagram_svg!(Program, Expr);
    println!("{diagram}");
}
