//  RAILROAD DERIVE.rs
//    by Lut99
//
//  Created:
//    22 Feb 2024, 14:26:07
//  Last edited:
//    25 Feb 2024, 15:23:40
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

use ast_toolkit_railroad::{diagram, railroad as rr, ToDelimNode, ToNode, ToNonTerm};
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
struct Plus;
impl ToNode for Plus {
    type Node = rr::Terminal;

    #[inline]
    fn railroad() -> Self::Node { rr::Terminal::new("+".into()) }
}
/// Represents a `-`.
struct Minus;
impl ToNode for Minus {
    type Node = rr::Terminal;

    #[inline]
    fn railroad() -> Self::Node { rr::Terminal::new("-".into()) }
}
/// Represents a `*`.
struct Star;
impl ToNode for Star {
    type Node = rr::Terminal;

    #[inline]
    fn railroad() -> Self::Node { rr::Terminal::new("*".into()) }
}
/// Represents a `/`.
struct Slash;
impl ToNode for Slash {
    type Node = rr::Terminal;

    #[inline]
    fn railroad() -> Self::Node { rr::Terminal::new("/".into()) }
}



/// Represents a literal number.
struct Lit(i64);
impl ToNode for Lit {
    type Node = rr::Sequence<Box<dyn rr::Node>>;

    #[inline]
    fn railroad() -> Self::Node {
        rr::Sequence::new(vec![Box::new(rr::Comment::new("regex".into())), Box::new(rr::Terminal::new("^[0-9]+$".into()))])
    }
}





/***** ENTRYPOINT *****/
fn main() {
    // Build a diagram out of it!
    let mut diagram: Diagram<_> = diagram!(Program, Expr);
    diagram.add_element(rr::svg::Element::new("style").set("type", "text/css").text(rr::DEFAULT_CSS));
    println!("{diagram}");
}
