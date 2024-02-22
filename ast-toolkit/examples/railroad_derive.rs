//  RAILROAD DERIVE.rs
//    by Lut99
//
//  Created:
//    22 Feb 2024, 14:26:07
//  Last edited:
//    22 Feb 2024, 14:39:38
//  Auto updated?
//    Yes
//
//  Description:
//!   Showcases generating railroad diagrams based on ASTs using the
//!   `railroad`-feature with macros provided by the `derive`-feature.
//

#[cfg(not(all(feature = "railroad", feature = "derive")))]
compile_error!("Please enable the 'railroad' and 'derive' features when running the 'railroad_derive' example.");

use ast_toolkit::railroad::{self as rr, ToNode};


/***** EXAMPLE AST *****/
/// Root node for a simple expression language.
#[derive(ToNode)]
enum Expr {
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

/// Represents a binary operation in the expression language.
#[derive(ToNode)]
struct ExprBinOp<O: ToNode> {
    /// The lefthand-side of the operation.
    lhs: Box<Expr>,
    /// The operation to apply.
    op:  O,
    /// The righthand-side of the operation.
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





/***** LIBRARY *****/
fn main() {
    todo!();
}
