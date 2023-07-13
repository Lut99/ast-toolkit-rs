//  ASTS.rs
//    by Lut99
// 
//  Created:
//    13 Jul 2023, 11:51:37
//  Last edited:
//    13 Jul 2023, 12:09:28
//  Auto updated?
//    Yes
// 
//  Description:
//!   Shows examples for how to use the stateful AST.
// 

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};


/***** ASTS *****/
/// The main AST definition
mod ast {
    pub type Span = ast_toolkit::Span<String, String>;


    /// Defines the expression
    pub struct Expression {
        /// The expression specifics
        pub kind      : ExpressionKind,
        /// The evaluated type of this expression.
        pub data_type : DataType,
        /// The span of this expression
        pub span      : Span,
    }

    /// Defines specific expression variants
    pub enum ExpressionKind {
        /// A binary operation.
        BinOp {
            /// The operation to execute
            op  : Operator<Binary>,
            /// The lefthand-side
            lhs : Box<Expression>,
            /// The righthand-side
            rhs : Box<Expression>,
        },

        /// A unary operation.
        UnaOp {
            /// The operation to execute.
            op   : Operator<Unary>,
            /// The expression to execute it on.
            expr : Box<Expression>,
        },

        /// A direct value
        Literal(Literal),
    }



    /// Defines an operator, either unary (if [`Operator<Unary>`]) or binary (if [`Operator<Binary>`]).
    pub struct Operator<K> {
        /// The specific operator
        pub kind      : K,
        /// The evaluated type of this operator.
        pub data_type : DataType,
        /// The span of this operator
        pub span      : Span,
    }

    /// Defines the supported unary operators
    pub enum Unary {
        /// Logical negation
        Not,
        /// Arithmetic negation
        Neg,
    }

    /// Defines the supported unary operators
    pub enum Binary {
        /// Addition
        Add,
        /// Subtraction
        Sub,
        /// Multiplication
        Mul,
        /// Division
        Div,

        /// Conjunction
        And,
        /// Disjunction
        Or,
    }



    /// A possible literal value
    pub struct Literal {
        /// The literal specifics
        pub kind      : LiteralKind,
        /// The evaluated type of this literal.
        pub data_type : DataType?,
        /// The span of this literal.
        pub span      : Span,
    }

    /// Defines the supported literals.
    pub enum LiteralKind {
        /// A literal boolean value
        Boolean(bool),
        /// A literal integer value
        Integer(i64),
    }



    /// Defines the supported data types.
    pub enum DataType {
        /// A boolean
        Boolean,
        /// An integer
        Integer,
    }
}





/***** ERRORS *****/
/// Defines errors that may occur during type analysis.
#[derive(Debug)]
enum TypeError {
    
}
impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        Ok(())
    }
}
impl Error for TypeError {}





/***** TRAVERSALS *****/
/// Deduces the type of the given expression and annotates it with it.
/// 
/// # Arguments
/// - `expr`: The [`Expression`](ast::Expression) to evaluate.
/// 
/// # Errors
/// This function may return an error if there was a type inconsistency.
pub fn type_analysis_expr(expr: &mut ast::Expression) -> Result<(), TypeError> {
    // Match on the type of expression given
    use ast::ExpressionKind::*;
    match &mut expr.kind {
        BinOp { op, lhs, rhs } => {

        },

        UnaOp { op, expr } => {

        },

        Literal(lit) => {
            
        },
    }

    // Done!
    Ok(())
}





/***** ENTRYPOINT *****/
fn main() {
    // First, we analyse a valid integer expression: '42 + (-24 * 2)'
    let ast: ast::Expression = ast::Expression {
        kind : ast::ExpressionKind::BinOp {
            op: ast::Operator {
                kind      : ast::Binary::Add,
                data_type : ast::DataType::
            }, lhs: (), rhs: () }
    };
}
