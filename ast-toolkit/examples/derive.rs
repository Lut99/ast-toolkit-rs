//  DERIVE.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 18:39:51
//  Last edited:
//    06 Jul 2023, 09:29:35
//  Auto updated?
//    Yes
// 
//  Description:
//!   Test file for us to do stuff in.
// 

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit::span::Span;
use ast_toolkit_derive::Diagnostic;


/***** ERRORS *****/
/// Defines an error... that is also a diagnostic!
#[derive(Debug, Diagnostic)]

pub enum TestError<F, S> {
    #[diag(error, code = "E0001")]
    Test { value: i32, span: Span<F, S> },
    #[diag(error, code = "E0002")]
    #[diag(note, message = "This has everything to do with something important over here!", span = "note")]
    TestNote { value: i32, span: Span<F, S>, note: Span<F, S> },
}
impl<F, S> Display for TestError<F, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use TestError::*;
        match self {
            Test { value, .. }     => write!(f, "An error has occurred relating to {value}"),
            TestNote { value, .. } => write!(f, "An error has occurred relating to {value} - again"),
        }
    }
}
impl<F: Debug, S: Debug> Error for TestError<F, S> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        use TestError::*;
        match self {
            Test { .. }     => None,
            TestNote { .. } => None,
        }
    }
}





/***** ENTRYPOINT *****/
fn main() {
    
}
