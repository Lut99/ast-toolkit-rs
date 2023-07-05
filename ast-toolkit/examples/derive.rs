//  DERIVE.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 18:39:51
//  Last edited:
//    05 Jul 2023, 18:44:32
//  Auto updated?
//    Yes
// 
//  Description:
//!   Test file for us to do stuff in.
// 

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};

use ast_toolkit::span::Span;


/***** ERRORS *****/
/// Defines an error... that is also a diagnostic!
#[derive(Debug, Diagnostic)]

pub enum TestError<F, S> {
    Test { value: i32, span: Span<F, S> },
}
impl<F, S> Display for TestError<F, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        
    }
}
impl<F, S> Error for TestError<F, S> {
    
}





/***** ENTRYPOINT *****/
fn main() {
    
}
