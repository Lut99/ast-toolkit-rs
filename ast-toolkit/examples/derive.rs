//  DERIVE.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 18:39:51
//  Last edited:
//    22 Jul 2023, 12:09:22
//  Auto updated?
//    Yes
// 
//  Description:
//!   Test file for us to do stuff in.
// 

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit::{Diagnostic, Span};


/***** ERRORS *****/
/// Defines an error... that is also a diagnostic!
#[derive(Debug, Diagnostic)]
pub enum TestError<F, S> {
    #[diag(error)]
    SingleLine { span: Span<F, S> },
    #[diag(error)]
    SingleInMultiLine { span: Span<F, S> },
    #[diag(error)]
    MultiLine { span: Span<F, S> },

    #[diag(error, code = "E001")]
    CommonError { span: Span<F, S> },
    #[diag(error, code = code)]
    CommonErrorRuntime { code: String, span: Span<F, S> },
    #[diag(warn, remark = "`#[warn(last_char)]` is enabled by default")]
    SpecificWarn { span: Span<F, S> },
    #[diag(warn, remark = remark)]
    SpecificWarnRuntime { remark: String, span: Span<F, S> },

    #[diag(warn)]
    Warn { span: Span<F, S> },
    #[diag(note)]
    Note { span: Span<F, S> },

    #[diag(suggestion, replace = "text")]
    Suggestion { span: Span<F, S> },
    #[diag(suggestion)]
    SuggestionRuntime { replace: String, span: Span<F, S> },

    #[diag(error)]
    #[diag(suggestion, message = "Replace with '{replace}'")]
    Chained { identifier: String, replace: String, span: Span<F, S> },
}
impl<F, S> Display for TestError<F, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use TestError::*;
        match self {
            SingleLine { .. }        => write!(f, "A single-line error"),
            SingleInMultiLine { .. } => write!(f, "A single-line in a multi-line error"),
            MultiLine { .. }         => write!(f, "A multi-line error"),

            CommonError { .. }         => write!(f, "A commonly occurring error"),
            CommonErrorRuntime { .. }  => write!(f, "A commonly occurring error but runtime-dependent"),
            SpecificWarn { .. }        => write!(f, "A specific warning"),
            SpecificWarnRuntime { .. } => write!(f, "A specific warning but runtime-dependent"),

            Warn { .. } => write!(f, "A warning"),
            Note { .. } => write!(f, "A note"),

            Suggestion { .. }        => write!(f, "A suggestion to replace 'TEXT' with 'text'"),
            SuggestionRuntime { .. } => write!(f, "A suggestion to replace 'TEXT' with a runtime-dependent value"),

            Chained { identifier, .. } => write!(f, "Invalid identifier '{identifier}'"),
        }
    }
}
impl<F: Debug, S: Debug> Error for TestError<F, S> {}





/***** ENTRYPOINT *****/
fn main() {
    Diagnostic::from(TestError::SingleLine{ span: Span::from_idx("<builtin>", "SOURCE TEXT SOURCE TEXT SOURCE TEXT", 0, 10) }).emit();
    Diagnostic::from(TestError::SingleInMultiLine { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12, 22) }).emit();
    Diagnostic::from(TestError::MultiLine { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12, 29) }).emit();

    // Codes, notes
    Diagnostic::from(TestError::CommonError { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0, 0) }).emit();
    Diagnostic::from(TestError::CommonErrorRuntime { code: format!("E00{}", 2), span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0, 0) }).emit();
    Diagnostic::from(TestError::SpecificWarn { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 34, 34) }).emit();
    Diagnostic::from(TestError::SpecificWarnRuntime { remark: format!("`#[warn({})]` is enabled by default", "runtime_error"), span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 34, 34) }).emit();

    // Warnings, notes
    Diagnostic::from(TestError::Warn { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12, 17) }).emit();
    Diagnostic::from(TestError::Note { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19, 22) }).emit();

    // Some suggestion
    Diagnostic::from(TestError::Suggestion { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19, 22) }).emit();
    Diagnostic::from(TestError::SuggestionRuntime { replace: format!("text {}", 2), span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19, 22) }).emit();

    // Chain a few
    Diagnostic::from(TestError::Chained { identifier: "SOURCE".into(), replace: "source".into(), span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0, 5) }).emit();
}
