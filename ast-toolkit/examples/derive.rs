//  DERIVE.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 18:39:51
//  Last edited:
//    17 Jul 2023, 19:13:48
//  Auto updated?
//    Yes
// 
//  Description:
//!   Test file for us to do stuff in.
// 

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit::Diagnostic;

type Span = ast_toolkit::Span<&'static str, String>;


/***** ERRORS *****/
/// Defines an error... that is also a diagnostic!
#[derive(Debug, Diagnostic)]
#[diagnostic(generics = "<&'static str, String>")]
pub enum TestError {
    #[diag(error)]
    SingleLine { span: Span },
    #[diag(error)]
    SingleInMultiLine { span: Span },
    #[diag(error)]
    MultiLine { span: Span },

    #[diag(error, code = "E001")]
    CommonError { span: Span },
    #[diag(error, code = code)]
    CommonErrorRuntime { code: String, span: Span },
    #[diag(warn, remark = "`#[warn(last_char)]` is enabled by default")]
    SpecificWarn { span: Span },
    #[diag(warn, remark = remark)]
    SpecificWarnRuntime { remark: String, span: Span },

    #[diag(warn)]
    Warn { span: Span },
    #[diag(note)]
    Note { span: Span },

    #[diag(suggestion, suggestion = "text")]
    Suggestion { span: Span },
    #[diag(suggestion)]
    SuggestionRuntime { suggestion: String, span: Span },

    #[diag(error)]
    #[diag(suggestion, message = "Replace with '{suggestion}'")]
    Chained { identifier: String, suggestion: String, span: Span },
}
impl Display for TestError {
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
impl Error for TestError {}





/***** ENTRYPOINT *****/
fn main() {
    Diagnostic::from(TestError::SingleLine{ span: Span::from_idx("<builtin>", "SOURCE TEXT SOURCE TEXT SOURCE TEXT".into(), 0, 10) }).emit();
    Diagnostic::from(TestError::SingleInMultiLine { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 12, 22) }).emit();
    Diagnostic::from(TestError::MultiLine { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 12, 29) }).emit();

    // Codes, notes
    Diagnostic::from(TestError::CommonError { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 0, 0) }).emit();
    Diagnostic::from(TestError::CommonErrorRuntime { code: format!("E00{}", 2), span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 0, 0) }).emit();
    Diagnostic::from(TestError::SpecificWarn { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 34, 34) }).emit();
    Diagnostic::from(TestError::SpecificWarnRuntime { remark: format!("`#[warn({})]` is enabled by default", "runtime_error"), span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 34, 34) }).emit();

    // Warnings, notes
    Diagnostic::from(TestError::Warn { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 12, 17) }).emit();
    Diagnostic::from(TestError::Note { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 19, 22) }).emit();

    // Some suggestion
    Diagnostic::from(TestError::Suggestion { span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 19, 22) }).emit();
    Diagnostic::from(TestError::SuggestionRuntime { suggestion: format!("text {}", 2), span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 19, 22) }).emit();

    // Chain a few
    Diagnostic::from(TestError::Chained { identifier: "SOURCE".into(), suggestion: "source".into(), span: Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT".into(), 0, 5) }).emit();
}
