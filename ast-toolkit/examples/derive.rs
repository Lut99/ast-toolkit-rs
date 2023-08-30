//  DERIVE.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 18:39:51
//  Last edited:
//    30 Aug 2023, 14:05:50
//  Auto updated?
//    Yes
// 
//  Description:
//!   Test file for us to do stuff in.
// 

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use ast_toolkit::{Diagnostic, DiagnosticSpan, Span};


/***** ERRORS *****/
/// Defines an error... that is also a diagnostic!
#[derive(Debug, Diagnostic)]
pub enum TestError {
    #[diag(error)]
    SingleLine { span: DiagnosticSpan },
    #[diag(error)]
    SingleInMultiLine { span: DiagnosticSpan },
    #[diag(error)]
    MultiLine { span: DiagnosticSpan },

    #[diag(error, code = "E001")]
    CommonError { span: DiagnosticSpan },
    #[diag(error, code = code)]
    CommonErrorRuntime { code: String, span: DiagnosticSpan },
    #[diag(warn, remark = "`#[warn(last_char)]` is enabled by default")]
    SpecificWarn { span: DiagnosticSpan },
    #[diag(warn, remark = remark)]
    SpecificWarnRuntime { remark: String, span: DiagnosticSpan },

    #[diag(warn)]
    Warn { span: DiagnosticSpan },
    #[diag(note)]
    Note { span: DiagnosticSpan },

    #[diag(suggestion, replace = "text")]
    Suggestion { span: DiagnosticSpan },
    #[diag(suggestion)]
    SuggestionRuntime { replace: String, span: DiagnosticSpan },

    #[diag(error)]
    #[diag(suggestion, message = "Replace with '{replace}'")]
    Chained { identifier: String, replace: String, span: DiagnosticSpan },
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
    Diagnostic::from(TestError::SingleLine{ span: Span::ranged("<builtin>", "SOURCE TEXT SOURCE TEXT SOURCE TEXT", 0..=10).into() }).emit();
    Diagnostic::from(TestError::SingleInMultiLine { span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12..=22).into() }).emit();
    Diagnostic::from(TestError::MultiLine { span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12..=29).into() }).emit();

    // Codes, notes
    Diagnostic::from(TestError::CommonError { span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0..=0).into() }).emit();
    Diagnostic::from(TestError::CommonErrorRuntime { code: format!("E00{}", 2), span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0..=0).into() }).emit();
    Diagnostic::from(TestError::SpecificWarn { span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 34..=34).into() }).emit();
    Diagnostic::from(TestError::SpecificWarnRuntime { remark: format!("`#[warn({})]` is enabled by default", "runtime_error"), span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 34..=34).into() }).emit();

    // Warnings, notes
    Diagnostic::from(TestError::Warn { span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12..=17).into() }).emit();
    Diagnostic::from(TestError::Note { span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19..=22).into() }).emit();

    // Some suggestion
    Diagnostic::from(TestError::Suggestion { span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19..=22).into() }).emit();
    Diagnostic::from(TestError::SuggestionRuntime { replace: format!("text {}", 2), span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19..=22).into() }).emit();

    // Chain a few
    Diagnostic::from(TestError::Chained { identifier: "SOURCE".into(), replace: "source".into(), span: Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0..=5).into() }).emit();
}
