//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 11:20:29
//  Last edited:
//    30 Aug 2023, 14:05:10
//  Auto updated?
//    Yes
// 
//  Description:
//!   Shows a few examples of a diagnostic.
// 

use ast_toolkit::span::Span;
use ast_toolkit::diagnostic::Diagnostic;


/***** ENTRYPOINT *****/
fn main() {
    // Create some diagnostic
    Diagnostic::error("A single-line error", Span::ranged("<builtin>", "SOURCE TEXT SOURCE TEXT SOURCE TEXT", 0..=10)).emit();
    Diagnostic::error("A single-line in a multi-line error", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12..=22)).emit();
    Diagnostic::error("A multi-line error", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12..=29)).emit();

    // Codes, notes
    Diagnostic::error("A commonly occurring error", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0..=0))
        .set_code("E001")
        .emit();
    Diagnostic::error("A commonly occurring error but runtime-dependent", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0..=0))
        .set_code(format!("E00{}", 2))
        .emit();
    Diagnostic::warn("A specific warning", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 34..=34))
        .set_remark("`#[warn(last_char)]` is enabled by default")
        .emit();
    Diagnostic::warn("A specific warning but runtime-dependent", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 34..=34))
        .set_remark(format!("`#[warn({})]` is enabled by default", "runtime_error"))
        .emit();

    // Warnings, notes
    Diagnostic::warn("A warning", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12..=17)).emit();
    Diagnostic::note("A note", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19..=22)).emit();

    // Some suggestion
    Diagnostic::suggestion("A suggestion to replace 'TEXT' with 'text'", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19..=22), "text").emit();
    Diagnostic::suggestion("A suggestion to replace 'TEXT' with a runtime-dependent value", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19..=22), format!("text {}", 2)).emit();

    // Chain a few
    Diagnostic::error("Invalid identifier 'SOURCE'", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0..=5))
        .add_suggestion("Replace with 'source'", Span::ranged("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0..=5), "source")
        .emit();
}
