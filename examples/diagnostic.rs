//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 11:20:29
//  Last edited:
//    05 Jul 2023, 12:07:57
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
    Diagnostic::error("A single-line error", Span::from_idx("<builtin>", "SOURCE TEXT SOURCE TEXT SOURCE TEXT", 0, 10)).emit();
    Diagnostic::error("A single-line in a multi-line error", Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12, 22)).emit();
    Diagnostic::error("A multi-line error", Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12, 29)).emit();

    // Codes, notes
    Diagnostic::error("A commonly occurring error", Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0, 0))
        .set_code("E001")
        .emit();
    Diagnostic::warn("A specific warning", Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 34, 34))
        .set_note("`#[warn(last_char)]` is enabled by default")
        .emit();

    // Warnings, notes
    Diagnostic::warn("A warning", Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 12, 17)).emit();
    Diagnostic::note("A note", Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19, 22)).emit();

    // Some suggestion
    Diagnostic::suggestion("A suggestion to replace 'TEXT' with 'text'", Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 19, 22), "text").emit();

    // Chain a few
    Diagnostic::error("Invalid identifier 'SOURCE'", Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0, 5))
        .add_suggestion("Replace with 'source'", Span::from_idx("<builtin>", "SOURCE TEXT\nSOURCE TEXT\nSOURCE TEXT", 0, 5), "source")
        .emit();
}
