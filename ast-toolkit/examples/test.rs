use ast_toolkit::diagnostic::{RustStyle, Span};


fn main() {
    let span = Span::new("<example>", "println!(\"Hello there!\");");
    println!("{}", span.text_snippet().style(RustStyle::error()));
    let span = Span::new("<example>", "fn foo() {\n    println!(\"Hello there!\");\n}");
    println!("{}", span.text_snippet().style(RustStyle::error()));

    let span = Span::empty("<example>", "println!(\"Hello there!\");");
    println!("{}", span.text_snippet().style(RustStyle::error()));
    let span = Span::empty("<example>", "fn foo() {\n    println!(\"Hello there!\");\n}");
    println!("{}", span.text_snippet().style(RustStyle::error()));

    let span = Span::new("<example>", b"println!(\"Hello there!\");");
    println!("{}", span.text_snippet().style(RustStyle::error()));
    let span = Span::new("<example>", b"fn foo() {\n    println!(\"Hello there!\");\n}");
    println!("{}", span.text_snippet().style(RustStyle::error()));
}
