//  DOT.rs
//    by Lut99
//
//  Created:
//    28 Nov 2024, 12:47:11
//  Last edited:
//    24 Mar 2025, 12:28:46
//  Auto updated?
//    Yes
//
//  Description:
//!   Showcases the macros of the `ast-toolkit-tokens` crate.
//

use ast_toolkit_span::Span;
use ast_toolkit_tokens::{Utf8Token as _, utf8_token};


/***** AST *****/
utf8_token!(Dot, ".");
#[cfg(feature = "snack")]
ast_toolkit_tokens::utf8_token_snack!(Dot);
#[cfg(feature = "railroad")]
ast_toolkit_tokens::utf8_token_railroad!(Dot, ".");
#[cfg(feature = "serde")]
ast_toolkit_tokens::utf8_token_serde!(Dot);





/***** ENTRYPOINT *****/
fn main() {
    // We can create them as expected
    let dot1 = Dot { span: Span::new(("<example1>", ".")) };
    let dot2 = Dot { span: Span::new(("<example2>", ".")) };

    // Now we can do stuff with it
    assert_eq!(format!("{dot1:?}"), "Dot { span: Span<(&str, &str)> { source: \"<example1>\", range: .. } }");
    assert_eq!(dot1, dot2);
    assert_eq!(Dot::<()>::TOKEN, ".");

    // Also parse some string
    #[cfg(feature = "snack")]
    use ast_toolkit_snack::Combinator as _;
    #[cfg(feature = "snack")]
    let dot3 = Dot::parser(ast_toolkit_snack::combinator::nop()).parse(Span::new(("<example3>", "."))).unwrap().1;
    #[cfg(feature = "snack")]
    assert_eq!(format!("{dot3:?}"), "Dot { span: Span<(&str, &str)> { source: \"<example3>\", range: ..1 } }");

    // Also generate railroad diagram nodes
    #[cfg(feature = "railroad")]
    let _node = <Dot<&str> as ast_toolkit_railroad::ToNode>::railroad();

    // Also serialize it
    #[cfg(feature = "serde")]
    assert_eq!(serde_json::to_string(&dot1).unwrap(), "{\"span\":{\"source\":\"<example1>\",\"range\":{\"inner\":\"Full\"}}}");
}
