//  DOT.rs
//    by Lut99
//
//  Created:
//    28 Nov 2024, 12:47:11
//  Last edited:
//    13 Mar 2025, 22:02:43
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





/***** ENTRYPOINT *****/
fn main() {
    // We can create them as expected
    let dot1 = Dot { span: Span::new("<example>", ".") };
    let dot2 = Dot { span: Span::new("<example>", ".") };

    // Now we can do stuff with it
    assert_eq!(format!("{dot1:?}"), "Dot { span: Span<&str, &str> { from: .., source: .., range: Open } }");
    assert_eq!(dot1, dot2);
    assert_eq!(Dot::<(), ()>::TOKEN, ".");

    // Also parse some string
    #[cfg(feature = "snack")]
    use ast_toolkit_snack::Combinator as _;
    #[cfg(feature = "snack")]
    let dot3 = Dot::parser(ast_toolkit_snack::combinator::nop()).parse(Span::new("<example>", ".")).unwrap().1;
    #[cfg(feature = "snack")]
    assert_eq!(format!("{dot3:?}"), "Dot { span: Span<&str, &str> { from: .., source: .., range: OpenClosed(1) } }");

    // Also generate railroad diagram nodes
    #[cfg(feature = "railroad")]
    let _node = <Dot<&str, &str> as ast_toolkit_railroad::ToNode>::railroad();
}
