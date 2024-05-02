//  PROCEDURAL.rs
//    by Lut99
//
//  Created:
//    02 May 2024, 18:25:42
//  Last edited:
//    02 May 2024, 18:33:57
//  Auto updated?
//    Yes
//
//  Description:
//!   Documentation for snack's procedural macros.
//


/***** LIBRARY *****/
/// Allows for easy, closure-based creation of [`Combinator`](crate::Combinator)s.
///
/// # Syntax
/// The comb-macro expects two blocks of input, started by a keyword (`expects` and `combinator`) and ended by semicolons (`;`).
///
/// ## Expects-block
/// The first block, started by `expects`, builds a string that is displayed when the combinator fails. This string should come after "Expected ..." to indicate what the closure expected as input.
///
/// It has three forms:
/// - Literal form, which encodes a single string literal, possibly with format-args;
/// - Tuple form, which encodes a string literal together with externalized format args; and
/// - Closure form, which provides a snippet of code to build a string manually.
///
/// ### Literal form
/// ```ignore
/// comb! {
///     expects "hello, world!";
/// }
/// ```
///
/// This form defines a single, straightforward format string that only uses variables in scope (by virtue of in-string `{format}`-args). At the end, it is generated to something of the form:
/// ```ignore
/// format!(<LITERAL>)
/// ```
/// meaning that it works as expected.
///
/// ### Tuple form
/// ```ignore
/// comb! {
///     expects ("hello, {}!", "world");
/// }
/// ```
///
/// This form defines a format literal together with its arguments. Here, too, anything in scope is available to use.
///
/// At the end, it is generated to something expected, like:
/// ```ignore
/// format!(<LITERAL>, <ARGS...>)
/// ```
pub mod comb {}
