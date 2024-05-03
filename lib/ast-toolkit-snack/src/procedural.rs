//  PROCEDURAL.rs
//    by Lut99
//
//  Created:
//    02 May 2024, 18:25:42
//  Last edited:
//    03 May 2024, 13:25:37
//  Auto updated?
//    Yes
//
//  Description:
//!   Documentation for snack's procedural macros.
//


/***** LIBRARY *****/
/// Allows for easy, closure-based creation of [`Combinator`](crate::Combinator)s.
///
/// The comb-macro expects two blocks of input, started by a keyword (`expects` and `combinator`) and ended by semicolons (`;`).
///
/// # Expects-block
/// The first block discussed (but not necessarily given), started by `expects`, builds a string that is displayed when the combinator fails. This string should come after "Expected ..." to indicate what the closure expected as input.
///
/// It has three forms:
/// - Literal form, which encodes a single string literal, possibly with format-args;
/// - Tuple form, which encodes a string literal together with externalized format args; and
/// - Closure form, which provides a snippet of code to build a string manually.
///
/// ## Literal form
/// ```ignore
/// comb! {
///     expects "hello, world!";
/// }
/// ```
///
/// This form defines a single, straightforward string literal that describes what to expect. At the end, it is generated to something of the form:
/// ```ignore
/// format!(<LITERAL>)
/// ```
/// which means that you can use the literal as if it's a format string without externalized variables. Any variable in scope can be referred to in the format string.
///
/// ## Tuple form
/// ```ignore
/// comb! {
///     expects ("hello, {}!", "world");
/// }
/// ```
///
/// This form defines a format literal together with its arguments. It is literally passed to the `format!()`-macro, and so can access anything in scope as arguments.
///
/// ## Closure form
/// ```ignore
/// comb! {
///     expects {
///         format!("hello, {}!", world);
///     };
/// }
/// ```
/// ```ignore
/// comb! {
///     expects move {
///         format!("hello, {}!", world);
///     };
/// }
/// ```
///
/// The closure form defines a closure that produces a string at runtime. Anything in scope can be imported into the closure, which can optionally `move` them by applying that keyword.
///
/// The given block is generated to a closure of the form:
/// ```ignore
/// || -> String {
///     format!("hello, {}!", world);
/// }
/// ```
/// with the `move` added as necessary.
///
///
/// # Combinator-block
/// The second block we discuss is started by the `combinator`-keyword, and implements a closure that implements the combinator.
///
/// In particular, the given code is compiled to a closure of the form:
/// ```ignore
/// |input: ::ast_toolkit_span::Span<F, S>| -> ::ast_toolkit_snack::Result<'static, R, F, S> {
///     
/// }
/// ```
/// where `F` and `S` must be generics defined by the parent function and `R` is the desired output of the combinator.
///
/// Note that this means that the variable `input` has special meaning in your combinator, which is the input text to parse.
///
/// The body of the closure is given as a block, i.e.,:
/// ```ignore
/// comb! {
///     combinator {
///         ast_toolkit_snack::utf8::complete::tag("Hello, world!").parse(input)
///     };
/// }
/// ```
///
/// Optionally, you can change the `'static` lifetime in the output of the closure. This is desirable if any failures or errors produced by your combinator somehow depend on something, e.g.,
/// ```ignore
/// fn tag_like_closure<'t, F, S>(tag: &'t str) -> impl ast_toolkit_snack::Combinator<'t, F, S, Output = Span<F, S>> {
///     comb! {
///         combinator -> 't {
///             ast_toolkit_snack::utf8::complete::tag(tag).parse(input)
///         };
///     }
/// }
/// ```
///
/// Similarly, you can also specify a specific type for `R`:
/// ```ignore
/// fn returns_true<F, S>() -> impl ast_toolkit_snack::Combinator<'static, F, S, Output = bool> {
///     comb! {
///         combinator -> bool {
///             ast_toolkit_snack::Result::Ok(input, true)
///         };
///     }
/// }
/// ```
/// or specify them both, where the lifetime is always first, separated by comma (e.g., `combinator -> 'a, bool { ... };`).
///
///
/// # Examples
/// For example, on could implement a combinator that parses `Hello, world!`:
/// ```
/// use ast_toolkit_snack::span::MatchBytes;
/// use ast_toolkit_snack::utf8::complete as utf8;
/// use ast_toolkit_snack::{comb, Combinator};
/// use ast_toolkit_span::Span;
///
/// fn hello_world<F: Clone, S: Clone + MatchBytes>()
/// -> impl Combinator<'static, F, S, Output = Span<F, S>> {
///     comb! {
///         expects "hello world";
///
///         combinator {
///             utf8::tag("Hello, world!").parse(input)
///         };
///     }
/// }
/// ```
pub mod comb {}
