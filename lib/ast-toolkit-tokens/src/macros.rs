//  MACROS.rs
//    by Lut99
//
//  Created:
//    09 Sep 2024, 14:38:51
//  Last edited:
//    13 Mar 2025, 21:24:09
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines macros for defining standard-shape tokens.
//


/***** LIBRARY *****/
/// Generates an implementation for a Token that parses UTF-8 text.
///
/// # Arguments
/// This macro accepts a comma-separated list of:
/// - `$name:ident`: An identifier that is used as the macro name.
/// - `$token:literal`: A string that represents the literal form of the token. E.g., for a dot-token, this would be a `"."`.
/// - (Optional) `$token_desc:literal`: A secondary string that is used in the doc string of the token only to represent what is parsed. If omitted, defaults to `token`.
///
/// # Generates
/// This macro generates a new struct and impls that make working with the Token convenient.
///
/// # Example
/// ```rust
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::{Utf8Token as _, utf8_token};
///
/// // The implementation
/// utf8_token!(Dot, ".");
///
/// // Now this struct exists
/// let dot1 = Dot { span: Span::new("<example1>", ".") };
/// let dot2 = Dot { span: Span::new("<example2>", ".") };
///
/// // And you can do some stuff with it!
/// assert_eq!(
///     format!("{dot1:?}"),
///     "Dot { span: Span<&str, &str> { from: .., source: .., range: Open } }"
/// );
/// assert_eq!(dot1, dot2);
/// assert_eq!(Dot::<(), ()>::TOKEN, ".");
/// ```
#[macro_export]
macro_rules! utf8_token {
    ($name:ident, $token:literal) => {
        ::ast_toolkit_tokens::utf8_token!{ $name, $token, $token }
    };

    ($name:ident, $token:literal, $token_desc:literal) => {
        #[doc = concat!("Represents a '", $token_desc, "' token.\n\n# Generics\n - `F`: The type of the filename (or other description of the source) that is embedded in all [`Span`]s in this AST.\n - `S`: The type of the source text that is embedded in all [`Span`]s in this AST.\n")]
        #[derive(::core::clone::Clone, ::core::marker::Copy, ::std::fmt::Debug)]
        pub struct $name<F, S> {
            /// The span that locates this token in the source text.
            pub span: ::ast_toolkit_span::Span<F, S>,
        }

        // Standard impls
        impl<F, S> ::std::cmp::Eq for $name<F, S> {}
        impl<F, S> ::std::cmp::PartialEq for $name<F, S> {
            /// Compares two tokens of the same type.
            ///
            /// This is always true, because there is nothing configurable about tokens.
            #[inline]
            fn eq(&self, _other: &Self) -> bool { true }

            /// Compares two tokens of the same type by inequality.
            ///
            /// This is never true, because there is nothing configurable about tokens.
            #[inline]
            fn ne(&self, _other: &Self) -> bool { false }
        }

        // Token impls
        impl<F, S> ::ast_toolkit_tokens::Utf8Token<F, S> for $name<F, S> {
            const TOKEN: &'static str = $token;
        }

        // Snack impls (if enabled)
        #[cfg(feature = "snack")]
        impl<F, S> $name<F, S> {
            /// Returns a snack combinator for parsing this token.
            ///
            /// # Arguments
            /// - `comb`: Some kind of parser for recognizing the end of an identifier. This is
            ///   used to ensure that we're not accidentally detecting a keyword within a larger
            ///   identifier.
            ///
            /// # Returns
            /// A new combinator that can parse this keyword.
            ///
            /// # Examples
            /// See the `utf8_token()`-combinator for more information.
            #[inline]
            pub const fn parser<'t, C>(comb: C) -> $crate::snack::complete::utf8_token::Utf8Token<Self, C, F, S>
            where
                C: $crate::snack::Combinator<'t, F, S>,
                F: ::std::clone::Clone,
                S: ::std::clone::Clone + $crate::snack::span::MatchBytes,
            {
                $crate::snack::complete::utf8_token::utf8_token(comb)
            }

            /// Returns a snack combinator for parsing this token.
            ///
            /// This specific function returns a streaming version of the parser. See `parser()` for
            /// a normal version.
            ///
            /// # Arguments
            /// - `comb`: Some kind of parser for recognizing the end of an identifier. This is
            ///   used to ensure that we're not accidentally detecting a keyword within a larger
            ///   identifier.
            ///
            /// # Returns
            /// A new combinator that can parse this keyword.
            ///
            /// # Examples
            /// See the `utf8_token()`-combinator for more information.
            #[inline]
            pub const fn parser_streaming<'t, C>(comb: C) -> $crate::snack::streaming::utf8_token::Utf8Token<Self, C, F, S>
            where
                C: $crate::snack::Combinator<'t, F, S>,
                F: ::std::clone::Clone,
                S: ::std::clone::Clone + $crate::snack::span::MatchBytes,
            {
                $crate::snack::streaming::utf8_token::utf8_token(comb)
            }
        }

        // Railroad impls (if enabled)
        #[cfg(feature = "railroad")]
        impl<F, S> ::ast_toolkit_railroad::ToNode for $name<F, S> {
            type Node = ::ast_toolkit_railroad::railroad::Terminal;

            #[inline]
            fn railroad() -> Self::Node {
                ::ast_toolkit_railroad::railroad::Terminal::new($token_desc.into())
            }
        }

        // Convertion impls
        impl<F, S> ::std::convert::From<::ast_toolkit_span::Span<F, S>> for $name<F, S> {
            #[inline]
            fn from(value: ::ast_toolkit_span::Span<F, S>) -> Self {
                Self { span: value }
            }
        }
    };
}



/// Generates an implementation for a Delimiter that parses UTF-8 text.
///
/// # Arguments
/// This macro accepts a comma-separated list of:
/// - `$name:ident`: An identifier that is used as the macro name.
/// - `$open:literal`: A string that represents the literal form of the opening token. E.g., for parenthesis, this would be a `"("`.
/// - `$close:literal`: A string that represents the literal form of the closing token. E.g., for parenthesis, this would be a `")"`.
///
/// # Generates
/// This macro generates a new struct and impls that make working with the Delimiter convenient.
///
/// # Example
/// ```rust
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::{Utf8Delimiter as _, utf8_delim};
///
/// // The implementation
/// utf8_delim!(Parens, "(", ")");
///
/// // Now this struct exists
/// let span1 = Span::new("<example1>", "(foo)");
/// let span2 = Span::new("<example2>", "(bar)");
/// let paren1 = Parens { open: span1.slice(..1), close: span1.slice(4..) };
/// let paren2 = Parens { open: span2.slice(..1), close: span2.slice(4..) };
///
/// // And you can do some stuff with it!
/// assert_eq!(
///     format!("{paren1:?}"),
///     "Parens { open: Span<&str, &str> { from: .., source: .., range: OpenClosed(1) }, close: \
///      Span<&str, &str> { from: .., source: .., range: ClosedOpen(4) } }"
/// );
/// assert_eq!(paren1, paren2);
/// assert_eq!(Parens::<(), ()>::OPEN_TOKEN, "(");
/// ```
#[macro_export]
macro_rules! utf8_delim {
    ($name:ident, $open:literal, $close:literal) => {
        #[doc = concat!("Represents the delimiting token pair '", $open, $close, "'.\n\n# Generics\n - `F`: The type of the filename (or other description of the source) that is embedded in all [`Span`]s in this AST.\n - `S`: The type of the source text that is embedded in all [`Span`]s in this AST.\n")]
        #[derive(::core::clone::Clone, ::core::marker::Copy, ::std::fmt::Debug)]
        pub struct $name<F, S> {
            #[doc = concat!("The opening delimiter `", $open, "`.\n")]
            pub open:  Span<F, S>,
            #[doc = concat!("The closing delimiter `", $close, "`.\n")]
            pub close: Span<F, S>,
        }

        // Standard impls
        impl<F, S> ::std::cmp::Eq for $name<F, S> {}
        impl<F, S> ::std::cmp::PartialEq for $name<F, S> {
            /// Compares two tokens of the same type.
            ///
            /// This is always true, because there is nothing configurable about tokens.
            #[inline]
            fn eq(&self, _other: &Self) -> bool { true }

            /// Compares two tokens of the same type by inequality.
            ///
            /// This is never true, because there is nothing configurable about tokens.
            #[inline]
            fn ne(&self, _other: &Self) -> bool { false }
        }

        // Token impls
        impl<F, S> ::ast_toolkit_tokens::Utf8Delimiter<F, S> for $name<F, S> {
            const OPEN_TOKEN: &'static str = $open;
            const CLOSE_TOKEN: &'static str = $close;
        }

        // Snack impls (if enabled)
        #[cfg(feature = "snack")]
        impl<F, S> $name<F, S> {
            /// Returns a snack combinator for parsing this token.
            ///
            /// # Arguments
            /// - `comb`: Some kind of parser for parsing the part in between the delimiters.
            ///
            /// # Returns
            /// A new combinator that can parse this keyword.
            ///
            /// # Examples
            /// See the `utf8_delim()`-combinator for more information.
            #[inline]
            pub const fn parser<'t, C>(comb: C) -> $crate::snack::complete::utf8_delim::Utf8Delim<Self, C, F, S>
            where
                C: $crate::snack::Combinator<'t, F, S>,
                F: ::std::clone::Clone,
                S: ::std::clone::Clone + $crate::snack::span::MatchBytes,
            {
                $crate::snack::complete::utf8_delim::utf8_delim(comb)
            }

            /// Returns a snack combinator for parsing this token.
            ///
            /// This specific function returns a streaming version of the parser. See `parser()` for
            /// a normal version.
            ///
            /// # Arguments
            /// - `comb`: Some kind of parser for parsing the part in between the delimiters.
            ///
            /// # Returns
            /// A new combinator that can parse this keyword.
            ///
            /// # Examples
            /// See the `utf8_delim()`-combinator for more information.
            #[inline]
            pub const fn parser_streaming<'t, C>(comb: C) -> $crate::snack::streaming::utf8_delim::Utf8Delim<Self, C, F, S>
            where
                C: $crate::snack::Combinator<'t, F, S>,
                F: ::std::clone::Clone,
                S: ::std::clone::Clone + $crate::snack::span::MatchBytes,
            {
                $crate::snack::streaming::utf8_delim::utf8_delim(comb)
            }
        }

        // Railroad impls (if enabled)
        #[cfg(feature = "railroad")]
        impl<F, S> ::ast_toolkit_railroad::ToNode for $name<F, S> {
            type Node = ::ast_toolkit_railroad::railroad::Terminal;

            #[inline]
            fn railroad() -> Self::Node {
                ::ast_toolkit_railroad::railroad::Terminal::new(concat!($open, $close).into())
            }
        }
        #[cfg(feature = "railroad")]
        impl<F, S> ::ast_toolkit_railroad::ToDelimNode for $name<F, S> {
            type NodeOpen = ::ast_toolkit_railroad::railroad::Terminal;
            type NodeClose = ::ast_toolkit_railroad::railroad::Terminal;

            #[inline]
            fn railroad_open() -> Self::NodeOpen {
                ::ast_toolkit_railroad::railroad::Terminal::new($open.into())
            }
            #[inline]
            fn railroad_close() -> Self::NodeClose {
                ::ast_toolkit_railroad::railroad::Terminal::new($close.into())
            }
        }

        // Convertion impls
        impl<F, S> ::std::convert::From<(::ast_toolkit_span::Span<F, S>, ::ast_toolkit_span::Span<F, S>)> for $name<F, S> {
            #[inline]
            fn from((open, close): (::ast_toolkit_span::Span<F, S>, ::ast_toolkit_span::Span<F, S>)) -> Self {
                Self { open, close }
            }
        }
    };
}
