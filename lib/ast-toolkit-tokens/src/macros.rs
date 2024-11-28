//  MACROS.rs
//    by Lut99
//
//  Created:
//    09 Sep 2024, 14:38:51
//  Last edited:
//    28 Nov 2024, 13:19:00
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
/// use ast_toolkit_tokens::{utf8_token, Utf8Token as _};
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
        $crate::utf8_token!{ $name, $token, $token }
    };

    ($name:ident, $token:literal, $token_desc:literal) => {
        #[doc = concat!("Represents a '", $token_desc, "' token.\n\n# Generics\n - `F`: The type of the filename (or other description of the source) that is embedded in all [`Span`]s in this AST.\n - `S`: The type of the source text that is embedded in all [`Span`]s in this AST.\n")]
        #[derive(::core::clone::Clone, ::core::marker::Copy, ::std::fmt::Debug)]
        pub struct $name<F, S> {
            /// The span that locates this token in the source text.
            pub span: $crate::__private::Span<F, S>,
        }

        // Standard impls
        impl<F, S> ::std::cmp::Eq for $name<F, S> {}
        impl<F, S> ::std::hash::Hash for $name<F, S> {
            #[inline]
            fn hash<H: ::std::hash::Hasher>(&self, _hasher: &mut H) {}
        }
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
        impl<F, S> $crate::Utf8Token<F, S> for $name<F, S> {
            const TOKEN: &'static str = $token;
        }

        // Convertion impls
        impl<F, S> ::std::convert::From<$crate::__private::Span<F, S>> for $name<F, S> {
            #[inline]
            fn from(value: $crate::__private::Span<F, S>) -> Self {
                Self { span: value }
            }
        }
    };
}

/// Generates a ast-toolkit-railroad `ToNode` implementation for a Token that parses UTF-8 text.
///
/// # Arguments
/// This macro accepts a comma-separated list of:
/// - `$name:ident`: An identifier that is used as the macro name.
/// - `$desc:literal`: A secondary string that will be written to the railroad diagram to represent
///   this token's input.
///
/// # Generates
/// This macro generates an impl for a type with the given `$name` that implements `ToNode`.
///
/// # Example
/// ```rust
/// use ast_toolkit_railroad::ToNode as _;
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::{utf8_token, utf8_token_railroad};
///
/// // The implementation
/// utf8_token!(Dot, ".");
/// utf8_token_railroad!(Dot, ".");
///
/// // Now you can call this!
/// let node = Dot::<(), ()>::railroad();
/// ```
#[macro_export]
#[cfg(feature = "railroad")]
macro_rules! utf8_token_railroad {
    ($name:ident, $desc:literal) => {
        // Railroad impl
        impl<F, S> $crate::__private::railroad::ToNode for $name<F, S> {
            type Node = $crate::__private::railroad::railroad::Terminal;

            #[inline]
            fn railroad() -> Self::Node { $crate::__private::railroad::railroad::Terminal::new($desc.into()) }
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
/// use ast_toolkit_tokens::{utf8_delimiter, Utf8Delimiter as _};
///
/// // The implementation
/// utf8_delimiter!(Parens, "(", ")");
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
macro_rules! utf8_delimiter {
    ($name:ident, $open:literal, $close:literal) => {
        #[doc = concat!("Represents the delimiting token pair '", $open, $close, "'.\n\n# Generics\n - `F`: The type of the filename (or other description of the source) that is embedded in all [`Span`]s in this AST.\n - `S`: The type of the source text that is embedded in all [`Span`]s in this AST.\n")]
        #[derive(::core::clone::Clone, ::core::marker::Copy, ::std::fmt::Debug)]
        pub struct $name<F, S> {
            #[doc = concat!("The opening delimiter `", $open, "`.\n")]
            pub open:  $crate::__private::Span<F, S>,
            #[doc = concat!("The closing delimiter `", $close, "`.\n")]
            pub close: $crate::__private::Span<F, S>,
        }

        // Standard impls
        impl<F, S> ::std::cmp::Eq for $name<F, S> {}
        impl<F, S> ::std::hash::Hash for $name<F, S> {
            #[inline]
            fn hash<H: ::std::hash::Hasher>(&self, _hasher: &mut H) {}
        }
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
        impl<F, S> $crate::Utf8Delimiter<F, S> for $name<F, S> {
            const OPEN_TOKEN: &'static str = $open;
            const CLOSE_TOKEN: &'static str = $close;
        }

        // Convertion impls
        impl<F, S> ::std::convert::From<($crate::__private::Span<F, S>, $crate::__private::Span<F, S>)> for $name<F, S> {
            #[inline]
            fn from((open, close): ($crate::__private::Span<F, S>, $crate::__private::Span<F, S>)) -> Self {
                Self { open, close }
            }
        }
    };
}

/// Generates a ast-toolkit-railroad `ToDelimNode` implementation for a delimited token that parses
/// UTF-8 text.
///
/// # Arguments
/// This macro accepts a comma-separated list of:
/// - `$name:ident`: An identifier that is used as the macro name.
/// - `$open:literal`: A string that will be written to the railroad diagram to represent
///   the opening token's input.
/// - `$close:literal`: A string that will be written to the railroad diagram to represent
///   the closing token's input.
///
/// # Generates
/// This macro generates an impl for a type with the given `$name` that implements `ToNode`.
///
/// # Example
/// ```rust
/// use ast_toolkit_railroad::ToDelimNode as _;
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::{utf8_delimiter, utf8_delimiter_railroad};
///
/// // The implementation
/// utf8_delimiter!(Parens, "(", ")");
/// utf8_delimiter_railroad!(Parens, "(", ")");
///
/// // Now you can call this!
/// let node1 = Parens::<(), ()>::railroad_open();
/// let node2 = Parens::<(), ()>::railroad_close();
/// ```
#[macro_export]
#[cfg(feature = "railroad")]
macro_rules! utf8_delimiter_railroad {
    ($name:ident, $open:literal, $close:literal) => {
        // Railroad impls
        impl<F, S> $crate::__private::railroad::ToNode for $name<F, S> {
            type Node = $crate::__private::railroad::railroad::Terminal;

            #[inline]
            fn railroad() -> Self::Node { $crate::__private::railroad::railroad::Terminal::new(concat!($open, $close).into()) }
        }
        impl<F, S> $crate::__private::railroad::ToDelimNode for $name<F, S> {
            type NodeOpen = $crate::__private::railroad::railroad::Terminal;
            type NodeClose = $crate::__private::railroad::railroad::Terminal;

            #[inline]
            fn railroad_open() -> Self::NodeOpen { $crate::__private::railroad::railroad::Terminal::new($open.into()) }
            #[inline]
            fn railroad_close() -> Self::NodeClose { $crate::__private::railroad::railroad::Terminal::new($close.into()) }
        }
    };
}
