//  MACROS.rs
//    by Lut99
//
//  Created:
//    09 Sep 2024, 14:38:51
//  Last edited:
//    08 May 2025, 13:25:58
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
/// let dot1 = Dot { span: Span::new(".") };
/// let dot2 = Dot { span: Span::new(".") };
///
/// // And you can do some stuff with it!
/// assert!(format!("{dot1:?}").starts_with("Dot { span: Span<&str> { source: "),);
/// assert!(format!("{dot1:?}").ends_with(", range: .. } }"));
/// assert_eq!(dot1, dot2);
/// assert_eq!(Dot::<()>::TOKEN, ".");
/// ```
#[macro_export]
macro_rules! utf8_token {
    ($name:ident, $token:literal) => {
        $crate::utf8_token!{ $name, $token, $token }
    };

    ($name:ident, $token:literal, $token_desc:literal) => {
        #[doc = concat!("Represents a '", $token_desc, "' token.\n\n# Generics\n - `S`: The type of the source text that is embedded in all [`Span`](", ::std::stringify!($crate::__private::Span), ")s in this AST.\n")]
        pub struct $name<S> {
            /// The span that locates this token in the source text.
            pub span: $crate::__private::Span<S>,
        }

        // Default
        impl ::std::default::Default for $name<&'static str> {
            #[inline]
            fn default() -> Self {
                Self { span: $crate::__private::Span::new(<Self as $crate::Utf8Token<&'static str>>::TOKEN) }
            }
        }

        // Standard impls
        impl<S> ::std::clone::Clone for $name<S>
        where
            S: ::std::clone::Clone,
        {
            #[inline]
            fn clone(&self) -> Self {
                Self { span: self.span.clone() }
            }
        }
        impl<S> ::std::marker::Copy for $name<S>
        where
            S: ::std::marker::Copy,
        {}
        impl<'s, S> ::std::fmt::Debug for $name<S>
        where
            S: $crate::__private::Spannable<'s>,
        {
            #[inline]
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                let mut fmt = f.debug_struct(::std::stringify!($name));
                fmt.field("span", &self.span);
                fmt.finish()
            }
        }
        impl<S> ::std::cmp::Eq for $name<S> {}
        impl<S> ::std::hash::Hash for $name<S> {
            #[inline]
            fn hash<H: ::std::hash::Hasher>(&self, _hasher: &mut H) {}
        }
        impl<S> ::std::cmp::PartialEq for $name<S> {
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
        impl<S> $crate::Utf8Token<S> for $name<S> {
            const TOKEN: &'static str = $token;
        }

        // Spanning impl
        impl<'s, S> $crate::__private::Spanning<S> for $name<S>
        where
            S: ::std::clone::Clone,
        {
            #[inline]
            fn span(&self) -> ::std::borrow::Cow<$crate::__private::Span<S>> { ::std::borrow::Cow::Borrowed(&self.span) }

            #[inline]
            fn into_span(self) -> $crate::__private::Span<S> { self.span }
        }

        // Convertion impls
        impl<S> ::std::convert::From<$crate::__private::Span<S>> for $name<S> {
            #[inline]
            fn from(value: $crate::__private::Span<S>) -> Self {
                Self { span: value }
            }
        }
    };
}

/// Generates an ast-toolkit-snack shortcut for creating this token's parser.
///
/// # Arguments
/// - `$name:ident`: The identifier of the type to implement the parser shortcut for.
///
/// # Generates
/// This macro generates an impl for a type with the given `$name` that implements `parser()` and
/// `parser_streaming()`.
///
/// # Example
/// For an example, see [`utf8_token()`](crate::snack::complete::utf8_token()).
#[macro_export]
#[cfg(feature = "snack")]
macro_rules! utf8_token_snack {
    ($name:ident) => {
        impl<S> $name<S> {
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
            pub const fn parser<'c, 's, C>(comb: C) -> $crate::snack::complete::utf8_token::Utf8Token<Self, C, S>
            where
                C: $crate::snack::__private::Combinator<'c, 's, S>,
                S: ::std::clone::Clone + $crate::snack::__private::SpannableUtf8<'s>,
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
            pub const fn parser_streaming<'c, 's, C>(comb: C) -> $crate::snack::streaming::utf8_token::Utf8Token<Self, C, S>
            where
                C: $crate::snack::__private::Combinator<'c, 's, S>,
                S: ::std::clone::Clone + $crate::snack::__private::SpannableUtf8<'s>,
            {
                $crate::snack::streaming::utf8_token::utf8_token(comb)
            }
        }
    };
}

/// Generates an ast-toolkit-railroad `ToNode` implementation for a Token that parses UTF-8 text.
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
/// let node = Dot::<()>::railroad();
/// ```
#[macro_export]
#[cfg(feature = "railroad")]
macro_rules! utf8_token_railroad {
    ($name:ident, $desc:literal) => {
        // Railroad impl
        impl<S> $crate::__private::railroad::ToNode for $name<S> {
            type Node = $crate::__private::railroad::railroad::Terminal;

            #[inline]
            fn railroad() -> Self::Node { $crate::__private::railroad::railroad::Terminal::new($desc.into()) }
        }
    };
}

/// Generates a serde `Serialize` implementation for a token.
///
/// # Arguments
/// This macro accepts a comma-separated list of:
/// - `$name:ident`: An identifier that is used as the token name to generate the impl for.
///
/// # Generates
/// This macro generates a `Serialize` impl for a type with the given `$name`.
///
/// # Example
/// ```rust
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::{utf8_token, utf8_token_serde};
/// use serde::Serialize as _;
///
/// // The implementation
/// utf8_token!(Dot, ".");
/// utf8_token_serde!(Dot);
///
/// // Now you can do
/// let dot = serde_json::to_string(&Dot { span: Span::new(".") }).unwrap();
/// ```
#[macro_export]
#[cfg(feature = "serde")]
macro_rules! utf8_token_serde {
    ($name:ident) => {
        // Serde impl
        impl<'s, S> $crate::__private::Serialize for $name<S>
        where
            S: $crate::__private::Spannable<'s>,
            S::SourceId: $crate::__private::Serialize,
        {
            #[inline]
            fn serialize<SE>(&self, serializer: SE) -> Result<SE::Ok, SE::Error>
            where
                SE: $crate::__private::Serializer,
            {
                use $crate::__private::SerializeStruct as _;

                let mut ser = serializer.serialize_struct(::std::stringify!($name), 1)?;
                ser.serialize_field("span", &self.span)?;
                ser.end()
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
/// use ast_toolkit_tokens::{Utf8Delimiter, Utf8Token as _, utf8_delim};
///
/// // The implementation
/// utf8_delim!(Parens, "(", ")");
///
/// // Now this struct exists
/// let span1 = Span::new(("<example>", "(foo)"));
/// let span2 = Span::new(("<example>", "(bar)"));
/// let paren1 = Parens {
///     open:  ParensOpen { span: span1.slice(..1) },
///     close: ParensClose { span: span1.slice(4..) },
/// };
/// // Alternatively,
/// let paren2 = Parens::from((span2.slice(..1), span2.slice(4..)));
///
/// // And you can do some stuff with it!
/// assert_eq!(
///     format!("{paren1:?}"),
///     "Parens { open: ParensOpen { span: Span<(&str, &str)> { source: \"<example>\", range: ..1 \
///      } }, close: ParensClose { span: Span<(&str, &str)> { source: \"<example>\", range: 4.. } \
///      } }"
/// );
/// assert_eq!(paren1, paren2);
/// assert_eq!(<Parens<()> as Utf8Delimiter<()>>::OpenToken::TOKEN, "(");
/// ```
#[macro_export]
macro_rules! utf8_delim {
    ($name:ident, $open:literal, $close:literal) => {
        // Generate the two individual tokens first
        ::paste::paste! { $crate::utf8_token!([<$name Open>], $open); }
        ::paste::paste! { $crate::utf8_token!([<$name Close>], $close); }



        // Generates the delimiter
        ::paste::paste! {
            #[doc = concat!("Represents the delimiting token pair '", $open, $close, "'.\n\n# Generics\n - `S`: The type of the source text that is embedded in all [`Span`](", ::std::stringify!($crate::__private::Span), ")s in this AST.\n")]
            pub struct $name<S> {
                #[doc = concat!("The opening delimiter [`", stringify!([<$name Open>]), "`].\n")]
                pub open:  [<$name Open>]<S>,
                #[doc = concat!("The closing delimiter [`", stringify!([<$name Close>]), "`].\n")]
                pub close: [<$name Close>]<S>,
            }
        }

        // Default
        impl ::std::default::Default for $name<&'static str> {
            #[inline]
            fn default() -> Self {
                Self {
                    open: ::std::default::Default::default(),
                    close: ::std::default::Default::default(),
                }
            }
        }

        // Standard impls
        impl<S> ::std::clone::Clone for $name<S>
        where
            S: ::std::clone::Clone,
        {
            #[inline]
            fn clone(&self) -> Self {
                Self { open: self.open.clone(), close: self.close.clone() }
            }
        }
        impl<S> ::std::marker::Copy for $name<S>
        where
            S: ::std::marker::Copy,
        {}
        impl<'s, S> ::std::fmt::Debug for $name<S>
        where
            S: $crate::__private::Spannable<'s>,
        {
            #[inline]
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                let Self { open, close } = self;
                let mut fmt = f.debug_struct(::std::stringify!($name));
                fmt.field("open", open);
                fmt.field("close", close);
                fmt.finish()
            }
        }
        impl<S> ::std::cmp::Eq for $name<S> {}
        impl<S> ::std::hash::Hash for $name<S> {
            #[inline]
            fn hash<H: ::std::hash::Hasher>(&self, _hasher: &mut H) {}
        }
        impl<S> ::std::cmp::PartialEq for $name<S> {
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
        ::paste::paste! {
            impl<S> $crate::Utf8Delimiter<S> for $name<S> {
                type OpenToken = [<$name Open>]<S>;
                type CloseToken = [<$name Close>]<S>;
            }
        }

        // Spanning impl
        impl<'s, S> $crate::__private::Spanning<S> for $name<S>
        where
            S: ::std::clone::Clone + $crate::__private::Spannable<'s>,
        {
            #[inline]
            #[track_caller]
            fn span(&self) -> ::std::borrow::Cow<$crate::__private::Span<S>> { ::std::borrow::Cow::Owned(<<Self as $crate::Utf8Delimiter<S>>::OpenToken as $crate::__private::Spanning<S>>::span(&self.open).join(<<Self as $crate::Utf8Delimiter<S>>::CloseToken as $crate::__private::Spanning<S>>::span(&self.close).as_ref()).unwrap_or_else(|| ::std::panic!("Cannot join spans that point to different files"))) }

            #[inline]
            #[track_caller]
            fn into_span(self) -> $crate::__private::Span<S> { <::std::borrow::Cow<$crate::__private::Span<S>>>::into_owned(<Self as $crate::__private::Spanning<S>>::span(&self)) }
        }

        // Convertion impls
        ::paste::paste! {
            impl<S> ::std::convert::From<($crate::__private::Span<S>, $crate::__private::Span<S>)> for $name<S> {
                #[inline]
                fn from((open, close): ($crate::__private::Span<S>, $crate::__private::Span<S>)) -> Self {
                    Self { open: [<$name Open>] { span: open }, close: [<$name Close>] { span: close } }
                }
            }
        }
        ::paste::paste! {
            impl<S> ::std::convert::From<([<$name Open>]<S>, [<$name Close>]<S>)> for $name<S> {
                #[inline]
                fn from((open, close): ([<$name Open>]<S>, [<$name Close>]<S>)) -> Self {
                    Self { open, close }
                }
            }
        }
    };
}

/// Generates an ast-toolkit-snack shortcut for creating this delimiter's parser.
///
/// # Arguments
/// - `$name:ident`: The identifier of the type to implement the parser shortcut for.
///
/// # Generates
/// This macro generates an impl for a type with the given `$name` that implements `parser()` and
/// `parser_streaming()`.
///
/// # Example
/// For an example, see [`utf8_delim()`](crate::snack::complete::utf8_delim()).
#[macro_export]
#[cfg(feature = "snack")]
macro_rules! utf8_delim_snack {
    ($name:ident) => {
        impl<S> $name<S> {
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
            pub const fn parser<'c, 's, C>(comb: C) -> $crate::snack::complete::utf8_delim::Utf8Delim<Self, C, S>
            where
                C: $crate::snack::__private::Combinator<'c, 's, S>,
                S: ::std::clone::Clone + $crate::snack::__private::SpannableUtf8<'s>,
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
            pub const fn parser_streaming<'c, 's, C>(comb: C) -> $crate::snack::streaming::utf8_delim::Utf8Delim<Self, C, S>
            where
                C: $crate::snack::__private::Combinator<'c, 's, S>,
                S: ::std::clone::Clone + $crate::snack::__private::SpannableUtf8<'s>,
            {
                $crate::snack::streaming::utf8_delim::utf8_delim(comb)
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
/// use ast_toolkit_tokens::{utf8_delim, utf8_delim_railroad};
///
/// // The implementation
/// utf8_delim!(Parens, "(", ")");
/// utf8_delim_railroad!(Parens, "(", ")");
///
/// // Now you can call this!
/// let node1 = Parens::<()>::railroad_open();
/// let node2 = Parens::<()>::railroad_close();
/// ```
#[macro_export]
#[cfg(feature = "railroad")]
macro_rules! utf8_delim_railroad {
    ($name:ident, $open:literal, $close:literal) => {
        // Railroad impls
        impl<S> $crate::__private::railroad::ToNode for $name<S> {
            type Node = $crate::__private::railroad::railroad::Terminal;

            #[inline]
            fn railroad() -> Self::Node { $crate::__private::railroad::railroad::Terminal::new(concat!($open, $close).into()) }
        }
        impl<S> $crate::__private::railroad::ToDelimNode for $name<S> {
            type NodeOpen = $crate::__private::railroad::railroad::Terminal;
            type NodeClose = $crate::__private::railroad::railroad::Terminal;

            #[inline]
            fn railroad_open() -> Self::NodeOpen { $crate::__private::railroad::railroad::Terminal::new($open.into()) }
            #[inline]
            fn railroad_close() -> Self::NodeClose { $crate::__private::railroad::railroad::Terminal::new($close.into()) }
        }
    };
}

/// Generates a serde `Serialize` implementation for a delimited token.
///
/// # Arguments
/// This macro accepts a comma-separated list of:
/// - `$name:ident`: An identifier that is used as the token name to generate the impl for.
///
/// # Generates
/// This macro generates a `Serialize` impl for a type with the given `$name`.
///
/// # Example
/// ```rust
/// use ast_toolkit_span::Span;
/// use ast_toolkit_tokens::{utf8_delim, utf8_delim_serde};
/// use serde::Serialize as _;
///
/// // The implementation
/// utf8_delim!(Parens, "(", ")");
/// utf8_delim_serde!(Parens);
///
/// // Now you can do
/// let dot = serde_json::to_string(&Parens::from((Span::new("("), Span::new(")")))).unwrap();
/// ```
#[macro_export]
#[cfg(feature = "serde")]
macro_rules! utf8_delim_serde {
    ($name:ident) => {
        // Also serde impl for its tokens
        ::paste::paste! { $crate::utf8_token_serde!([<$name Open>]); }
        ::paste::paste! { $crate::utf8_token_serde!([<$name Close>]); }



        // Serde impl
        impl<'s, S> $crate::__private::Serialize for $name<S>
        where
            S: $crate::__private::Spannable<'s>,
            S::SourceId: $crate::__private::Serialize,
        {
            #[inline]
            fn serialize<SE>(&self, serializer: SE) -> Result<SE::Ok, SE::Error>
            where
                SE: $crate::__private::Serializer,
            {
                use $crate::__private::SerializeStruct as _;

                let mut ser = serializer.serialize_struct(::std::stringify!($name), 2)?;
                ser.serialize_field("open", &self.open)?;
                ser.serialize_field("close", &self.close)?;
                ser.end()
            }
        }
    };
}
