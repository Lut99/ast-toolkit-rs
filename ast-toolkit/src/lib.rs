//  LIB.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:06
//  Last edited:
//    18 Sep 2023, 16:47:19
//  Auto updated?
//    Yes
// 
//  Description:
//!   A collection of structs and interfaces extremely useful when working
//!   with compilers that parse text into ASTs.
//! 
//!   # Diagnostics
//!   One of the main features this crate contributes is the addition of
//!   [`struct@Diagnostic`]s, which, much like the one in Rust's own proc_macro
//!   ecosystem, allows for the formatting of source-related errors, warnings,
//!   notes and suggestions. For example:
//! 
//!   ```plain
//!   error: Invalid word 'Hlelo'
//!     --> <example>:1:1
//!      |
//!    1 | Hlelo World!
//!      | ^^^^^       
//!      |
//!   ```
//!   ```plain
//!    warning: Second word shouldn't be capitalized
//!    --> <example>:1:7
//!      |
//!    1 | Hlelo World!
//!      |       ^     
//!      |
//!   ```
//!   ```plain
//!    note: The most classical program in the world is given here
//!    --> <example>:1:1
//!      |
//!    1 | Hlelo World!
//!      | ^^^^^^^^^^^^
//!      |
//!   ```
//!   ```plain
//!    suggestion: Consider writing it properly
//!     --> <example>:1:1
//!      |
//!    1 | Hello, world!
//!      | ^^^^^^^^^^^^^
//!      |
//!   ```
//!   (colourized examples can be found [here](https://github.com/Lut99/ast-toolkit-rs))
//!   
//!   The interface of the struct is similar to the [`struct@Diagnostic`](https://docs.rs/proc-macro-error/latest/proc_macro_error/struct.Diagnostic.html),
//!   with minor tweaks to be more convenient. Most notably, though, when the
//!   `derive`-feature is enabled (see [below](#features)), there is a
//!   convenient API for automatically creating [`struct@Diagnostic`]s for types
//!   implementing [`Error`](std::error::Error) such that it is compatible with
//!   the more typical paradigm of defining error enums. You can refer to the
//!   [derive-macro](derive@Diagnostic)-macro itself for more information.
//! 
//!   Like its Rust-counterpart, the [`struct@Diagnostic`] also supports
//!   adding error- or warning codes, notes, or even full extra diagnostics in
//!   a chain. For example:
//!   ```plain
//!   error[E001]: Greeting not found
//!    --> <example>:1:1
//!     |
//!   1 | Hlelo World!
//!     | ^^^^^^^^^^^^
//!     |
//!   ```
//!   ```plain
//!   warning: Missing comma before 'world'
//!    --> <example>:1:6
//!     |
//!   1 | Hlelo World!
//!     |      ^      
//!     = note: Warning `comma` enabled by default
//!   suggestion: Add the missing comma
//!    --> <example>:1:6
//!     |
//!   1 | Hlelo, World!
//!     |      ^^      
//!     |
//!   ```
//! 
//!   You can refer to the [`examples`](../examples)-folder for a few practical examples.
//! 
//!   # Spans
//!   This crate contributes [`Span`]s for mapping AST nodes/tokens to source
//!   text. It is very comparable to [`LocatedSpan`](https://docs.rs/nom_locate/latest/nom_locate/struct.LocatedSpan.html)
//!   in that it slices a given input string according to the parsed source.
//!   Similarly, when enabling the `nom`-feature (see [below](#features)), it
//!   also implements the required traits to be used in `nom`-functions in a
//!   similar fashion.
//! 
//!   The biggest contribution is that this [`Span`] also carries filename
//!   information.
//! 
//!   # Features
//!   The following features can be enabled in this crate:
//!   - `derive`: Auto-imports the [`ast-toolkit-derive`](ast_toolkit_derive) crate. [enabled by default]
//!   - `nom`: Adds [nom](https://docs.rs/nom/latest/nom/)-compatability for [`Span`] and exposes a re-export of it.
// 

// Define the submodules
pub mod diagnostic;
pub mod position;
pub mod span;

#[cfg(feature = "nom")]
pub mod nom;


// Pull the relevant stuff into the global namespace
pub use crate::diagnostic::{Diagnostic, Diagnosticable, DiagnosticKind};
pub use crate::position::Position;
pub use crate::span::{Combining, IntoSpan, Span, Spanning, SpanningExt};

#[cfg(feature = "nom")]
pub use crate::nom::NomError;


// Pull any procedural macros into this namespace
/// This module documents the use of the various procedural macros defined in the [`ast_toolkit_derive`]-crate.
#[cfg(feature = "derive")]
pub mod procedural {
    /// The [`Diagnostic`](ast_toolkit_derive::Diagnostic) derive-macro automatically derives [`From<Diagnostic>`] on the annotated struct or enum.
    /// 
    /// You can create diagnostics for the struct or every enum variant by annotating them with `#[diag(...)]`. Toplevel attributes can be set on the struct or enum itself using `#[diagnostic(...)]`.
    /// 
    /// # Goal
    /// The main use-case of this macro is to provide easy-to-use interoparability with standard Rust enum errors. For example, consider the following error:
    /// ```rust
    /// # #[derive(Debug)] enum DataType {} impl std::fmt::Display for DataType { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { Ok(()) } }
    /// #[derive(Debug)]
    /// enum SourceError {
    ///     InvalidType { identifier: String, got: DataType, expected: DataType },
    ///     UndefinedSymbol { identifier: String, suggestion: String },
    ///     DuplicateSymbol { identifier: String },
    /// }
    /// impl std::fmt::Display for SourceError {
    ///     #[inline]
    ///     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    ///         use SourceError::*;
    ///         match self {
    ///             InvalidType { identifier, got, expected } => write!(f, "Variable '{identifier}' has type {got}, expected type {expected}"),
    ///             UndefinedSymbol { identifier, suggestion } => write!(f, "Undefined variable '{identifier}' (did you mean '{suggestion}'?)"),
    ///             DuplicateSymbol { identifier } => write!(f, "Duplicate variable '{identifier}'"),
    ///         }
    ///     }
    /// }
    /// impl std::error::Error for SourceError {}
    /// ```
    /// By adding spans and minimal annotations, we can turn this error into a real source-referring error:
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// # #[derive(Debug)] enum DataType {} impl std::fmt::Display for DataType { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { Ok(()) } }
    /// #[derive(Debug, Diagnostic)]
    /// enum SourceError<'f, 's> {
    ///     #[diag(error)]
    ///     InvalidType { identifier: String, got: DataType, expected: DataType, span: Span<'f, 's> },
    ///     #[diag(error, message = "Unidentified variable '{identifier}'")]
    ///     #[diag(suggestion, message = "Did you mean '{suggestion}'?", replace = suggestion)]
    ///     UndefinedSymbol { identifier: String, suggestion: String, span: Span<'f, 's> },
    ///     #[diag(error)]
    ///     #[diag(note, message = "Previous declaration here", span = prev)]
    ///     DuplicateSymbol { identifier: String, span: Span<'f, 's>, prev: Span<'f, 's> },
    /// }
    /// impl<'f, 's> std::fmt::Display for SourceError<'f, 's> {
    ///     #[inline]
    ///     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    ///         use SourceError::*;
    ///         match self {
    ///             InvalidType { identifier, got, expected, .. } => write!(f, "Variable '{identifier}' has type {got}, expected type {expected}"),
    ///             UndefinedSymbol { identifier, suggestion, .. } => write!(f, "Undefined variable '{identifier}' (did you mean '{suggestion}'?)"),
    ///             DuplicateSymbol { identifier, .. } => write!(f, "Duplicate variable '{identifier}'"),
    ///         }
    ///     }
    /// }
    /// impl<'f, 's> std::error::Error for SourceError<'f, 's> {}
    /// ```
    /// To use:
    /// ```rust
    /// # use ast_toolkit::{Diagnostic, Span};
    /// # 
    /// # #[derive(Debug)] enum DataType { String, UnsignedInt8 } impl std::fmt::Display for DataType { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { match self { Self::String => write!(f, "string"), Self::UnsignedInt8 => write!(f, "uint8"), } } }
    /// # #[derive(Debug, Diagnostic)]
    /// # enum SourceError<'f, 's>{
    /// #     #[diag(error)]
    /// #     InvalidType { identifier: String, got: DataType, expected: DataType, span: Span<'f, 's> },
    /// #     #[diag(error, message = "Unidentified variable '{identifier}'")]
    /// #     #[diag(suggestion, message = "Did you mean '{suggestion}'?", replace = suggestion)]
    /// #     UndefinedSymbol { identifier: String, suggestion: String, span: Span<'f, 's> },
    /// #     #[diag(error)]
    /// #     #[diag(note, message = "Previous declaration here", span = prev)]
    /// #     DuplicateSymbol { identifier: String, span: Span<'f, 's>, prev: Span<'f, 's> },
    /// # }
    /// # impl<'f, 's> std::fmt::Display for SourceError<'f, 's> {
    /// #     #[inline]
    /// #     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    /// #         use SourceError::*;
    /// #         match self {
    /// #             InvalidType { identifier, got, expected, .. } => write!(f, "Variable '{identifier}' has type {got}, expected type {expected}"),
    /// #             UndefinedSymbol { identifier, suggestion, .. } => write!(f, "Undefined variable '{identifier}' (did you mean '{suggestion}'?)"),
    /// #             DuplicateSymbol { identifier, .. } => write!(f, "Duplicate variable '{identifier}'"),
    /// #         }
    /// #     }
    /// # }
    /// # impl<'f, 's> std::error::Error for SourceError<'f, 's> {}
    /// # 
    /// let span = Span::ranged("<example>", "let test: &str = \"test\"; let test: u8 = test;", 40..=43);
    /// let err = SourceError::InvalidType {
    ///     identifier: "test".into(),
    ///     got: DataType::String,
    ///     expected: DataType::UnsignedInt8,
    ///     span: span.into(),
    /// };
    /// Diagnostic::from(err).emit();
    /// ```
    /// 
    /// # [`Span`](crate::span::Span)s VS [`DiagnosticSpan`](crate::diagnostic::DiagnosticSpan)s
    /// The macro makes use of [`DiagnosticSpan`](crate::diagnostic::DiagnosticSpan)s instead of [`Span`](crate::span::Span)s because they do not implement references. Instead, they copy
    /// part of the source text which makes the errors much more portable. You can easily convert from the latter to the first using `DiagnosticSpan::from()` or `Span::into()`.
    /// 
    /// # Attributes
    /// ## Toplevel
    /// - `#[diagnostic(f = "<LIFETIME>")]`: Specifies the name of the file-lifetime (default `'f`) defined for the [`Diagnostic`](crate::diagnostic::Diagnostic). This is useful if you do not name your error lifetimes `'f` and `'s`, respectively, or if you intend to use `'static`. Note that the preceding apostrophe (`'`) is optional.
    ///   
    ///   For example:
    ///   ```rust
    ///   use ast_toolkit::{Diagnostic, Span};
    /// 
    ///   #[derive(Diagnostic)]
    ///   #[diagnostic(f = "'a")]
    ///   enum Error<'a, 's> {
    ///       /// Some error
    ///       #[diag(error, message = "Oh no, something happened!")]
    ///       Something { span: Span<'a, 's> },
    ///   }
    ///   ```
    /// - `#[diagnostic(s = "<LIFETIME>")]`: Specifies the name of the source-lifetime (default `'s`) defined for the [`Diagnostic`](crate::diagnostic::Diagnostic). This is useful if you do not name your error lifetimes `'f` and `'s`, respectively, or if you intend to use `'static`. Note that the preceding apostrophe (`'`) is optional.
    ///   
    ///   For example:
    ///   ```rust
    ///   use ast_toolkit::{Diagnostic, Span};
    /// 
    ///   #[derive(Diagnostic)]
    ///   #[diagnostic(s = "'static")]
    ///   enum Error<'f> {
    ///       /// Some error
    ///       #[diag(error, message = "Oh no, something happened!")]
    ///       Something { span: Span<'f, 'static> },
    ///   }
    ///   ```
    /// 
    /// ## Variant-level
    /// For every variant (or the struct itself if you are deriving a struct), you can use `#[diag(...)]` to derive a new [`Diagnostic`](crate::diagnostic::Diagnostic) from that variant. Specifically:
    /// - `#[diag(error)]`, `#[diag(warn)]`, `#[diag(note)]`, `#[diag(suggestion)]`: Derive a new error-, warning-, note- or suggestion-diagnostic from this variant.
    ///   How to derive it is guided by specifying or omitting any of the other arguments.
    /// - `#[diag(message = "<MESSAGE>")]` or `#[diag(message = <FIELD>)]`: Specifies the main message to set for this diagnostic. If a string value is given, then that value
    ///   is used literally; although it is used in a format string with access to all the variant's fields, so you can use those inline. If an identifier is given, then
    ///   the value of that field is used, which must implement [`Display`](std::fmt::Display). Finally, if omitted, then `format!(self)` is used to rely on the struct or
    ///   enum's [`Display`](std::fmt::Display) implementation.
    ///   
    ///   For example:
    ///   ```rust
    ///   use std::fmt::{Display, Formatter, Result as FResult};
    ///   use ast_toolkit::{Diagnostic, Span};
    /// 
    ///   #[derive(Debug, Diagnostic)]
    ///   enum Error<'f, 's> {
    ///       /// First case, where we specify a (potentially formatted) string as message:
    ///       #[diag(error, message = "Hello, {world}!")]
    ///       HelloX { world: String, span: Span<'f, 's> },
    ///   
    ///       /// Second case, where we directly use the value of a field:
    ///       #[diag(error, message = hello_world)]
    ///       HelloWorld { hello_world: String, span: Span<'f, 's> },
    /// 
    ///       /// Third case, where we use the [`Display`]-implementation
    ///       #[diag(error)]
    ///       HelloDisplay { span: Span<'f, 's> },
    ///   }
    ///   impl<'f, 's> Display for Error<'f, 's> {
    ///       fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
    ///           write!(f, "Hello, display!")
    ///       }
    ///   }
    ///   ```
    /// - `#[diag(code = "<CODE>")]` or `#[diag(code = <FIELD>)]` or `#[diag(code)]`: Specifies an optional error-code-like identifier for this [`Diagnostic`](crate::diagnostic::Diagnostic). If a
    ///   string value is given, then it is used literally (no formatting); if an identifier is given, then the value of that field is used; or optionally, if no
    ///   value is given at all, then a default field name of `code` is assumed.
    ///   
    ///   For example:
    ///   ```rust
    ///   use std::fmt::{Display, Formatter, Result as FResult};
    ///   use ast_toolkit::{Diagnostic, Span};
    /// 
    ///   #[derive(Debug, Diagnostic)]
    ///   enum Error<'f, 's> {
    ///       /// First case, where we specify a string as code:
    ///       #[diag(error, code = "err1")]
    ///       LiteralCode { span: Span<'f, 's> },
    ///   
    ///       /// Second case, where we use the value of a field:
    ///       #[diag(error, code = warn_code)]
    ///       FieldCode { warn_code: String, span: Span<'f, 's> },
    /// 
    ///       /// Third case, where we use the value of a the default `code`-field:
    ///       #[diag(error, code)]
    ///       AutoCode { code: String, span: Span<'f, 's> },
    /// 
    ///       /// And, of course, the fourth case, which has no code:
    ///       #[diag(error)]
    ///       NoCode { span: Span<'f, 's> },
    ///   }
    ///   // ...
    ///   # impl<'f, 's> Display for Error<'f, 's> {
    ///   #     fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
    ///   #         write!(f, "Some message")
    ///   #     }
    ///   # }
    ///   ```
    /// - `#[diag(remark = "<REMARK>")]` or `#[diag(remark = <FIELD>)]` or `#[diag(remark)]`: Specifies an optional small remark or note at the bottom of this
    ///   [`Diagnostic`](crate::diagnostic::Diagnostic). Note that this is significantly different from specifying a second `#[diag(note)]` derivation, and is subsequently less expressive.
    ///   
    ///   If a string value is given, then it is used literally (no formatting); if an identifier is given, then the value of that field is used; or optionally,
    ///   if no value is given at all, then a default field name of `remark` is assumed.
    ///   
    ///   For example:
    ///   ```rust
    ///   use std::fmt::{Display, Formatter, Result as FResult};
    ///   use ast_toolkit::{Diagnostic, Span};
    /// 
    ///   #[derive(Debug, Diagnostic)]
    ///   enum Error<'f, 's> {
    ///       /// First case, where we specify a string as remark:
    ///       #[diag(error, remark = "Additional information")]
    ///       LiteralRemark { span: Span<'f, 's> },
    ///   
    ///       /// Second case, where we use the value of a field:
    ///       #[diag(error, remark = note)]
    ///       FieldRemark { note: String, span: Span<'f, 's> },
    /// 
    ///       /// Third case, where we use the value of a the default `remark`-field:
    ///       #[diag(error, remark)]
    ///       AutoRemark { remark: String, span: Span<'f, 's> },
    /// 
    ///       /// And, of course, the fourth case, which has no remark:
    ///       #[diag(error)]
    ///       NoRemark { span: Span<'f, 's> },
    ///   }
    ///   // ...
    ///   # impl<'f, 's> Display for Error<'f, 's> {
    ///   #     fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
    ///   #         write!(f, "Some message")
    ///   #     }
    ///   # }
    ///   ```
    /// - `#[diag(replace = "<NEW SOURCE>")]` or `#[diag(replace = <FIELD>)]` or `#[diag(replace)]`: Specifies a replacement for the spanned source text to give as a
    ///   suggestion. *NOTE*: This field is only available for `#[diag(suggestion)]`-diagnostics.
    ///   
    ///   If a string value is given, then it is used literally (with field formatting); if an identifier is given, then the value of that field is used; or optionally,
    ///   if no value is given at all, then a default field name of `replace` is assumed.
    ///   
    ///   For example:
    ///   ```rust
    ///   use std::fmt::{Display, Formatter, Result as FResult};
    ///   use ast_toolkit::{Diagnostic, Span};
    /// 
    ///   #[derive(Debug, Diagnostic)]
    ///   enum Error<'f, 's> {
    ///       /// First case, where we specify a (potentially formatted) string as remark:
    ///       #[diag(suggestion, replace = "let {ident} = \"Hello, there!\"")]
    ///       LiteralReplace { ident: String, span: Span<'f, 's> },
    ///   
    ///       /// Second case, where we use the value of a field:
    ///       #[diag(suggestion, replace = new_code)]
    ///       FieldReplace { new_code: String, span: Span<'f, 's> },
    /// 
    ///       /// Third case, where we use the value of a the default `remark`-field:
    ///       #[diag(suggestion, replace)]
    ///       AutoReplace { replace: String, span: Span<'f, 's> },
    ///       /// This is equivalent to omitting it altogether for `#[diag(suggestion)]`-diagnostics:
    ///       #[diag(suggestion)]
    ///       NoReplace { replace: String, span: Span<'f, 's> },
    ///   }
    ///   // ...
    ///   # impl<'f, 's> Display for Error<'f, 's> {
    ///   #     fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
    ///   #         write!(f, "Some message")
    ///   #     }
    ///   # }
    ///   ```
    /// - `#[diag(span = <FIELD>)]` or `#[diag(span)]`: Specifies the fieldname of the [`DiagnosticSpan`](crate::diagnostic::DiagnosticSpan) that links the diagnostic to the source text. If not value is given, then
    ///   a default fieldname of `span` is used. Omitting the attribute altogether will yield the same result.
    ///   
    ///   For example:
    ///   ```rust
    ///   use std::fmt::{Display, Formatter, Result as FResult};
    ///   use ast_toolkit::{Diagnostic, Span};
    /// 
    ///   #[derive(Debug, Diagnostic)]
    ///   enum Error<'f, 's> {
    ///       /// First case, where we specify the field manually:
    ///       #[diag(error, span = main_span)]
    ///       FieldSpan { main_span: Span<'f, 's> },
    ///   
    ///       /// Second case, where we use the default fieldname:
    ///       #[diag(error, span)]
    ///       AutoSpan { new_code: String, span: Span<'f, 's> },
    ///       /// This is equivalent to omitting it altogether:
    ///       #[diag(error)]
    ///       NoSpan { replace: String, span: Span<'f, 's> },
    ///   }
    ///   // ...
    ///   # impl<'f, 's> Display for Error<'f, 's> {
    ///   #     fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
    ///   #         write!(f, "Some message")
    ///   #     }
    ///   # }
    ///   ```
    /// 
    /// # Chaining
    /// A key feature of the API is that multiple [`Diagnostic`](crate::diagnostic::Diagnostic) can be derived per struct or variant:
    /// ```rust
    /// use std::fmt::{Display, Formatter, Result as FResult};
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// #[derive(Debug, Diagnostic)]
    /// #[diag(error, code = "bad", span = main_span)]
    /// #[diag(note, message = "See here for more information", span = note_span)]
    /// struct Error<'f, 's> {
    ///     main_span : Span<'f, 's>,
    ///     note_span : Span<'f, 's>,
    /// }
    /// impl<'f, 's> Display for Error<'f, 's> {
    ///     fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
    ///         write!(f, "Oh no! A fatal error has occurred!")
    ///     }
    /// }
    /// impl<'f, 's> std::error::Error for Error<'f, 's> {}
    /// ```
    #[allow(non_snake_case)]
    pub mod Diagnostic {}
}
#[cfg(feature = "derive")]
pub use ast_toolkit_derive::Diagnostic;
