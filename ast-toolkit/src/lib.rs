//  LIB.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:06
//  Last edited:
//    20 Jul 2023, 19:49:20
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
//!   The interface of the struct is similar to the [`Diagnostic`](https://docs.rs/proc-macro-error/latest/proc_macro_error/struct.Diagnostic.html),
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
pub mod span;
pub mod diagnostic;


// Pull the relevant stuff into the global namespace
pub use span::{Position, Span};
pub use diagnostic::{Diagnostic, DiagnosticKind};

// Pull any macros into this namespace
/// Derives [`From<Diagnostic>`] automatically on the annotated struct or enum.
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
/// enum SourceError<F, S> {
///     #[diag(error)]
///     InvalidType { identifier: String, got: DataType, expected: DataType, span: Span<F, S> },
///     #[diag(error, message = "Unidentified variable '{identifier}'")]
///     #[diag(suggestion, message = "Did you mean '{suggestion}'?")]
///     UndefinedSymbol { identifier: String, suggestion: String, span: Span<F, S> },
///     #[diag(error)]
///     #[diag(note, message = "Previous declaration here", span = prev)]
///     DuplicateSymbol { identifier: String, span: Span<F, S>, prev: Span<F, S> },
/// }
/// impl<F, S> std::fmt::Display for SourceError<F, S> {
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
/// impl<F: std::fmt::Debug, S: std::fmt::Debug> std::error::Error for SourceError<F, S> {}
/// ```
/// To use:
/// ```rust
/// # use ast_toolkit::{Diagnostic, Span};
/// # 
/// # #[derive(Debug)] enum DataType { String, UnsignedInt8 } impl std::fmt::Display for DataType { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { match self { Self::String => write!(f, "string"), Self::UnsignedInt8 => write!(f, "uint8"), } } }
/// # #[derive(Debug, Diagnostic)]
/// # enum SourceError<F, S> {
/// #     #[diag(error)]
/// #     InvalidType { identifier: String, got: DataType, expected: DataType, span: Span<F, S> },
/// #     #[diag(error, message = "Unidentified variable '{identifier}'")]
/// #     #[diag(suggestion, message = "Did you mean '{suggestion}'?")]
/// #     UndefinedSymbol { identifier: String, suggestion: String, span: Span<F, S> },
/// #     #[diag(error)]
/// #     #[diag(note, message = "Previous declaration here", span = prev)]
/// #     DuplicateSymbol { identifier: String, span: Span<F, S>, prev: Span<F, S> },
/// # }
/// # impl<F, S> std::fmt::Display for SourceError<F, S> {
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
/// # impl<F: std::fmt::Debug, S: std::fmt::Debug> std::error::Error for SourceError<F, S> {}
/// # 
/// let span: Span<&str, &str> = Span::from_idx("<example>", "let test: &str = \"test\"; let test: u8 = test;", 40, 43);
/// let err: SourceError<&str, &str> = SourceError::InvalidType {
///     identifier: "test".into(),
///     got: DataType::String,
///     expected: DataType::UnsignedInt8,
///     span,
/// };
/// Diagnostic::from(err).emit();
/// ```
#[cfg(feature = "derive")]
pub use ast_toolkit_derive::Diagnostic;
