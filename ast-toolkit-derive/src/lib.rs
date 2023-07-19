//  LIB.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 15:47:40
//  Last edited:
//    19 Jul 2023, 15:23:44
//  Auto updated?
//    Yes
// 
//  Description:
//!   Crate that implements various `#[derive(...)]` macros for the
//!   `ast-toolkit` crate.
// 

// Declare submodules
mod diagnostic;



/***** LIBRARY *****/
/// Derives [`From<Diagnostic>`] automatically on the annotated struct or enum.
/// 
/// You can create diagnostics for the struct or every enum variant by annotating them with `#[diag(...)]`. Toplevel attributes can be set on the struct or enum itself using `#[diagnostic(...)]`.
/// 
/// # Goal
/// The main use-case of this macro is to provide easy-to-use interoparability with standard Rust enum errors. For example, consider the following error:
/// ```rust
/// # enum DataType {}
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
/// # enum DataType {}
/// #[derive(Diagnostic)]
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
/// To use:
/// ```rust
/// # use ast_toolkit::{Diagnostic, Span};
/// # 
/// # enum DataType { String, UnsignedInt8 } impl 
/// # #[derive(Diagnostic)]
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
/// # impl std::fmt::Display for SourceError {
/// #     #[inline]
/// #     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
/// #         use SourceError::*;
/// #         match self {
/// #             InvalidType { identifier, got, expected } => write!(f, "Variable '{identifier}' has type {got}, expected type {expected}"),
/// #             UndefinedSymbol { identifier, suggestion } => write!(f, "Undefined variable '{identifier}' (did you mean '{suggestion}'?)"),
/// #             DuplicateSymbol { identifier } => write!(f, "Duplicate variable '{identifier}'"),
/// #         }
/// #     }
/// # }
/// # impl std::error::Error for SourceError {}
/// # 
/// let span: Span<&str, &str> = Span::from_idx("<example>", "let test: &str = \"test\"; let test: u8 = test;", 40, 43);
/// let err: SourceError<&str, &str> = SourceError::InvalidType { identifier: "test".into(), got: DataType::String, expected: DataType::UnsignedInt8 };
/// Diagnostic::from(err).emit();
/// ```
#[proc_macro_error::proc_macro_error]
#[proc_macro_derive(Diagnostic, attributes(diagnostic, diag))]
pub fn derive_directory(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use syn::{parse_macro_input, DeriveInput};


    // Parse the thing we've gotten
    let DeriveInput{ ident, data, attrs, generics, vis } = parse_macro_input!(input);

    // Pass to the main function
    match diagnostic::derive(ident, data, attrs, generics, vis) {
        Ok(stream) => stream,
        Err(err)   => { err.abort(); },
    }
}
