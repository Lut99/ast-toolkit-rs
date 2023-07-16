//  LIB.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 16:40:06
//  Last edited:
//    16 Jul 2023, 11:51:30
//  Auto updated?
//    Yes
// 
//  Description:
//!   A collection of structs and interfaces extremely useful when working
//!   with compilers that parse text into ASTs.
//! 
//!   # Features
//!   This section lists the specific features contributed by this crate.
//! 
//!   ## Diagnostics
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
//! 
//!   ## Spans
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
#[cfg(feature = "derive")]
pub use ast_toolkit_derive::Diagnostic;
