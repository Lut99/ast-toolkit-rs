//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    02 Jul 2023, 17:13:06
//  Last edited:
//    02 Jul 2023, 17:49:51
//  Auto updated?
//    Yes
// 
//  Description:
//!   Contributes the [`Diagnostic`], which acts like a source-bound note,
//!   warning or error.
//!   
//!   This API is quite inspired by Rust's notion of a [`Diagnostic`](https://rustc-dev-guide.rust-lang.org/diagnostics/diagnostic-structs.html).
// 

use std::io::Write;

use enum_debug::EnumDebug;
use never_say_never::Never;

use crate::span::Span;


/***** AUXILLARY *****/
/// Defines the possible types of [`Diagnostic`].
#[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum DiagnosticKind {
    /// An fatal error.
    Error,
    /// A warning.
    Warning,
    /// A note.
    Note,
    /// A suggestion.
    Suggestion,
}





/***** LIBRARY *****/
/// Represents a message (note, suggestion, warning or error) that contains an error relating to the source text.
/// 
/// # Lifetimes
/// - `s`: The lifetime of the source text which this Diagnostic refers to.
pub struct Diagnostic<'s> {
    /// The type of the diagnostic.s
    kind    : DiagnosticKind,
    /// The message to show when diagnosing.
    message : String,
    /// The [`Span`] detailling the source text to relate to this error.
    span    : Span<'s>,

    /// Any nested diagnostics.
    sub : Vec<Self>,
}

impl<'s> Diagnostic<'s> {
    /// Constructor for the Diagnostic.
    /// 
    /// # Arguments
    /// - `kind`: Determines the kind of this Diagnostic to show.
    /// - `message`: The message to show in this diagnostic.
    /// - `span`: The [`Span`] that relates this [`Diagnostic`] to the source text.
    /// 
    /// # Returns
    /// A new instance of Self.
    #[inline]
    pub fn new(kind: DiagnosticKind, message: impl ToString, span: impl Into<Span<'s>>) -> Self {
        Self {
            kind,
            message : message.to_string(),
            span    : span.into(),

            sub : vec![],
        }
    }

    /// Constructor for a [`Note`](DiagnosticKind::Note) Diagnostic.
    /// 
    /// # Arguments
    /// - `message`: The message to show in this diagnostic.
    /// - `span`: The [`Span`] that relates this [`Diagnostic`] to the source text.
    /// 
    /// # Returns
    /// A new instance of Self.
    #[inline]
    pub fn note(message: impl ToString, span: impl Into<Span<'s>>) -> Self {
        Self {
            kind    : DiagnosticKind::Note,
            message : message.to_string(),
            span    : span.into(),

            sub : vec![],
        }
    }

    /// Constructor for a [`Suggestion`](DiagnosticKind::Suggestion) Diagnostic.
    /// 
    /// # Arguments
    /// - `message`: The message to show in this diagnostic.
    /// - `span`: The [`Span`] that relates this [`Diagnostic`] to the source text.
    /// 
    /// # Returns
    /// A new instance of Self.
    #[inline]
    pub fn suggestion(message: impl ToString, span: impl Into<Span<'s>>) -> Self {
        Self {
            kind    : DiagnosticKind::Suggestion,
            message : message.to_string(),
            span    : span.into(),

            sub : vec![],
        }
    }

    /// Constructor for a [`Warning`](DiagnosticKind::Warning) Diagnostic.
    /// 
    /// # Arguments
    /// - `message`: The message to show in this diagnostic.
    /// - `span`: The [`Span`] that relates this [`Diagnostic`] to the source text.
    /// 
    /// # Returns
    /// A new instance of Self.
    #[inline]
    pub fn warn(message: impl ToString, span: impl Into<Span<'s>>) -> Self {
        Self {
            kind    : DiagnosticKind::Warning,
            message : message.to_string(),
            span    : span.into(),

            sub : vec![],
        }
    }

    /// Constructor for an [`Error`](DiagnosticKind::Error) Diagnostic.
    /// 
    /// # Arguments
    /// - `message`: The message to show in this diagnostic.
    /// - `span`: The [`Span`] that relates this [`Diagnostic`] to the source text.
    /// 
    /// # Returns
    /// A new instance of Self.
    #[inline]
    pub fn error(message: impl ToString, span: impl Into<Span<'s>>) -> Self {
        Self {
            kind    : DiagnosticKind::Error,
            message : message.to_string(),
            span    : span.into(),

            sub : vec![],
        }
    }



    /// Adds the given Diagnostic to be displayed immediately after this Diagnostic.
    /// 
    /// # Arguments
    /// - `diag`: The [`Diagnostic`] to show in the new diagnostic.
    /// 
    /// # Returns
    /// A mutable reference to `self` for chaining.
    #[inline]
    pub fn add(&mut self, diagnostic: impl Into<Diagnostic<'s>>) -> &mut Self {
        // Add it and return ourselves for chaining
        self.sub.push(diagnostic.into());
        self
    }

    /// Adds a new [`Note`](DiagnosticKind::Note) to be displayed immediately after this Diagnostic.
    /// 
    /// # Arguments
    /// - `message`: The message to show in the new diagnostic.
    /// - `span`: The [`Span`] that relates the new [`Diagnostic`] to the source text.
    /// 
    /// # Returns
    /// A mutable reference to `self` for chaining.
    #[inline]
    pub fn add_note(&mut self, message: impl ToString, span: impl Into<Span<'s>>) -> &mut Self {
        // Add it and return ourselves for chaining
        self.sub.push(Self::note(message, span));
        self
    }

    /// Adds a new [`Suggestion`](DiagnosticKind::Suggestion) to be displayed immediately after this Diagnostic.
    /// 
    /// # Arguments
    /// - `message`: The message to show in the new diagnostic.
    /// - `span`: The [`Span`] that relates the new [`Diagnostic`] to the source text.
    /// 
    /// # Returns
    /// A mutable reference to `self` for chaining.
    #[inline]
    pub fn add_suggestion(&mut self, message: impl ToString, span: impl Into<Span<'s>>) -> &mut Self {
        // Add it and return ourselves for chaining
        self.sub.push(Self::suggestion(message, span));
        self
    }

    /// Adds a new [`Warning`](DiagnosticKind::Warning) to be displayed immediately after this Diagnostic.
    /// 
    /// # Arguments
    /// - `message`: The message to show in the new diagnostic.
    /// - `span`: The [`Span`] that relates the new [`Diagnostic`] to the source text.
    /// 
    /// # Returns
    /// A mutable reference to `self` for chaining.
    #[inline]
    pub fn add_warn(&mut self, message: impl ToString, span: impl Into<Span<'s>>) -> &mut Self {
        // Add it and return ourselves for chaining
        self.sub.push(Self::warn(message, span));
        self
    }

    /// Adds a new [`Error`](DiagnosticKind::Error) to be displayed immediately after this Diagnostic.
    /// 
    /// # Arguments
    /// - `message`: The message to show in the new diagnostic.
    /// - `span`: The [`Span`] that relates the new [`Diagnostic`] to the source text.
    /// 
    /// # Returns
    /// A mutable reference to `self` for chaining.
    #[inline]
    pub fn add_error(&mut self, message: impl ToString, span: impl Into<Span<'s>>) -> &mut Self {
        // Add it and return ourselves for chaining
        self.sub.push(Self::error(message, span));
        self
    }



    /// Emits this [`Diagnostic`] to the given writer.
    /// 
    /// # Arguments
    /// - `writer`: The [`Write`]r that we will write the formatted Diagnostic on.
    /// 
    /// # Errors
    /// This function may error if we failed to write to the given writer.
    #[inline]
    pub fn emit_on(&self, writer: impl Write) -> Result<(), std::io::Error> {
        // Fetch the source lines that we'll want to write.
        let lines: Vec<&str> = self.span.lines();

        // Done!
        Ok(())
    }

    /// Emits this [`Diagnostic`] to stderr.
    /// 
    /// Note that this version ignores any errors that occur while writing. If you expect potential errors while doing so, use [`Self::emit_on()`](Diagnostic::emit_on()) instead.
    #[inline]
    pub fn emit(&self) {
        self.emit_on(std::io::stderr());
    }

    /// Emits this [`Diagnostic`] to the given writer, then quits the application (using [`exit(1)`](std::process::exit())).
    /// 
    /// # Arguments
    /// - `writer`: The [`Write`]r that we will write the formatted Diagnostic on.
    /// 
    /// # Returns
    /// Never ([`!`])
    /// 
    /// # Errors
    /// This function may error if we failed to write to the given writer.
    #[inline]
    pub fn abort_on(&self, writer: impl Write) -> Result<Never, std::io::Error> {
        // Emit first...
        self.emit_on(writer)?;
        // ...and then quit
        std::process::exit(1)
    }

    /// Emits this [`Diagnostic`] to stderr, then quits the application (using [`exit(1)`](std::process::exit())).
    /// 
    /// Note that this version ignores any errors that occur while writing. If you expect potential errors while doing so, use [`Self::abort_on()`](Diagnostic::abort_on()) instead.
    #[inline]
    pub fn abort(&self) -> ! {
        self.abort_on(std::io::stderr()).unwrap_or_else(|_| std::process::exit(1))
    }
}
