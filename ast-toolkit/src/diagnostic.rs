//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    04 Jul 2023, 19:17:50
//  Last edited:
//    17 Jul 2023, 19:54:16
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Diagnostic`] object, which concerns itself with
//!   prettily formatting an error.
// 

use std::io::Write;
use std::ops::Deref;

use console::{style, Style};
use enum_debug::EnumDebug;
use never_say_never::Never;
use unicode_segmentation::UnicodeSegmentation as _;

use crate::span::{Position, Span};


/***** HELPERS *****/
/// Defines the specific attributes for certain kinds of diagnostic.
#[derive(Clone, Debug, EnumDebug)]
enum DiagnosticSpecific {
    /// A typical error
    Error,
    /// A typical warning
    Warning,
    /// A typical note
    Note,
    /// A suggestion.
    Suggestion {
        /// Provides a replacement for the [`Span`]ned area.
        suggestion : String,
    },
}





/***** AUXILLARY *****/
/// Defines the possible types of [`Diagnostic`].
/// 
/// Despite the existance of this struct, there is no way to generally construct a [`Diagnostic`] with variable kind because of the varying input for every kind.
/// 
/// Instead, this can be used to, for example, check which [`Diagnostic`] has been emitted by a function.
/// 
/// # Example
/// ```rust
/// use ast_toolkit::{Diagnostic, DiagnosticKind, Span};
/// 
/// fn foo(raw: &str) -> Diagnostic<&str, &str> {
///     Diagnostic::error("Invalid input", Span::new("<example>", raw))
/// }
/// 
/// let diag = foo("Hello, world!");
/// if diag.kind() == DiagnosticKind::Error {
///     diag.emit();
///     eprintln!("Could not run foo() due to the previous error.");
/// };
/// ```
#[derive(Clone, Copy, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum DiagnosticKind {
    /// An fatal error.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, DiagnosticKind, Span};
    /// 
    /// assert_eq!(Diagnostic::error("Example", Span::new("<example>", "example")).kind(), DiagnosticKind::Error);
    /// ```
    Error,
    /// A warning.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, DiagnosticKind, Span};
    /// 
    /// assert_eq!(Diagnostic::warn("Example", Span::new("<example>", "example")).kind(), DiagnosticKind::Warning);
    Warning,
    /// A note.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, DiagnosticKind, Span};
    /// 
    /// assert_eq!(Diagnostic::note("Example", Span::new("<example>", "example")).kind(), DiagnosticKind::Note);
    Note,
    /// A suggestion.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, DiagnosticKind, Span};
    /// 
    /// assert_eq!(Diagnostic::suggestion("Example", Span::new("<example>", "example"), "better example").kind(), DiagnosticKind::Suggestion);
    Suggestion,
}





/***** LIBRARY *****/
/// Represents a (series of) source-bound errors that can be neatly formatted.
/// 
/// # Generic arguments
/// - `F`: Decides the type of the filename string embedded in [`Span`]s compatible with this diagnostic.
/// - `S`: Decides the type of the source string embedded in [`Span`]s compatible with this diagnostic.
/// 
/// # Example
/// To create a new Diagnostic, use [`Diagnostic::error()`], [`Diagnostic::warn()`], [`Diagnostic::note()`] or [`Diagnostic::suggestion()`]:
/// ```rust
/// use ast_toolkit::{Diagnostic, Span};
/// 
/// let err = Diagnostic::error("Invalid word 'Hlelo'", Span::from_idx("<example>", "Hlelo World!", 0, 4));
/// let warn = Diagnostic::warn("Second word shouldn't be capitalized", Span::from_idx("<example>", "Hlelo World!", 6, 6));
/// let note = Diagnostic::note("The most classical program in the world is given here", Span::new("<example>", "Hlelo World!"));
/// let suggest = Diagnostic::suggestion("Consider writing it properly", Span::new("<example>", "Hlelo World!"), "Hello, world!");
/// ```
/// To print the diagnostics, check [`Diagnostic::emit()`]:
/// ```rust
/// # use ast_toolkit::{Diagnostic, Span};
/// # 
/// # let err = Diagnostic::error("Invalid word 'Hlelo'", Span::from_idx("<example>", "Hlelo World!", 0, 4));
/// # let warn = Diagnostic::warn("Second word shouldn't be capitalized", Span::from_idx("<example>", "Hlelo World!", 6, 6));
/// # let note = Diagnostic::note("The most classical program in the world is given here", Span::new("<example>", "Hlelo World!"));
/// # let suggest = Diagnostic::suggestion("Consider writing it properly", Span::new("<example>", "Hlelo World!"), "Hello, world!");
/// # 
/// err.emit();
/// warn.emit();
/// note.emit();
/// suggest.emit();
/// ```
/// 
/// See this struct's other methods for detailled configuration options.
#[derive(Clone, Debug)]
pub struct Diagnostic<F, S> {
    /// The message to show
    message : String,
    /// Some code identifier for distinguishing errors machine-wise.
    code    : Option<String>,
    /// The in-diagnostic note to display, if any.
    remark  : Option<String>,
    /// The span that relates this message to the source text.
    span    : Span<F, S>,
    /// Anything kind-specific.
    kind    : DiagnosticSpecific,
    /// Any other diagnostics to print in succession after this one
    sub     : Vec<Self>,
}

impl<F, S> Diagnostic<F, S> {
    /// Constructor for an error.
    /// 
    /// Note that the diagnostic's generics -`F` and `S`- are decuded from the span you give here, so if you are using references in the [`Span`] it means that the diagnostic inherits their lifetimes.
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// A new Diagnostic that will emit an error.
    /// 
    /// # Example
    /// ```rust
    /// use std::borrow::Cow;
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// // Note the propagation of the span types, but not the message.
    /// let diag: Diagnostic<&str, &str> = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
    /// let diag: Diagnostic<Cow<str>, String> = Diagnostic::error("An example error.", Span::new(String::from_utf8_lossy(b"<example>"), "Example".to_string()));
    /// ```
    #[inline]
    pub fn error(message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            remark  : None,
            span    : span.into(),
            kind    : DiagnosticSpecific::Error,
            sub     : vec![],
        }
    }

    /// Constructor for a warning.
    /// 
    /// Note that the diagnostic's generics -`F` and `S`- are decuded from the span you give here, so if you are using references in the [`Span`] it means that the diagnostic inherits their lifetimes.
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// A new Diagnostic that will emit a warning.
    /// 
    /// # Example
    /// ```rust
    /// use std::borrow::Cow;
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// // Note the propagation of the span types, but not the message.
    /// let diag: Diagnostic<&str, &str> = Diagnostic::warn("An example warning.", Span::new("<example>", "Example"));
    /// let diag: Diagnostic<Cow<str>, String> = Diagnostic::warn("An example warning.", Span::new(String::from_utf8_lossy(b"<example>"), "Example".to_string()));
    /// ```
    #[inline]
    pub fn warn(message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            remark  : None,
            span    : span.into(),
            kind    : DiagnosticSpecific::Warning,
            sub     : vec![],
        }
    }

    /// Constructor for a note.
    /// 
    /// Note that the diagnostic's generics -`F` and `S`- are decuded from the span you give here, so if you are using references in the [`Span`] it means that the diagnostic inherits their lifetimes.
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// A new Diagnostic that will emit a note.
    /// 
    /// # Example
    /// ```rust
    /// use std::borrow::Cow;
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// // Note the propagation of the span types, but not the message.
    /// let diag: Diagnostic<&str, &str> = Diagnostic::note("An example note.", Span::new("<example>", "Example"));
    /// let diag: Diagnostic<Cow<str>, String> = Diagnostic::note("An example note.", Span::new(String::from_utf8_lossy(b"<example>"), "Example".to_string()));
    /// ```
    #[inline]
    pub fn note(message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            remark  : None,
            span    : span.into(),
            kind    : DiagnosticSpecific::Note,
            sub     : vec![],
        }
    }

    /// Constructor for a suggestion.
    /// 
    /// Note that the diagnostic's generics -`F` and `S`- are decuded from the span you give here, so if you are using references in the [`Span`] it means that the diagnostic inherits their lifetimes.
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// - `suggestion`: An alternative source code to show instead of the `span`ned source text.
    /// 
    /// # Returns
    /// A new Diagnostic that will emit a suggestion.
    /// 
    /// # Example
    /// ```rust
    /// use std::borrow::Cow;
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// // Note the propagation of the span types, but not the message or the suggestion.
    /// let diag: Diagnostic<&str, &str> = Diagnostic::suggestion("An example suggestion.", Span::new("<example>", "Example"), "A better example.");
    /// let diag: Diagnostic<Cow<str>, String> = Diagnostic::suggestion("An example suggestion.", Span::new(String::from_utf8_lossy(b"<example>"), "Example".to_string()), "A better example.");
    /// ```
    #[inline]
    pub fn suggestion(message: impl Into<String>, span: impl Into<Span<F, S>>, suggestion: impl Into<String>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            remark  : None,
            span    : span.into(),
            kind    : DiagnosticSpecific::Suggestion { suggestion: suggestion.into() },
            sub     : vec![],
        }
    }



    /// Adds a code to this Diagnostic.
    /// 
    /// This is useful for telling the user very short-hand, machine-readable identifiers of the diagnostic. For example, `E001` or `dead_code`.
    /// 
    /// # Arguments
    /// - `code`: The code to set.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// Diagnostic::error("An example error.", Span::new("<example>", "Example"))
    ///     .set_code("1")
    ///     .emit();
    /// ```
    #[inline]
    pub fn set_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }
    /// Adds an in-diagnostic note.
    /// 
    /// This differs from [`Self::add_note()`](Diagnostic::add_note()) in that the latter adds a completely new diagnostic with the `note`-keyword. In contrast, this function simply adds a small note at the end of it.
    /// 
    /// # Arguments
    /// - `message`: The message to add as remark.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// Diagnostic::note("An example note.", Span::new("<example>", "Example"))
    ///     .set_remark("We can apply more notes for just this diagnostic!")
    ///     .emit();
    /// ```
    #[inline]
    pub fn set_remark(mut self, message: impl Into<String>) -> Self {
        self.remark = Some(message.into());
        self
    }

    /// Adds the given Diagnostic to be emitted right after this one.
    /// 
    /// This is useful for providing notes with additional information, say.
    /// 
    /// # Arguments
    /// - `diagnostic`: The Diagnostic to emit.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    /// 
    /// # Examples
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let span: Span<&str, &str> = Span::from_idx("<example>", "pub sttaic TEST = 42;", 4, 9);
    /// Diagnostic::error("Unknown keyword 'sttaic'", span)
    ///     .add(Diagnostic::suggestion("Try 'static'", span, "static"))
    ///     .emit();
    /// ```
    #[inline]
    pub fn add(mut self, diagnostic: impl Into<Diagnostic<F, S>>) -> Self {
        self.sub.push(diagnostic.into());
        self
    }

    /// Adds a new error to be emitted right after this diagnostic.
    /// 
    /// This is a convenience function for calling:
    /// ```ignore
    /// diagnostic.add(Diagnostic::error(message, span));
    /// ```
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    /// 
    /// # Examples
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let source: &str = "pbu static TEST = 42;";
    /// let span1: Span<&str, &str> = Span::from_idx("<example>", source, 0, 2);
    /// let span2: Span<&str, &str> = Span::from_idx("<example>", source, 11, 14);
    /// Diagnostic::error("Unknown keyword 'pbu'", span1)
    ///     .add_error("'TEST' is therefore not publicly accessible", span2)
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_error(mut self, message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        self.sub.push(Self::error(message, span));
        self
    }

    /// Adds a new warning to be emitted right after this diagnostic.
    /// 
    /// This is a convenience function for calling:
    /// ```ignore
    /// diagnostic.add(Diagnostic::warn(message, span));
    /// ```
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    /// 
    /// # Examples
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let source: &str = "pbu static TeST = 42;";
    /// let span1: Span<&str, &str> = Span::from_idx("<example>", source, 0, 2);
    /// let span2: Span<&str, &str> = Span::from_idx("<example>", source, 11, 14);
    /// Diagnostic::error("Unknown keyword 'pbu'", span1)
    ///     .add_warn("Statics are conventionally spelled using full-caps", span2)
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_warn(mut self, message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        self.sub.push(Self::warn(message, span));
        self
    }

    /// Adds a new note to be emitted right after this diagnostic.
    /// 
    /// This is a convenience function for calling:
    /// ```ignore
    /// diagnostic.add(Diagnostic::note(message, span));
    /// ```
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    /// 
    /// # Examples
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let source: &str = "pub static TEST = 42; println!(\"{}\", TEST == true)";
    /// let span1: Span<&str, &str> = Span::from_idx("<example>", source, 37, 48);
    /// let span2: Span<&str, &str> = Span::from_idx("<example>", source, 11, 14);
    /// Diagnostic::error("Cannot compare integer with boolean", span1)
    ///     .add_note("TEST defined here", span2)
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_note(mut self, message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        self.sub.push(Self::note(message, span));
        self
    }

    /// Adds a new suggestion to be emitted right after this diagnostic.
    /// 
    /// This is a convenience function for calling:
    /// ```ignore
    /// diagnostic.add(Diagnostic::suggestion(message, span, code));
    /// ```
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// - `code`: An alternative source code to show instead of the `span`ned source text.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    /// 
    /// # Examples
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let span: Span<&str, &str> = Span::from_idx("<example>", "pub sttaic TEST = 42;", 4, 9);
    /// Diagnostic::error("Unknown keyword 'sttaic'", span)
    ///     .add_suggestion("Try 'static'", span, "static")
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_suggestion(mut self, message: impl Into<String>, span: impl Into<Span<F, S>>, code: impl Into<String>) -> Self {
        self.sub.push(Self::suggestion(message, span, code));
        self
    }



    /// Returns the message in this Diagnostic.
    /// 
    /// # Returns
    /// A [`&str`](str) that refers to the message.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let span: Span<&str, &str> = Span::from_idx("<example>", "Hello, World!", 7, 11);
    /// assert_eq!(Diagnostic::error("An example error", span).message(), "An example error");
    /// ```
    #[inline]
    pub fn message(&self) -> &str { &self.message }

    /// Returns the code of this diagnostic if there is any.
    /// 
    /// # Returns
    /// A [`&str`](str) that refers to the code, or [`None`] is no code is set.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let span: Span<&str, &str> = Span::from_idx("<example>", "Hello, World!", 7, 11);
    /// let diag: Diagnostic<&str, &str> = Diagnostic::error("An example error", span);
    /// assert_eq!(diag.code(), None);
    /// 
    /// // Set the code and try again
    /// let diag: Diagnostic<&str, &str> = diag.set_code("E001");
    /// assert_eq!(diag.code(), Some("E001"));
    /// ```
    #[inline]
    pub fn code(&self) -> Option<&str> { self.code.as_ref().map(|s| s.as_str()) }

    /// Returns the remark of this diagnostic if there is any.
    /// 
    /// # Returns
    /// A [`&str`](str) that refers to the remark, or [`None`] is no remark is set.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let span: Span<&str, &str> = Span::from_idx("<example>", "Hello, World!", 7, 11);
    /// let diag: Diagnostic<&str, &str> = Diagnostic::error("An example error", span);
    /// assert_eq!(diag.remark(), None);
    /// 
    /// // Set the code and try again
    /// let diag: Diagnostic<&str, &str> = diag.set_remark("Hello from below the error");
    /// assert_eq!(diag.remark(), Some("Hello from below the error"));
    /// ```
    #[inline]
    pub fn remark(&self) -> Option<&str> { self.remark.as_ref().map(|s| s.as_str()) }

    /// Returns the span of this Diagnostic.
    /// 
    /// If you want to get the text referred to by it, see [`Self::text()`](Diagnostic::text()) instead.
    /// 
    /// # Returns
    /// A reference to the internal [`Span`].
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let span: Span<&str, &str> = Span::from_idx("<example>", "Hello, World!", 7, 11);
    /// assert_eq!(Diagnostic::error("An example error", span).span(), &span);
    /// ```
    #[inline]
    pub fn span(&self) -> &Span<F, S> { &self.span }

    /// Returns the kind of this Diagnostic.
    /// 
    /// # Returns
    /// The [`DiagnosticKind`] describing what kind of diagnostic this is.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, DiagnosticKind, Span};
    /// 
    /// assert_eq!(Diagnostic::error("Example", Span::new("<example>", "example")).kind(), DiagnosticKind::Error);
    /// ```
    #[inline]
    pub fn kind(&self) -> DiagnosticKind {
        match &self.kind {
            DiagnosticSpecific::Error             => DiagnosticKind::Error,
            DiagnosticSpecific::Warning           => DiagnosticKind::Warning,
            DiagnosticSpecific::Note              => DiagnosticKind::Note,
            DiagnosticSpecific::Suggestion { .. } => DiagnosticKind::Suggestion,
        }
    }

    /// Returns the new code to display.
    /// 
    /// # Returns
    /// A [`&str`](str) that refers to the new code.
    /// 
    /// # Panics
    /// This function may panic if we are not a suggestion. You can check this using [`Self::kind()`](Diagnostic::kind()).
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let span: Span<&str, &str> = Span::from_idx("<example>", "Hello, World!", 7, 11);
    /// assert_eq!(Diagnostic::suggestion("Try writing 'World' lowercase", span, "world").suggestion_text(), "world");
    /// ```
    #[inline]
    #[track_caller]
    pub fn suggestion_text(&self) -> &str { if let DiagnosticSpecific::Suggestion { suggestion } = &self.kind { suggestion } else { panic!("Cannot return the code of a non-Suggestion Diagnostic (is {})", self.kind.variant()); } }
}
impl<F, S: Deref<Target = str>> Diagnostic<F, S> {
    /// Returns the text referred to by the span in this Diagnostic.
    /// 
    /// # Returns
    /// The referred text, as a [`&str`](str).
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Position, Span};
    /// 
    /// let span: Span<&str, &str> = Span::new("<example>", "Hello, World!");
    /// assert_eq!(Diagnostic::error("An example error", span).text(), "Hello, World!");
    /// 
    /// let span: Span<&str, &str> = Span::from_pos("<example>", "Hello, World!", Position::new1(1, 1), Position::new1(1, 5));
    /// assert_eq!(Diagnostic::error("An example error", span).text(), "Hello");
    /// 
    /// let span: Span<&str, &str> = Span::from_idx("<example>", "Hello, World!", 7, 11);
    /// assert_eq!(Diagnostic::error("An example error", span).text(), "World");
    /// ```
    #[inline]
    pub fn text(&self) -> &str { self.span.text() }
}
impl<F: Deref<Target = str>, S: Deref<Target = str>> Diagnostic<F, S> {
    /// Function that *actually* implements `emit_on`, but uses indirection to only print on toplevel diagnostic.
    /// 
    /// # Arguments
    /// - `writer`: The [`Write`]r to emit on.
    /// - `toplevel`: Whether we are the toplevel diagnostic or not. Matters for printing newlines at the end.
    /// 
    /// # Errors
    /// This function may error if we failed to write to the given `writer`.
    #[track_caller]
    fn _emit_on(&self, writer: &mut impl Write, toplevel: bool) -> Result<(), std::io::Error> {
        // Match on the kind to find the keyword and colour to show, as well as lines and span
        let (keyword, colour, lines, start, end): (&'static str, Style, Vec<String>, Position, Position) = match &self.kind {
            // For these we just need the colour and junk
            DiagnosticSpecific::Error   => ("error", Style::new().bold().red(), self.span.lines().into_iter().map(|s| s.into()).collect(), self.span.start(), self.span.end()),
            DiagnosticSpecific::Warning => ("warning", Style::new().bold().yellow(), self.span.lines().into_iter().map(|s| s.into()).collect(), self.span.start(), self.span.end()),
            DiagnosticSpecific::Note    => ("note", Style::new().bold().green(), self.span.lines().into_iter().map(|s| s.into()).collect(), self.span.start(), self.span.end()),

            // Suggestions, however, have a different thing to write too
            DiagnosticSpecific::Suggestion { suggestion } => {
                // First, collect the source to show
                let source: String = self.span.lines().join("\n");
                // Replace the part with the code
                let source: String = source.replace(self.span.text(), suggestion);
                // Store again as a Lines
                let lines: Vec<String> = source.split('\n').map(|s| s.into()).collect();

                // Now get the adapted start and end and return
                let (start, end): (Position, Position) = (self.span.start(), self.span.pos_of(self.span.start + suggestion.graphemes(true).map(|c| c.len()).sum::<usize>() - 1));
                ("suggestion", Style::new().cyan().bold(), lines, start, end)
            },
        };

        // Find the width of the largest line number
        let max_line_width: usize = ((end.line1() as f32).log10()) as usize + 1;

        // Now write the header part of the error
        writeln!(writer, "{}{}{}{}", colour.apply_to(keyword), if let Some(code) = &self.code { colour.apply_to(format!("[{code}]")).to_string() } else { String::new() }, style(": ").bold(), style(&self.message).bold())?;
        writeln!(writer, "{}{} {}:{}", (0..max_line_width).map(|_| ' ').collect::<String>(), style("-->").blue().bright(), &*self.span.file, start)?;

        // Write the toplevel source newline
        writeln!(writer, "{} {}", (0..max_line_width).map(|_| ' ').collect::<String>(), style("|").blue().bright())?;
        // Write the source text next
        for (l, line) in lines.into_iter().enumerate() {
            let l: usize = start.line + l;

            // Write the first line with the (adapted) source text
            write!(writer, "{} {} ", style(format!("{}{}", (0..(max_line_width - ((((l + 1) as f32).log10()) as usize + 1))).map(|_| ' ').collect::<String>(), l + 1)).blue().bright(), style("|").blue().bright())?;
            for (c, ch) in line.grapheme_indices(true) {
                // Write it with accent colour is spanned by the new text
                if (l == start.line && c >= start.col && (l < end.line || c <= end.col)) || (l > start.line && l < end.line) || (l == end.line && c <= end.col && (l > start.line || c >= start.col)) { write!(writer, "{}", colour.apply_to(ch))?; }
                else { write!(writer, "{ch}")?; }
            }
            writeln!(writer)?;

            // Write the second line with the line highlight
            write!(writer, "{} {} ", (0..max_line_width).map(|_| ' ').collect::<String>(), style("|").blue().bright())?;
            for (c, _) in line.grapheme_indices(true) {
                // Write the thing only if within range
                if (l == start.line && c >= start.col && (l < end.line || c <= end.col)) || (l > start.line && l < end.line) || (l == end.line && c <= end.col && (l > start.line || c >= start.col)) { write!(writer, "{}", colour.apply_to('^'))?; }
                else { write!(writer, " ")?; }
            }
            writeln!(writer)?;
        }
        // Write the bottom-level source newline - or the note
        if let Some(remark) = &self.remark {
            writeln!(writer, "{} {} {}: {}", (0..max_line_width).map(|_| ' ').collect::<String>(), style("=").blue().bright(), style("note").bold(), remark)?;
        } else {
            writeln!(writer, "{} {}", (0..max_line_width).map(|_| ' ').collect::<String>(), style("|").blue().bright())?;
        }

        // OK, wrote the suggestion! Then emit the nested ones
        for diagnostic in &self.sub {
            diagnostic._emit_on(writer, false)?;
        }

        // Done!
        if toplevel {
            writeln!(writer)?;
            writeln!(writer)?;
        }
        Ok(())
    }



    /// Emits the diagnostic (and all of its subsequent ones) on the given writer.
    /// 
    /// # Arguments
    /// - `writer`: The [`Write`]r to emit on.
    /// 
    /// # Errors
    /// This function may error if we failed to write to the given `writer`.
    /// 
    /// # Example
    /// ```rust
    /// use std::fs::File;
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let path = std::env::temp_dir().join("testfile_emit_on");
    /// let mut f: File = File::create(path).unwrap();
    /// 
    /// let diag: Diagnostic<&str, &str> = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
    /// diag.emit_on(&mut f);   // Prints the error to the opened file
    /// ```
    #[inline]
    #[track_caller]
    pub fn emit_on(&self, writer: &mut impl Write) -> Result<(), std::io::Error> {
        self._emit_on(writer, true)
    }

    /// Emits the diagnostic (and all of its subsequent ones) on [`stderr`](std::io::Stderr).
    /// 
    /// Note that this function ignores errors (panics on them). Thus, if you expect to fail to write to stderr, please use [`Self::emit_on(std::io::stderr())`](Diagnostic::emit_on()) instead.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let diag: Diagnostic<&str, &str> = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
    /// diag.emit();   // Prints the error to stderr
    /// ```
    #[inline]
    #[track_caller]
    pub fn emit(&self) {
        self.emit_on(&mut std::io::stderr()).unwrap();
    }

    /// Emits the diagnostic (and all of its subsequent ones) on the given writer, then quits the program.
    /// 
    /// The return code is based on the kind of this diagnostic. Specifically:
    /// - [`Error`](DiagnosticKind::Error) will return `1`
    /// - The rest will return `0`.
    /// 
    /// # Arguments
    /// - `writer`: The [`Write`]r to emit on.
    /// 
    /// # Returns
    /// This function will never return.
    /// 
    /// # Errors
    /// This function may error if we failed to write to the given `writer`.
    /// 
    /// # Example
    /// ```should_panic
    /// use std::fs::File;
    /// use std::io::Write as _;
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let path = std::env::temp_dir().join("testfile_abort_on");
    /// let mut f: File = File::create(path).unwrap();
    /// 
    /// let diag: Diagnostic<&str, &str> = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
    /// diag.abort_on(&mut f);   // Prints the error to the opened file
    /// 
    /// // Will never run
    /// writeln!(&mut f, "Hey!").unwrap();
    /// ```
    #[track_caller]
    pub fn abort_on(&self, writer: &mut impl Write) -> Result<Never, std::io::Error> {
        // Emit first
        self.emit_on(writer)?;
        std::process::exit(if matches!(self.kind, DiagnosticSpecific::Error) { 1 } else { 0 });
    }

    /// Emits the diagnostic (and all of its subsequent ones) on [`stderr`](std::io::Stderr), then quits the program.
    /// 
    /// The return code is based on the kind of this diagnostic. Specifically:
    /// - [`Error`](DiagnosticKind::Error) will return `1`
    /// - The rest will return `0`.
    /// 
    /// Note that this function ignores errors (panics on them). Thus, if you expect to fail to write to stderr, please use [`Self::abort_on(std::io::stderr())`](Diagnostic::abort_on()) instead.
    /// 
    /// # Example
    /// ```should_panic
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let diag: Diagnostic<&str, &str> = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
    /// diag.abort();   // Prints the error to stderr
    /// 
    /// // Will never run
    /// eprintln!("Hey!");
    /// ```
    #[inline]
    #[track_caller]
    pub fn abort(&self) -> ! {
        self.abort_on(&mut std::io::stderr()).unwrap();
    }
}

impl<F, S> AsRef<Diagnostic<F, S>> for Diagnostic<F, S> {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl<F, S> AsMut<Diagnostic<F, S>> for Diagnostic<F, S> {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl<F: Clone, S: Clone> From<&Diagnostic<F, S>> for Diagnostic<F, S> {
    #[inline]
    fn from(value: &Diagnostic<F, S>) -> Self { value.clone() }
}
impl<F: Clone, S: Clone> From<&mut Diagnostic<F, S>> for Diagnostic<F, S> {
    #[inline]
    fn from(value: &mut Diagnostic<F, S>) -> Self { value.clone() }
}
