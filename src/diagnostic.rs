//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    04 Jul 2023, 19:17:50
//  Last edited:
//    05 Jul 2023, 12:07:00
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
/// Represents a (series of) source-bound errors that can be neatly formatted.
#[derive(Clone, Debug)]
pub struct Diagnostic<F, S> {
    /// The message to show
    message : String,
    /// Some code identifier for distinguishing errors machine-wise.
    code    : Option<String>,
    /// The in-diagnostic note to display, if any.
    note    : Option<String>,
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
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// A new Diagnostic that will emit an error.
    #[inline]
    pub fn error(message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            note    : None,
            span    : span.into(),
            kind    : DiagnosticSpecific::Error,
            sub     : vec![],
        }
    }

    /// Constructor for a warning.
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// A new Diagnostic that will emit a warning.
    #[inline]
    pub fn warn(message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            note    : None,
            span    : span.into(),
            kind    : DiagnosticSpecific::Warning,
            sub     : vec![],
        }
    }

    /// Constructor for a note.
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// A new Diagnostic that will emit a note.
    #[inline]
    pub fn note(message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            note    : None,
            span    : span.into(),
            kind    : DiagnosticSpecific::Note,
            sub     : vec![],
        }
    }

    /// Constructor for a suggestion.
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// - `suggestion`: An alternative source code to show instead of the `span`ned source text.
    /// 
    /// # Returns
    /// A new Diagnostic that will emit a suggestion.
    #[inline]
    pub fn suggestion(message: impl Into<String>, span: impl Into<Span<F, S>>, suggestion: impl Into<String>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            note    : None,
            span    : span.into(),
            kind    : DiagnosticSpecific::Suggestion { suggestion: suggestion.into() },
            sub     : vec![],
        }
    }



    /// Adds a code to this Diagnostic.
    /// 
    /// This is useful for telling the user very short-hand, machine-readable identifiers of the error or warning. For example, `E001` or `dead_code`.
    /// 
    /// # Arguments
    /// - `code`: The code to set.
    /// 
    /// # Returns
    /// `self` to allow chaining.
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
    /// - `message`: The message to add as note.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    #[inline]
    pub fn set_note(mut self, message: impl Into<String>) -> Self {
        self.note = Some(message.into());
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
    #[inline]
    pub fn add(mut self, diagnostic: impl Into<Diagnostic<F, S>>) -> Self {
        self.sub.push(diagnostic.into());
        self
    }

    /// Adds a new error to be emitted right after this diagnostic.
    /// 
    /// This is a convenience function for calling:
    /// ```no_run
    /// diagnostic.add(Diagnostic::error(message, span));
    /// ```
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    #[inline]
    pub fn add_error(mut self, message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        self.sub.push(Self::error(message, span));
        self
    }

    /// Adds a new warning to be emitted right after this diagnostic.
    /// 
    /// This is a convenience function for calling:
    /// ```no_run
    /// diagnostic.add(Diagnostic::warn(message, span));
    /// ```
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    #[inline]
    pub fn add_warn(mut self, message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        self.sub.push(Self::warn(message, span));
        self
    }

    /// Adds a new note to be emitted right after this diagnostic.
    /// 
    /// This is a convenience function for calling:
    /// ```no_run
    /// diagnostic.add(Diagnostic::note(message, span));
    /// ```
    /// 
    /// # Arguments
    /// - `message`: The message to show to the user.
    /// - `span`: The [`Span`] that relates this diagnostic to the source text.
    /// 
    /// # Returns
    /// `self` to allow chaining.
    #[inline]
    pub fn add_note(mut self, message: impl Into<String>, span: impl Into<Span<F, S>>) -> Self {
        self.sub.push(Self::note(message, span));
        self
    }

    /// Adds a new suggestion to be emitted right after this diagnostic.
    /// 
    /// This is a convenience function for calling:
    /// ```no_run
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
    #[inline]
    pub fn add_suggestion(mut self, message: impl Into<String>, span: impl Into<Span<F, S>>, code: impl Into<String>) -> Self {
        self.sub.push(Self::suggestion(message, span, code));
        self
    }



    /// Returns the message in this Diagnostic.
    /// 
    /// # Returns
    /// A [`&str`](str) that refers to the message.
    #[inline]
    pub fn message(&self) -> &str { &self.message }

    /// Returns the code of this diagnostic if there is any.
    /// 
    /// # Returns
    /// A [`&str`](str) that refers to the code, or [`None`] is no code is set.
    #[inline]
    pub fn code(&self) -> Option<&str> { self.code.as_ref().map(|s| s.as_str()) }

    /// Returns the span of this Diagnostic.
    /// 
    /// If you want to get the text referred to by it, see [`Self::text()`](Diagnostic::text()) instead.
    /// 
    /// # Returns
    /// A reference to the internal [`Span`].
    #[inline]
    pub fn span(&self) -> &Span<F, S> { &self.span }

    /// Returns the kind of this Diagnostic.
    /// 
    /// # Returns
    /// The [`DiagnosticKind`] describing what kind of diagnostic this is.
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
    /// This function may panic if we are not a suggestion.
    #[inline]
    #[track_caller]
    pub fn suggestion_text(&self) -> &str { if let DiagnosticSpecific::Suggestion { suggestion } = &self.kind { suggestion } else { panic!("Cannot return the code of a non-Suggestion Diagnostic (is {})", self.kind.variant()); } }
}
impl<F, S: Deref<Target = str>> Diagnostic<F, S> {
    /// Returns the text referred to by the span in this Diagnostic.
    /// 
    /// # Returns
    /// The referred text, as a [`&str`](str).
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
    pub fn _emit_on(&self, writer: &mut impl Write, toplevel: bool) -> Result<(), std::io::Error> {
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
                let (start, end): (Position, Position) = (self.span.start(), self.span.pos_of(self.span.start + suggestion.graphemes(true).count()));
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
        if let Some(note) = &self.note {
            writeln!(writer, "{} {} {}: {}", (0..max_line_width).map(|_| ' ').collect::<String>(), style("=").blue().bright(), style("note").bold(), note)?;
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
    #[inline]
    #[track_caller]
    pub fn emit_on(&self, writer: &mut impl Write) -> Result<(), std::io::Error> {
        self._emit_on(writer, true)
    }

    /// Emits the diagnostic (and all of its subsequent ones) on [`stderr`](std::io::Stderr).
    /// 
    /// Note that this function ignores errors (panics on them). Thus, if you expect to fail to write to stderr, please use [`Self::emit_on(std::io::stderr())`](Diagnostic::emit_on()) instead.
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
