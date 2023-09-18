//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    04 Jul 2023, 19:17:50
//  Last edited:
//    18 Sep 2023, 17:51:36
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Diagnostic`] object, which concerns itself with
//!   prettily formatting an error.
// 

use std::borrow::Cow;
use std::io::Write;

use console::{style, Style};
use enum_debug::EnumDebug;
use never_say_never::Never;
use unicode_segmentation::UnicodeSegmentation as _;

use crate::position::Position;
use crate::span::{IntoSpan, Span, Spanning as _, SpanningExt};


/***** HELPER MACROS *****/
/// Computes the number of digits in the given integer.
macro_rules! n_digits {
    ($n:expr) => { n_digits!($n, usize) };
    ($n:expr, $t:path) => {
        (($n as f64).log10() + 1.0).floor() as $t
    };
}





/***** HELPER FUNCTIONS *****/
/// Writes the header of an error message to the given writer.
/// 
/// # Arguments
/// - `writer`: The [`Write`]r to write on.
/// - `accent_colour`: The colour to write accents with.
/// - `kind`: The error message to show.
/// - `code`: Any error code to show.
/// - `message`: The error message to show.
/// 
/// # Errors
/// This function may error if we failed to write on the given writer.
#[inline]
fn emit_diagnostic_header(writer: &mut impl Write, accent_colour: &Style, kind: &str, code: Option<&str>, message: &str) -> Result<(), std::io::Error> {
    writeln!(writer, "{}{}{}", accent_colour.apply_to(kind), if let Some(code) = code { accent_colour.apply_to(format!("[{code}]")) } else { accent_colour.apply_to(format!("")) }, style(format!(": {message}")).bold())
}

/// Writes the line that gives the source location.
/// 
/// # Arguments
/// - `writer`: The [`Write`]r to write on.
/// - `max_line`: The length of the maximum line number we will be displaying.
/// - `file`: The filename (or other source identifier) to write.
/// - `pos`: The position of the source start.
/// 
/// # Errors
/// This function may error if we failed to write on the given writer.
#[inline]
fn emit_diagnostic_source_pos(writer: &mut impl Write, max_line: usize, file: &str, pos: Position) -> Result<(), std::io::Error> {
    writeln!(writer, "{}{} {}:{}", (0..max_line).map(|_| ' ').collect::<String>(), style("-->").bold().blue(), file, pos)
}

/// Writes a single source line.
/// 
/// # Arguments
/// - `writer`: The [`Write`]r to write on.
/// - `accent_colour`: The colour to write accents with.
/// - `max_line`: The length of the maximum line number we will be displaying.
/// - `source`: The actual source to write.
/// - `accent`: The total access range of this line, ignoring newlines for a sec.
/// - `note`: Any note to write.
/// 
/// # Errors
/// This function may error if we failed to write on the given writer.
#[inline]
fn emit_diagnostic_source_lines<'f, 's>(writer: &mut impl Write, accent_colour: &Style, source: Span<'f, 's>, note: Option<&str>) -> Result<(), std::io::Error> {
    // Fetch the maximum line number
    let max_line: usize = match source.end() {
        Some(end) => end.line1(),
        None      => 1 + source.source().chars().filter(|c| *c == '\n').count(),
    };

    // Next, we extract only the relevant lines from the source
    let lines_range: (Option<usize>, Option<usize>) = source.lines_range();
    let (lines, line, start): (&str, usize, usize) = match lines_range {
        (Some(start), Some(end)) => if start < source.source.len() && end < source.source.len() && start <= end {
            // Fully in bounds
            (&source.source[start..=end], 1 + &source.source[..start].chars().filter(|c| *c == '\n').count(), start)
        } else if start < source.source.len() && start <= end {
            // Now we know the source is non-empty & start is within bounds; clip
            (&source.source[start..], 1 + &source.source[..start].chars().filter(|c| *c == '\n').count(), start)
        } else {
            // Evaluates to an empty slice
            ("", 0, start)
        },

        // The rest is always empty but might reveal any start info
        (Some(start), _) => ("", 0, start),
        (_, _)           => ("", 0, 0),
    };

    // Write the empty line first
    writeln!(writer, "{} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('|').bold().blue())?;

    // Next, write the source lines
    let mut l: usize = line;
    let mut first_accent: bool = true;
    let mut line_buffer: String = String::new();
    let mut mark_buffer: String = String::new();
    for (i, c) in lines.grapheme_indices(true) {
        // Convert the index to one relative to the start of the source
        let i: usize = start + i;

        // Decide whether to apply highlighting to this character
        let highlight: bool = match (source.start_idx(), source.end_idx()) {
            (Some(start), Some(end)) => start <= i && i <= end,
            (None, _)                => false,
            (_, None)                => false,
        };

        // Write it either highlighted or not to the line buffer
        if c != "\n" && highlight {
            line_buffer.push_str(&accent_colour.apply_to(c).to_string());
            if first_accent {
                mark_buffer.push_str(&accent_colour.apply_to('^').to_string());
                first_accent = false;
            } else {
                mark_buffer.push_str(&accent_colour.apply_to('~').to_string());
            }
        } else if c != "\n" {
            line_buffer.push_str(c);
            mark_buffer.push(' ');
        } else {
            // Write the entire line (flush the buffer)
            writeln!(writer, "{}{} {} {}", (0..max_line - n_digits!(l)).map(|_| ' ').collect::<String>(), style(l).blue().bold(), style('|').blue().bold(), line_buffer)?;
            writeln!(writer, "{} {} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('|').blue().bold(), mark_buffer)?;
            line_buffer.clear();
            mark_buffer.clear();
            l += 1;
        }
    }

    // If there's still anything in the buffers, flush it
    if !line_buffer.is_empty() {
        writeln!(writer, "{}{} {} {}", (0..max_line - n_digits!(l)).map(|_| ' ').collect::<String>(), style(l).blue().bold(), style('|').blue().bold(), line_buffer)?;
        writeln!(writer, "{} {} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('|').blue().bold(), mark_buffer)?;
    }

    // Write the final one _or_ a note and we're done
    if let Some(note) = note {
        writeln!(writer, "{} {} {} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('=').bold().blue(), style("note:").bold(), note)
    } else {
        writeln!(writer, "{} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('|').bold().blue())
    }
}





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
        replace : String,
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
/// fn foo(raw: &str) -> Diagnostic {
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



// /// A counterpart to a [`Span`] which is not dependent on, but instead takes ownership of, its two containing strings.
// /// 
// /// This is useful for errors, where we typically do not want the error to depend on the source anymore lifetime-wise.
// /// 
// /// # Example
// /// ```rust
// /// use ast_toolkit::{DiagnosticSpan, Span};
// /// 
// /// // Use the DiagnosticSpan in your errors to be lifetime-free
// /// enum ExampleError {
// ///     SomeError { span: DiagnosticSpan },
// /// }
// /// 
// /// // Then use normal Spans to parse, for efficiency
// /// let span: Span = Span::new("<example>", "Example source text");
// /// let err: ExampleError = ExampleError::SomeError { span: span.into() };
// /// ```
// #[derive(Clone, Debug, Eq)]
// pub struct DiagnosticSpan {
//     /// The filename or other identifier that lets the user identify the source text.
//     pub file    : String,
//     /// The lines that mark the text we want to show with this span.
//     pub source  : String,
//     /// The range spanned in this (limited) source.
//     /// 
//     /// Note that the following holds:
//     /// - Both are inclusive bounds;
//     /// - If either side is [`None`], the range is empty;
//     /// - If `.0 > .1`, then the range is empty; and
//     /// - Either may be out-of-range of the `source` still.
//     pub range   : (Option<usize>, Option<usize>),
//     /// Keeps track of how many lines are skipped before we get to this span.
//     pub skipped : usize,
// }

// impl<'f, 's> Spanning<'f, 's> for DiagnosticSpan {
//     #[inline]
//     fn file(&self) -> &'f str { &self.file }
//     #[inline]
//     fn source(&self) -> &'s str { &self.source }

//     #[inline]
//     fn start_idx(&self) -> Option<usize> { self.range.0 }
//     #[inline]
//     fn end_idx(&self) -> Option<usize> { self.range.1 }
// }
// impl SpanningExt<'static, 'static> for DiagnosticSpan {
//     #[track_caller]
//     fn pos_of(&self, idx: impl AsPrimitive<usize>) -> Position {
//         let idx: usize = idx.as_();
//         let source: &str = self.source();

//         // Assert it is correctly sized
//         if idx >= source.len() { panic!("Given index '{}' is out-of-bounds for DiagnosticSpan of length {}", idx, source.len()); }

//         // Iterate over the source to find the line & column
//         let (mut line, mut col): (usize, usize) = (0, 0);
//         for (i, c) in source.grapheme_indices(true) {
//             // If we reached it, we done
//             if i == idx { break; }
//             else if i > idx { panic!("Index {idx} does not point on the grapheme boundary"); }

//             // Otherwise, count
//             if c == "\n" { line += 1; col = 0; }
//             else { col += 1; }
//         }

//         // Done, return it as a position
//         Position::new0(self.skipped + line, col)
//     }
// }

// impl<'f, 's, T: SpanningExt<'f, 's>> PartialEq<T> for DiagnosticSpan {
//     #[inline]
//     fn eq(&self, other: &T) -> bool {
//         self.file == other.file() && self.text() == self.text()
//     }
// }

// impl<'f, 's> From<Span<'f, 's>> for DiagnosticSpan {
//     #[inline]
//     fn from(value: Span<'f, 's>) -> Self {
//         // Find, find the indices
//         let source: &str = value.source();
//         let (start, end): (Option<usize>, Option<usize>) = (value.start_idx(), value.end_idx());
//         let (line_start, line_end): (Option<usize>, Option<usize>) = crate::span::find_lines_box(source, start, end);

//         // Extract the span
//         let line_source: &str = match (line_start, line_end) {
//             (Some(start), Some(end)) => if start < source.len() && end < source.len() && start <= end {
//                 // Fully in bounds
//                 &source[start..=end]
//             } else if start < source.len() {
//                 // Now we know the source is non-empty & start is within bounds; clip
//                 &source[start..]
//             } else {
//                 // Evaluates to an empty slice
//                 ""
//             },

//             // The rest is always empty
//             (_, _) => "",
//         };

//         // Scale the span's ranges to the found ranges
//         let rel_start: Option<usize> = match (line_start, start) {
//             (Some(line_start), Some(start)) => Some(start - line_start),
//             (_, _)                          => None,
//         };
//         let rel_end: Option<usize> = match (line_start, end) {
//             (Some(line_start), Some(end)) => Some(end - line_start),
//             (_, _)                        => None,
//         };

//         // Compute the number of lines skipped
//         let skipped: usize = match line_start {
//             Some(start) => source[..start].chars().filter(|c| *c == '\n').count(),
//             None        => 0,
//         };

//         // Extract that piece of text and return ourselves
//         Self {
//             file   : value.file.into(),
//             source : line_source.into(),
//             range  : (rel_start, rel_end),
//             skipped,
//         }
//     }
// }

// impl AsRef<DiagnosticSpan> for DiagnosticSpan {
//     #[inline]
//     fn as_ref(&self) -> &DiagnosticSpan { self }
// }
// impl AsMut<DiagnosticSpan> for DiagnosticSpan {
//     #[inline]
//     fn as_mut(&mut self) -> &mut DiagnosticSpan { self }
// }
// impl From<&DiagnosticSpan> for DiagnosticSpan {
//     #[inline]
//     fn from(value: &DiagnosticSpan) -> Self { value.clone() }
// }
// impl From<&mut DiagnosticSpan> for DiagnosticSpan {
//     #[inline]
//     fn from(value: &mut DiagnosticSpan) -> Self { value.clone() }
// }



// /// A companion trait for the [`DiagnosticSpan`] that allows it to be created from any other [`Spanning`].
// /// 
// /// This trait exists to circumvent the fact that [`From<S>`], where `S: Spanning`, conflicts with the default implementation [`From<DiagnosticSpan>`].
// pub trait DiagnosticSpannable {
//     /// Creates a [`DiagnosticSpan`] out of Self.
//     /// 
//     /// # Returns
//     /// A [`DiagnosticSpan`] suitable for use in [`Diagnostic`]s to carry around without lifetimes.
//     fn into_dspan(&self) -> DiagnosticSpan;
// }
// impl<'a, T: Spanning<'a, 'a>> DiagnosticSpannable for T {
//     fn into_dspan(&self) -> DiagnosticSpan {
//         // Find, find the indices
//         let source: &str = self.source();
//         let (start, end): (Option<usize>, Option<usize>) = (self.start_idx(), self.end_idx());
//         let (line_start, line_end): (Option<usize>, Option<usize>) = crate::span::find_lines_box(source, start, end);

//         // Extract the span
//         let line_source: &str = match (line_start, line_end) {
//             (Some(start), Some(end)) => if start < source.len() && end < source.len() && start <= end {
//                 // Fully in bounds
//                 &source[start..=end]
//             } else if start < source.len() {
//                 // Now we know the source is non-empty & start is within bounds; clip
//                 &source[start..]
//             } else {
//                 // Evaluates to an empty slice
//                 ""
//             },

//             // The rest is always empty
//             (_, _) => "",
//         };

//         // Scale the span's ranges to the found ranges
//         let rel_start: Option<usize> = match (line_start, start) {
//             (Some(line_start), Some(start)) => if line_start <= start { Some(start - line_start) } else { None },
//             (_, _)                          => None,
//         };
//         let rel_end: Option<usize> = match (line_start, end) {
//             (Some(line_start), Some(end)) => if line_start <= end { Some(end - line_start) } else { None },
//             (_, _)                        => None,
//         };

//         // Compute the number of lines skipped
//         let skipped: usize = match line_start {
//             Some(start) => source[..start].chars().filter(|c| *c == '\n').count(),
//             None        => 0,
//         };

//         // Extract that piece of text and return ourselves
//         DiagnosticSpan {
//             file   : self.file().into(),
//             source : line_source.into(),
//             range  : (rel_start, rel_end),
//             skipped,
//         }
//     }
// }
// impl<'a> From<Span<'a, 'a>> for DiagnosticSpan {
//     #[inline]
//     fn from(value: Span<'a, 'a>) -> Self { value.into_dspan() }
// }

/// A companion trait for the [`Diagnostic`] that allows one to use `X.into_diag()` instead of [`Diagnostic::from(X)`](From<Diagnostic>::from())).
pub trait Diagnosticable<'f, 's>: Into<Diagnostic<'f, 's>> {
    /// Shorthand for [`Diagnostic::from(self)`](From<Diagnostic>::from()).
    /// 
    /// Also equivalent to [`self.into()`](Into<Diagnostic>::into()) but then without type guessing.
    /// 
    /// # Returns
    /// A [`Diagnostic`] created from self.
    fn into_diag(self) -> Diagnostic<'f, 's>;
}
impl<'f, 's, T: Into<Diagnostic<'f, 's>>> Diagnosticable<'f, 's> for T {
    #[inline]
    #[track_caller]
    fn into_diag(self) -> Diagnostic<'f, 's> { self.into() }
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
/// let err = Diagnostic::error("Invalid word 'Hlelo'", Span::ranged("<example>", "Hlelo World!", 0..=4));
/// let warn = Diagnostic::warn("Second word shouldn't be capitalized", Span::ranged("<example>", "Hlelo World!", 6..=6));
/// let note = Diagnostic::note("The most classical program in the world is given here", Span::new("<example>", "Hlelo World!"));
/// let suggest = Diagnostic::suggestion("Consider writing it properly", Span::new("<example>", "Hlelo World!"), "Hello, world!");
/// ```
/// To print the diagnostics, check [`Diagnostic::emit()`]:
/// ```rust
/// # use ast_toolkit::{Diagnostic, Span};
/// # 
/// # let err = Diagnostic::error("Invalid word 'Hlelo'", Span::ranged("<example>", "Hlelo World!", 0..=4));
/// # let warn = Diagnostic::warn("Second word shouldn't be capitalized", Span::ranged("<example>", "Hlelo World!", 6..=6));
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
pub struct Diagnostic<'f, 's> {
    /// The message to show
    message : String,
    /// Some code identifier for distinguishing errors machine-wise.
    code    : Option<String>,
    /// The in-diagnostic note to display, if any.
    remark  : Option<String>,
    /// The span that relates this message to the source text.
    span    : Span<'f, 's>,
    /// Anything kind-specific.
    kind    : DiagnosticSpecific,
    /// Any other diagnostics to print in succession after this one
    sub     : Vec<Self>,
}

impl<'f, 's> Diagnostic<'f, 's> {
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
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let diag = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
    /// ```
    #[inline]
    pub fn error(message: impl Into<String>, span: impl IntoSpan<'f, 's>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            remark  : None,
            span    : span.into_span(),
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
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let diag: Diagnostic = Diagnostic::warn("An example warning.", Span::new("<example>", "Example"));
    /// ```
    #[inline]
    pub fn warn(message: impl Into<String>, span: impl IntoSpan<'f, 's>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            remark  : None,
            span    : span.into_span(),
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
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let diag: Diagnostic = Diagnostic::note("An example note.", Span::new("<example>", "Example"));
    /// ```
    #[inline]
    pub fn note(message: impl Into<String>, span: impl IntoSpan<'f, 's>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            remark  : None,
            span    : span.into_span(),
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
    /// - `replacement`: An alternative source code to show instead of the `span`ned source text.
    /// 
    /// # Returns
    /// A new Diagnostic that will emit a suggestion.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let diag: Diagnostic = Diagnostic::suggestion("An example suggestion.", Span::new("<example>", "Example"), "A better example.");
    /// ```
    #[inline]
    pub fn suggestion(message: impl Into<String>, span: impl IntoSpan<'f, 's>, replacement: impl Into<String>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            remark  : None,
            span    : span.into_span(),
            kind    : DiagnosticSpecific::Suggestion { replace: replacement.into() },
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
    /// let span = Span::ranged("<example>", "pub sttaic TEST = 42;", 4..=9);
    /// Diagnostic::error("Unknown keyword 'sttaic'", span)
    ///     .add(Diagnostic::suggestion("Try 'static'", span, "static"))
    ///     .emit();
    /// ```
    #[inline]
    pub fn add(mut self, diagnostic: impl Into<Diagnostic<'f, 's>>) -> Self {
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
    /// let span1 = Span::ranged("<example>", source, 0..=2);
    /// let span2 = Span::ranged("<example>", source, 11..=14);
    /// Diagnostic::error("Unknown keyword 'pbu'", span1)
    ///     .add_error("'TEST' is therefore not publicly accessible", span2)
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_error(mut self, message: impl Into<String>, span: impl IntoSpan<'f, 's>) -> Self {
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
    /// let span1 = Span::ranged("<example>", source, 0..=2);
    /// let span2 = Span::ranged("<example>", source, 11..=14);
    /// Diagnostic::error("Unknown keyword 'pbu'", span1)
    ///     .add_warn("Statics are conventionally spelled using full-caps", span2)
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_warn(mut self, message: impl Into<String>, span: impl IntoSpan<'f, 's>) -> Self {
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
    /// let span1 = Span::ranged("<example>", source, 37..=48);
    /// let span2 = Span::ranged("<example>", source, 11..=14);
    /// Diagnostic::error("Cannot compare integer with boolean", span1)
    ///     .add_note("TEST defined here", span2)
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_note(mut self, message: impl Into<String>, span: impl IntoSpan<'f, 's>) -> Self {
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
    /// let span = Span::ranged("<example>", "pub sttaic TEST = 42;", 4..=9);
    /// Diagnostic::error("Unknown keyword 'sttaic'", span)
    ///     .add_suggestion("Try 'static'", span, "static")
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_suggestion(mut self, message: impl Into<String>, span: impl IntoSpan<'f, 's>, code: impl Into<String>) -> Self {
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
    /// let span = Span::ranged("<example>", "Hello, World!", 7..=11);
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
    /// let span = Span::ranged("<example>", "Hello, World!", 7..=11);
    /// let diag: Diagnostic = Diagnostic::error("An example error", span);
    /// assert_eq!(diag.code(), None);
    /// 
    /// // Set the code and try again
    /// let diag: Diagnostic = diag.set_code("E001");
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
    /// let span = Span::ranged("<example>", "Hello, World!", 7..=11);
    /// let diag: Diagnostic = Diagnostic::error("An example error", span);
    /// assert_eq!(diag.remark(), None);
    /// 
    /// // Set the code and try again
    /// let diag: Diagnostic = diag.set_remark("Hello from below the error");
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
    /// let span = Span::ranged("<example>", "Hello, World!", 7..=11);
    /// assert_eq!(Diagnostic::error("An example error", span).span(), &span);
    /// ```
    #[inline]
    pub fn span(&self) -> Span<'f, 's> { self.span }

    /// Returns the text referred to by the span in this Diagnostic.
    /// 
    /// # Returns
    /// The referred text, as a [`&str`](str).
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{Diagnostic, Position, Span};
    /// 
    /// let span = Span::new("<example>", "Hello, World!");
    /// assert_eq!(Diagnostic::error("An example error", span).text(), "Hello, World!");
    /// 
    /// let span = Span::ranged("<example>", "Hello, World!", 7..=11);
    /// println!("{:?}", Diagnostic::error("An example error", span));
    /// assert_eq!(Diagnostic::error("An example error", span).text(), "World");
    /// ```
    #[inline]
    pub fn text(&self) -> &str { self.span.text() }

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

    /// Returns the new code to suggest instead of the currently highlighted code.
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
    /// let span = Span::ranged("<example>", "Hello, World!", 7..=11);
    /// assert_eq!(Diagnostic::suggestion("Try writing 'World' lowercase", span, "world").replacement(), "world");
    /// ```
    #[inline]
    #[track_caller]
    pub fn replacement(&self) -> &str { if let DiagnosticSpecific::Suggestion { replace } = &self.kind { replace } else { panic!("Cannot return the code of a non-Suggestion Diagnostic (is {})", self.kind.variant()); } }



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
        // Determine some properties based on the new information
        let (kind, accent_colour): (&str, Style) = match &self.kind {
            DiagnosticSpecific::Error             => ("error", Style::new().bold().red()),
            DiagnosticSpecific::Warning           => ("warning", Style::new().bold().yellow()),
            DiagnosticSpecific::Note              => ("note", Style::new().bold().green()),
            DiagnosticSpecific::Suggestion { .. } => ("suggestion", Style::new().bold().cyan()),
        };

        // We can always write the top line
        emit_diagnostic_header(writer, &accent_colour, kind, self.code.as_ref().map(|c| c.as_str()), &self.message)?;

        // If there is a range to emit, then also emit source stuff
        if !self.span.is_empty() {
            // If it's a suggestion, then catch the source and range and make some tweaks
            let (source, accent_range): (Cow<str>, (Option<usize>, Option<usize>)) = match &self.kind {
                DiagnosticSpecific::Suggestion { replace } => {
                    // First: construct a new source
                    let mut new_source: String = String::new();
                    match self.span.range {
                        (Some(start), Some(end)) => {
                            // First, add everything before the start (if it's within range, at least)
                            let source: &str = self.span.source;
                            if start < source.len() {
                                new_source.push_str(&source[..start]);
                            }

                            // Add the replacement string
                            new_source.push_str(replace);

                            // Next, add everything after the end if it would make a non-empty span...
                            if start <= end && end < source.len() {
                                // Note we do the skip to exclude the bound position itself over a grapheme, which is inclusive
                                new_source.push_str(&source[end..].graphemes(true).skip(1).collect::<String>());
                            } else if start < source.len() {
                                // ...or we consider start a length-zero span (which is why we include start here)
                                new_source.push_str(&source[start..]);
                            }
                        },
                        (Some(start), None) => {
                            // First, add everything before the start (if it's within range, at least)
                            let source: &str = self.span.source;
                            if start < source.len() {
                                new_source.push_str(&source[..start]);
                            }

                            // Add the replacement string
                            new_source.push_str(replace);

                            // ...and add everything after the start (if it's within range, at least)
                            if start < source.len() {
                                new_source.push_str(&source[start..]);
                            }
                        },
                        (None, _) => {
                            // We're shooting in the dark here, only add the replacement
                            new_source.push_str(replace);
                        }
                    }

                    // Then: compute the new range of the source
                    let new_range: (Option<usize>, Option<usize>) = match self.span.range {
                        (Some(start), _) => (Some(start), Some(start + replace.len() - 1)),
                        (None, _)        => (None, None),
                    };

                    // Alright, return the new things
                    (Cow::Owned(new_source), new_range)
                },

                DiagnosticSpecific::Error | DiagnosticSpecific::Warning | DiagnosticSpecific::Note => (Cow::Borrowed(self.span.source), self.span.range),
            };

            // Write the source stuff
            // NOTE: We can safely unwrap the positions because we assert the range is not empty
            let max_line: usize = n_digits!(self.span.end().unwrap().line1());
            emit_diagnostic_source_pos(writer, max_line, self.span.file, self.span.start().unwrap())?;
            emit_diagnostic_source_lines(writer, &accent_colour, Span { file: self.span.file, source: source.as_ref(), range: accent_range }, self.remark.as_ref().map(|r| r.as_str()))?;
        }

        // Recursively write child diagnostics
        for diagnostic in &self.sub {
            diagnostic._emit_on(writer, false)?;
        }

        // If we're not the toplevel diagnostic, write the closing newlines
        if toplevel {
            writeln!(writer)?;
            writeln!(writer)?;
        }

        // And that's it!
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
    /// let diag: Diagnostic = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
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
    /// let diag: Diagnostic = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
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
    /// let diag: Diagnostic = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
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
    /// let diag = Diagnostic::error("An example error.", Span::new("<example>", "Example"));
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

impl<'f, 's> AsRef<Diagnostic<'f, 's>> for Diagnostic<'f, 's> {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl<'f, 's> AsMut<Diagnostic<'f, 's>> for Diagnostic<'f, 's> {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl<'f, 's> From<&Diagnostic<'f, 's>> for Diagnostic<'f, 's> {
    #[inline]
    fn from(value: &Diagnostic<'f, 's>) -> Self { value.clone() }
}
impl<'f, 's> From<&mut Diagnostic<'f, 's>> for Diagnostic<'f, 's> {
    #[inline]
    fn from(value: &mut Diagnostic<'f, 's>) -> Self { value.clone() }
}
