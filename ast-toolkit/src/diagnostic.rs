//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    04 Jul 2023, 19:17:50
//  Last edited:
//    27 Aug 2023, 12:17:29
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the [`Diagnostic`] object, which concerns itself with
//!   prettily formatting an error.
// 

use std::borrow::Cow;
use std::io::Write;
use std::ops::{Bound, RangeBounds as _};

use console::{style, Style};
use enum_debug::EnumDebug;
use never_say_never::Never;
use num_traits::AsPrimitive;
use unicode_segmentation::UnicodeSegmentation as _;

use crate::position::Position;
use crate::span::{Span, SpanRange};


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
/// - `line`: The line number of the first line to write (one-indexed).
/// - `source`: The actual source to write.
/// - `accent`: The total access range of this line, ignoring newlines for a sec.
/// - `note`: Any note to write.
/// 
/// # Errors
/// This function may error if we failed to write on the given writer.
#[inline]
fn emit_diagnostic_source_lines(writer: &mut impl Write, accent_colour: &Style, max_line: usize, line: usize, source: &str, accent: SpanRange, note: Option<&str>) -> Result<(), std::io::Error> {
    // Write the empty line first
    writeln!(writer, "{} {}", (0..max_line).map(|_| ' ').collect::<String>(), style('|').bold().blue())?;

    // Next, write the source lines
    let mut l: usize = line;
    let mut first_accent: bool = true;
    let mut line_buffer: String = String::new();
    let mut mark_buffer: String = String::new();
    for (i, c) in source.grapheme_indices(true) {
        // Decide whether to apply highlighting to this character
        let highlight: bool = match (accent.start_bound(), accent.end_bound()) {
            (Bound::Excluded(start), Bound::Excluded(end)) => *start < i && i < *end,
            (Bound::Excluded(start), Bound::Included(end)) => *start < i && i <= *end,
            (Bound::Excluded(start), Bound::Unbounded)     => *start < i,
            (Bound::Included(start), Bound::Excluded(end)) => *start <= i && i < *end,
            (Bound::Included(start), Bound::Included(end)) => *start <= i && i <= *end,
            (Bound::Included(start), Bound::Unbounded)     => *start <= i,
            (Bound::Unbounded, Bound::Excluded(end))       => i < *end,
            (Bound::Unbounded, Bound::Included(end))       => i <= *end,
            (Bound::Unbounded, Bound::Unbounded)           => true,
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



/// A counterpart to a [`Span`] which is not dependent on, but instead takes ownership of, its two containing strings.
/// 
/// This is useful for errors, where we typically do not want the error to depend on the source anymore lifetime-wise.
/// 
/// # Example
/// ```rust
/// use ast_toolkit::{DiagnosticSpan, Span};
/// 
/// // Use the DiagnosticSpan in your errors to be lifetime-free
/// enum ExampleError {
///     SomeError { span: DiagnosticSpan },
/// }
/// 
/// // Then use normal Spans to parse, for efficiency
/// let span: Span = Span::new("<example>", "Example source text");
/// let err: ExampleError = ExampleError::SomeError { span: span.into() };
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DiagnosticSpan {
    /// The filename or other identifier that lets the user identify the source text.
    pub file    : String,
    /// The lines that mark the text we want to show with this span.
    pub source  : String,
    /// The range spanned in this (limited) source.
    pub range   : SpanRange,
    /// Keeps track of how many lines are skipped before we get to this span.
    pub skipped : usize,
}

impl DiagnosticSpan {
    /// Returns the text contained in this [`Span`].
    /// 
    /// # Returns
    /// A reference to the internally stored string.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{DiagnosticSpan, Span};
    /// 
    /// let span: DiagnosticSpan = Span::new("<example>", "Example text").into();
    /// assert_eq!(span.text(), "Example text");
    /// ```
    #[inline]
    pub fn text(&self) -> &str { self.range.slice(&self.source) }

    /// Converts a character index to a [`Position`] within this span's source text.
    /// 
    /// Note that the position given is _relative_ to the source text, but line-wise; this function ignores the spanned area but only up a sense. If this sounds confusing, don't worry, as long as you use indices coming from this DiagnosticSpan itself you're fine.
    /// 
    /// # Arguments
    /// - `index`: The index to translate.
    /// 
    /// # Returns
    /// A new [`Position`] representing the index as a (line, column) coordinate.
    /// 
    /// # Panics
    /// This function may panic if the given index is not at the grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{DiagnosticSpan, Position, Span};
    /// 
    /// let span1: DiagnosticSpan = Span::new("<example>", "Hello\nworld!").into();
    /// let span2: DiagnosticSpan = Span::from_idx("<example>", "Hello\nworld!", 0, 4).into();
    /// let span3: DiagnosticSpan = Span::from_idx("<example>", "Hello\nworld!", 6, 10).into();
    /// 
    /// assert_eq!(span1.pos_of(3), Position::new0(0, 3));
    /// assert_eq!(span1.pos_of(7), Position::new0(1, 1));
    /// assert_eq!(span2.pos_of(3), Position::new0(0, 3));
    /// assert_eq!(span3.pos_of(3), Position::new0(1, 3));
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::{DiagnosticSpan, Span};
    /// // This will panic!
    /// DiagnosticSpan::from(Span::new("<example>", "Hÿllo\nworld!")).pos_of(2);
    /// ```
    #[track_caller]
    pub fn pos_of(&self, index: impl AsPrimitive<usize>) -> Position {
        let index: usize = index.as_();

        // Assert it is correctly sized
        if index >= self.source.len() { panic!("Given index '{}' is out-of-bounds for DiagnosticSpan of length {}", index, self.source.len()); }

        // Iterate over the source to find the line & column
        let (mut line, mut col): (usize, usize) = (0, 0);
        for (i, c) in self.source.grapheme_indices(true) {
            // If we reached it, we done
            if i == index { break; }
            else if i > index { panic!("Index {} does not point to grapheme boundary", index); }

            // Otherwise, count
            if c == "\n" { line += 1; col = 0; }
            else { col += 1; }
        }

        // Done, return it as a position
        Position::new0(self.skipped + line, col)
    }

    /// Returns the start position of this span as a [`Position`].
    /// 
    /// # Returns
    /// A [`Position`] describing the start position in the source text, or [`None`] if we are empty.
    /// 
    /// # Panics
    /// This function may panic if `start` does not point at the unicode grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{DiagnosticSpan, Position, Span};
    /// 
    /// let span1: DiagnosticSpan = Span::new("<example>", "Hello\nworld!").into();
    /// let span2: DiagnosticSpan = Span::from_idx("<example>", "Hello\nworld!", 2, 2).into();
    /// let span3: DiagnosticSpan = Span::from_idx("<example>", "Hello\nworld!", 6, 10).into();
    /// let span4: DiagnosticSpan = Span::from_idx("<example>", "Hello\nworld!", 10, 9).into();
    /// 
    /// assert_eq!(span1.start(), Position::new0(0, 0));
    /// assert_eq!(span2.start(), Position::new0(0, 2));
    /// assert_eq!(span3.start(), Position::new0(1, 0));
    /// assert_eq!(span5.start(), Position::new0(1, 4));
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::{DiagnosticSpan, Span};
    /// // This will panic!
    /// DiagnosticSpan::from(Span::from_idx("<example>", "Hÿllo\nworld!", 2, 6)).start();
    /// ```
    #[inline]
    #[track_caller]
    pub fn start(&self) -> Position {
        match self.range.start_bound() {
            Bound::Excluded(start) => self.pos_of(if *start < usize::MAX { *start + 1 } else { panic!("Cannot convert excluded start of `usize::MAX` to inclusive (would overflow)") }),
            Bound::Included(start) => self.pos_of(*start),
            Bound::Unbounded       => self.pos_of(0),
        }
    }

    /// Returns the end position of this span as a [`Position`].
    /// 
    /// # Returns
    /// A [`Position`] describing the end position in the source text, or [`None`] if we are empty.
    /// 
    /// # Panics
    /// This function may panic if `end` does not point at the unicode grapheme boundary.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{DiagnosticSpan, Position, Span};
    /// 
    /// let span1: DiagnosticSpan = Span::new("<example>", "Hello world!").into();
    /// let span2: DiagnosticSpan = Span::new("<example>", "Hello\nworld!").into();
    /// let span3: DiagnosticSpan = Span::from_idx("<example>", "Hello\nworld!", 2, 2).into();
    /// let span4: DiagnosticSpan = Span::from_idx("<example>", "Hello\nworld!", 6, 10).into();
    /// let span5: DiagnosticSpan = Span::from_idx("<example>", "Hello\nworld!", 10, 9).into();
    /// 
    /// assert_eq!(span1.end(), Position::new0(0, 11));
    /// assert_eq!(span2.end(), Position::new0(1, 5));
    /// assert_eq!(span3.end(), Position::new0(0, 2));
    /// assert_eq!(span4.end(), Position::new0(1, 4));
    /// assert_eq!(span5.end(), Position::new0(1, 3));
    /// ```
    /// ```should_panic
    /// # use ast_toolkit::{DiagnosticSpan, Span};
    /// // This will panic!
    /// DiagnosticSpan::from(Span::from_idx("<example>", "Hÿllo\nworld!", 2, 6)).end();
    /// ```
    #[inline]
    #[track_caller]
    pub fn end(&self) -> Position {
        match self.range.start_bound() {
            Bound::Excluded(end) => self.pos_of(if *end > 0 { *end - 1 } else { panic!("Cannot convert excluded end of `0` to inclusive (would underflow)") }),
            Bound::Included(end) => self.pos_of(*end),
            Bound::Unbounded     => self.pos_of(if !self.source.is_empty() { self.source.len() - 1 } else { panic!("Cannot get position of unbounded end in empty source"); }),
        }
    }



    /// Returns if this DiagnosticSpan would return a non-empty text.
    /// 
    /// # Returns
    /// True if this DiagnosticSpan spans nothing, or false otherwise.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{DiagnosticSpan, Span};
    /// 
    /// assert_eq!(DiagnosticSpan::from(Span::new("<example>", "Hello, world!")).is_empty(), false);
    /// assert_eq!(DiagnosticSpan::from(Span::from_idx("<example>", "Hello, world!", 0, 4)).is_empty(), false);
    /// assert_eq!(DiagnosticSpan::from(Span::from_idx("<example>", "Hello, world!", 6, 5)).is_empty(), true);
    /// ```
    #[inline]
    pub fn is_empty(&self) -> bool { self.len() > 0 }

    /// Returns the number of logical units that this DiagnosticSpan spans.
    /// 
    /// # Returns
    /// The number of characters.
    /// 
    /// # Example
    /// ```rust
    /// use ast_toolkit::{DiagnosticSpan, Span};
    /// 
    /// assert_eq!(DiagnosticSpan::from(Span::new("<example>", "Hello, world!")).len(), 13);
    /// assert_eq!(DiagnosticSpan::from(Span::from_idx("<example>", "Hello, world!", 0, 4)).len(), 5);
    /// assert_eq!(DiagnosticSpan::from(Span::from_idx("<example>", "Hello, world!", 7, 12)).len(), 6);
    /// assert_eq!(DiagnosticSpan::from(Span::from_idx("<example>", "Hello, world!", 5, 5)).len(), 1);
    /// assert_eq!(DiagnosticSpan::from(Span::from_idx("<example>", "Hello, world!", 6, 5)).len(), 0);
    /// ```
    #[inline]
    pub fn len(&self) -> usize { self.range.len(self.source.len()) }
}

impl<'f, 's> PartialEq<Span<'f, 's>> for DiagnosticSpan {
    #[inline]
    fn eq(&self, other: &Span<'f, 's>) -> bool {
        self.file == other.file && self.text() == other.text()
    }
}
impl<'f, 's> PartialEq<DiagnosticSpan> for Span<'f, 's> {
    #[inline]
    fn eq(&self, other: &DiagnosticSpan) -> bool {
        self.file == other.file && self.text() == other.text()
    }
}

impl<'f, 's> From<Span<'f, 's>> for DiagnosticSpan {
    #[inline]
    fn from(value: Span<'f, 's>) -> Self {
        // Easy case: if we're empty, then nothing to consume
        if value.is_empty() {
            return Self {
                file    : value.file.into(),
                source  : String::new(),
                range   : value.range,
                skipped : 0,
            }
        }
        let (start, end): (Bound<usize>, Bound<usize>) = (value.range.start_bound().cloned(), value.range.end_bound().cloned());

        // Find the first newline starting backwards from the start (and find how many lines we're skipping)
        let (skipped, line_start): (usize, Bound<usize>) = match start {
            Bound::Excluded(start) => 'res: {
                for (i, c) in value.source[..=start].grapheme_indices(true).rev() {
                    if c == "\n" {
                        break 'res (value.pos_of(i + 1).line, Bound::Included(i + 1));
                    }
                }
                (0, Bound::Unbounded)
            },
            Bound::Included(start) => 'res: {
                for (i, c) in value.source[..start].grapheme_indices(true).rev() {
                    if c == "\n" {
                        break 'res (value.pos_of(i + 1).line, Bound::Included(i + 1));
                    }
                }
                (0, Bound::Unbounded)
            },
            Bound::Unbounded => (0, Bound::Unbounded),
        };
        // Next, first newline starting forwards from the endline
        let line_end: Bound<usize> = match end {
            Bound::Excluded(end) => 'res: {
                if end > 0 {
                    for (i, c) in value.source[end - 1..].grapheme_indices(true) {
                        if c == "\n" {
                            break 'res Bound::Included(end + i);
                        }
                    }
                }
                Bound::Unbounded
            },
            Bound::Included(end) => 'res: {
                for (i, c) in value.source[end..].grapheme_indices(true) {
                    if c == "\n" {
                        break 'res Bound::Included(end + i);
                    }
                }
                Bound::Unbounded
            },
            Bound::Unbounded => Bound::Unbounded,
        };

        // Construct a range that is scaled to the only captured lines
        let scaled_start: Bound<usize> = match (start, line_start) {
            (Bound::Excluded(bound), Bound::Excluded(line_bound)) => Bound::Excluded(bound - line_bound),
            (Bound::Bounded(bound), SpanBound::Unbounded)           => SpanBound::Bounded(bound),
            (Bound::Unbounded, _)                                   => SpanBound::Unbounded,
        };
        let scaled_end: Bound<usize> = match (end, line_start) {
            (Bound::Bounded(bound), SpanBound::Bounded(line_bound)) => SpanBound::Bounded(bound - line_bound),
            (Bound::Bounded(bound), SpanBound::Unbounded)           => SpanBound::Bounded(bound),
            (Bound::Unbounded, _)                                   => SpanBound::Unbounded,
        };

        // Extract that piece of text and return ourselves
        Self {
            file   : value.file.into(),
            source : SpanRange::new(line_start, line_end).slice(value.source).into(),
            range  : SpanRange::new(scaled_start, scaled_end),
            skipped,
        }
    }
}

impl AsRef<DiagnosticSpan> for DiagnosticSpan {
    #[inline]
    fn as_ref(&self) -> &DiagnosticSpan { self }
}
impl AsMut<DiagnosticSpan> for DiagnosticSpan {
    #[inline]
    fn as_mut(&mut self) -> &mut DiagnosticSpan { self }
}
impl From<&DiagnosticSpan> for DiagnosticSpan {
    #[inline]
    fn from(value: &DiagnosticSpan) -> Self { value.clone() }
}
impl From<&mut DiagnosticSpan> for DiagnosticSpan {
    #[inline]
    fn from(value: &mut DiagnosticSpan) -> Self { value.clone() }
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
pub struct Diagnostic {
    /// The message to show
    message : String,
    /// Some code identifier for distinguishing errors machine-wise.
    code    : Option<String>,
    /// The in-diagnostic note to display, if any.
    remark  : Option<String>,
    /// The span that relates this message to the source text.
    span    : DiagnosticSpan,
    /// Anything kind-specific.
    kind    : DiagnosticSpecific,
    /// Any other diagnostics to print in succession after this one
    sub     : Vec<Self>,
}

impl Diagnostic {
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
    pub fn error(message: impl Into<String>, span: impl Into<DiagnosticSpan>) -> Self {
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
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let diag: Diagnostic = Diagnostic::warn("An example warning.", Span::new("<example>", "Example"));
    /// ```
    #[inline]
    pub fn warn(message: impl Into<String>, span: impl Into<DiagnosticSpan>) -> Self {
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
    /// use ast_toolkit::{Diagnostic, Span};
    /// 
    /// let diag: Diagnostic = Diagnostic::note("An example note.", Span::new("<example>", "Example"));
    /// ```
    #[inline]
    pub fn note(message: impl Into<String>, span: impl Into<DiagnosticSpan>) -> Self {
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
    pub fn suggestion(message: impl Into<String>, span: impl Into<DiagnosticSpan>, replacement: impl Into<String>) -> Self {
        Self {
            message : message.into(),
            code    : None,
            remark  : None,
            span    : span.into(),
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
    /// let span = Span::from_idx("<example>", "pub sttaic TEST = 42;", 4, 9);
    /// Diagnostic::error("Unknown keyword 'sttaic'", span)
    ///     .add(Diagnostic::suggestion("Try 'static'", span, "static"))
    ///     .emit();
    /// ```
    #[inline]
    pub fn add(mut self, diagnostic: impl Into<Diagnostic>) -> Self {
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
    /// let span1 = Span::from_idx("<example>", source, 0, 2);
    /// let span2 = Span::from_idx("<example>", source, 11, 14);
    /// Diagnostic::error("Unknown keyword 'pbu'", span1)
    ///     .add_error("'TEST' is therefore not publicly accessible", span2)
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_error(mut self, message: impl Into<String>, span: impl Into<DiagnosticSpan>) -> Self {
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
    /// let span1 = Span::from_idx("<example>", source, 0, 2);
    /// let span2 = Span::from_idx("<example>", source, 11, 14);
    /// Diagnostic::error("Unknown keyword 'pbu'", span1)
    ///     .add_warn("Statics are conventionally spelled using full-caps", span2)
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_warn(mut self, message: impl Into<String>, span: impl Into<DiagnosticSpan>) -> Self {
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
    /// let span1 = Span::from_idx("<example>", source, 37, 48);
    /// let span2 = Span::from_idx("<example>", source, 11, 14);
    /// Diagnostic::error("Cannot compare integer with boolean", span1)
    ///     .add_note("TEST defined here", span2)
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_note(mut self, message: impl Into<String>, span: impl Into<DiagnosticSpan>) -> Self {
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
    /// let span = Span::from_idx("<example>", "pub sttaic TEST = 42;", 4, 9);
    /// Diagnostic::error("Unknown keyword 'sttaic'", span)
    ///     .add_suggestion("Try 'static'", span, "static")
    ///     .emit();
    /// ```
    #[inline]
    pub fn add_suggestion(mut self, message: impl Into<String>, span: impl Into<DiagnosticSpan>, code: impl Into<String>) -> Self {
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
    /// let span = Span::from_idx("<example>", "Hello, World!", 7, 11);
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
    /// let span = Span::from_idx("<example>", "Hello, World!", 7, 11);
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
    /// let span = Span::from_idx("<example>", "Hello, World!", 7, 11);
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
    /// let span = Span::from_idx("<example>", "Hello, World!", 7, 11);
    /// assert_eq!(Diagnostic::error("An example error", span).span(), &span);
    /// ```
    #[inline]
    pub fn span(&self) -> &DiagnosticSpan { &self.span }

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
    /// let span = Span::from_pos("<example>", "Hello, World!", Position::new1(1, 1), Position::new1(1, 5));
    /// assert_eq!(Diagnostic::error("An example error", span).text(), "Hello");
    /// 
    /// let span = Span::from_idx("<example>", "Hello, World!", 7, 11);
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
    /// let span = Span::from_idx("<example>", "Hello, World!", 7, 11);
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
            let (source, accent_range): (Cow<str>, SpanRange) = match &self.kind {
                DiagnosticSpecific::Suggestion { replace } => {
                    // First: construct a new source
                    // NOTE: We can call start() because we ensure the span is never empty
                    let mut new_source: String = String::new();
                    if let SpanBound::Bounded(bound) = self.span.range.start() {
                        new_source.push_str(&self.span.source[..bound]);
                    }
                    new_source.push_str(replace);
                    if let SpanBound::Bounded(bound) = self.span.range.end() {
                        // Note we do the skip to exclude the bound position itself over a grapheme, which is inclusive
                        new_source.push_str(&self.span.source[bound..].graphemes(true).skip(1).collect::<String>());
                    }

                    // Then: compute the new range of the source
                    let new_range: SpanRange = match self.span.range.range() {
                        (SpanBound::Bounded(start), SpanBound::Bounded(_)) => SpanRange::Range(SpanBound::Bounded(start), SpanBound::Bounded(start + replace.len() - 1)),
                        (SpanBound::Bounded(start), SpanBound::Unbounded)  => SpanRange::Range(SpanBound::Bounded(start), SpanBound::Unbounded),
                        (SpanBound::Unbounded, SpanBound::Bounded(_))      => SpanRange::Range(SpanBound::Unbounded, SpanBound::Bounded(replace.len() - 1)),
                        (SpanBound::Unbounded, SpanBound::Unbounded)       => SpanRange::Range(SpanBound::Unbounded, SpanBound::Unbounded),
                    };

                    // Alright, return the new things
                    (Cow::Owned(new_source), new_range)
                },

                DiagnosticSpecific::Error | DiagnosticSpecific::Warning | DiagnosticSpecific::Note => (Cow::Borrowed(&self.span.source), self.span.range),
            };

            // Write the source stuff
            let max_line: usize = n_digits!(1 + self.span.skipped + source.chars().filter(|c| *c == '\n').count());
            // NOTE: We can safely unwrap because we assert the range is not empty
            emit_diagnostic_source_pos(writer, max_line, &self.span.file, self.span.start().unwrap())?;
            emit_diagnostic_source_lines(writer, &accent_colour, max_line, self.span.skipped + 1, source.as_ref(), accent_range, self.remark.as_ref().map(|r| r.as_str()))?;
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

impl AsRef<Diagnostic> for Diagnostic {
    #[inline]
    fn as_ref(&self) -> &Self { self }
}
impl AsMut<Diagnostic> for Diagnostic {
    #[inline]
    fn as_mut(&mut self) -> &mut Self { self }
}
impl From<&Diagnostic> for Diagnostic {
    #[inline]
    fn from(value: &Diagnostic) -> Self { value.clone() }
}
impl From<&mut Diagnostic> for Diagnostic {
    #[inline]
    fn from(value: &mut Diagnostic) -> Self { value.clone() }
}
