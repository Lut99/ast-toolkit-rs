//  DIAGNOSTIC.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:22:17
//  Last edited:
//    27 May 2024, 10:48:49
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the main [`Diagnostic`]-interface itself.
//

use std::fmt::Display;

use ast_toolkit_span::Span;

use crate::annotations::{Annotation, AnnotationHighlight, IntoAnnotation};
use crate::style::{Plain, Style};


/***** AUXILLARY *****/
/// The severity levels supported by the [`Diagnostic`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Level {
    /// Fatal errors.
    Error,
    /// Non-fatal warnings.
    Warning,
}



/// Trait that allows things (e.g., typical [`Error`](std::error::Error)s) to be translated into [`Diagnostic`]s.
///
/// # Generics
/// - `F`: The "from"-string type of any [`Span`](ast_toolkit_span::Span) passed to the resulting diagnostic.
/// - `S`: The "source"-string type of any [`Span`](ast_toolkit_span::Span) passed to the resulting diagnostic.
pub trait IntoDiagnostic<F, S> {
    /// Returns a [`Diagnostic`] out of `self`.
    ///
    /// # Returns
    /// A [`Diagnostic`] made out of `self`.
    fn into_diagnostic(self) -> Diagnostic<F, S>;
}





/***** LIBRARY *****/
/// Provides an interface for building complex diagnostic messages.
///
/// Most notably, the Diagnostic allows for typical compiler-style error messages on several
/// sevirity levels (errors and warnings).
#[derive(Debug)]
pub struct Diagnostic<F, S> {
    // Unique to the toplevel
    /// The severity of the error.
    pub level: Level,
    /// Some error code that users might recognize elsewhere.
    pub code:  Option<String>,
    /// The style to apply while rendering.
    pub style: Box<dyn Style>,

    // Annotation
    /// The span referred to by the main message.
    pub main:   AnnotationHighlight<F, S>,
    /// Any other annotations.
    pub annots: Vec<Annotation<F, S>>,
}

// Constructors
impl<F, S> Diagnostic<F, S> {
    /// Generic constructor for a Diagnostic that takes both a severity level and a message.
    ///
    /// # Arguments
    /// - `level`: The severity [`Level`] of the main message.
    /// - `msg`: The main message to display on top of the snippet.
    /// - `span`: Some [`Span`] that is the source text to display.
    ///
    /// # Returns
    /// A new Diagnostic with the given severity `level` and message.
    #[inline]
    pub fn new(level: Level, msg: impl Display, span: Span<F, S>) -> Self {
        Self { level, code: None, style: Box::new(Plain), main: AnnotationHighlight { msg: msg.to_string(), span }, annots: vec![] }
    }

    /// Constructor for a Diagnostic that will create it as an error message.
    ///
    /// # Arguments
    /// - `msg`: The main message to display on top of the snippet.
    /// - `span`: Some [`Span`] that is the source text to display.
    ///
    /// # Returns
    /// A new Diagnostic with [`Level::Error`] severity and the given message.
    #[inline]
    pub fn error(msg: impl Display, span: Span<F, S>) -> Self { Self::new(Level::Error, msg, span) }

    /// Constructor for a Diagnostic that will create it as a warning message.
    ///
    /// # Arguments
    /// - `msg`: The main message to display on top of the snippet.
    /// - `span`: Some [`Span`] that is the source text to display.
    ///
    /// # Returns
    /// A new Diagnostic with [`Level::Warning`] severity and the given message.
    #[inline]
    pub fn warn(msg: impl Display, span: Span<F, S>) -> Self { Self::new(Level::Warning, msg, span) }



    /// Generic constructor for a Diagnostic that takes both a severity level and a style.
    ///
    /// # Arguments
    /// - `level`: The severity [`Level`] of the main message.
    /// - `msg`: The main message to display on top of the snippet.
    /// - `span`: Some [`Span`] that is the source text to display.
    /// - `style`: Some [`Style`] that determines the colour palette for this Diagnostic's renderings.
    ///
    /// # Returns
    /// A new Diagnostic with the given severity `level`, message and initialized `style`.
    #[inline]
    pub fn with_style(level: Level, msg: impl Display, span: Span<F, S>, style: impl 'static + Style) -> Self {
        Self { level, code: None, style: Box::new(style), main: AnnotationHighlight { msg: msg.to_string(), span }, annots: vec![] }
    }

    /// Constructor for a Diagnostic that will create it as an error message and with a given style.
    ///
    /// # Arguments
    /// - `msg`: The main message to display on top of the snippet.
    /// - `span`: Some [`Span`] that is the source text to display.
    /// - `style`: Some [`Style`] that determines the colour palette for this Diagnostic's renderings.
    ///
    /// # Returns
    /// A new Diagnostic with [`Level::Error`] severity, the given message and initialized `style`.
    #[inline]
    pub fn error_with_style(msg: impl Display, span: Span<F, S>, style: impl 'static + Style) -> Self {
        Self::with_style(Level::Error, msg, span, style)
    }

    /// Constructor for a Diagnostic that will create it as a warning message and with a given style.
    ///
    /// # Arguments
    /// - `msg`: The main message to display on top of the snippet.
    /// - `span`: Some [`Span`] that is the source text to display.
    /// - `style`: Some [`Style`] that determines the colour palette for this Diagnostic's renderings.
    ///
    /// # Returns
    /// A new Diagnostic with [`Level::Warning`] severity, the given message and initialized `style`.
    #[inline]
    pub fn warn_with_style(msg: impl Display, span: Span<F, S>, style: impl 'static + Style) -> Self {
        Self::with_style(Level::Warning, msg, span, style)
    }
}

// Factory methods
impl<F, S> Diagnostic<F, S> {
    /// Changes the severity level of this Diagnostic.
    ///
    /// The severity level determines whether this Diagnostic emits a [warning](Level::Warning) or an [error](Level::Error).
    ///
    /// # Arguments
    /// - `level`: The new [`Level`] to assign to this Diagnostic.
    ///
    /// # Returns
    /// Self with the given `level`.
    #[inline]
    pub fn level_mut(&mut self, level: Level) -> &mut Self {
        self.level = level;
        self
    }

    /// Sets a _code_ for this Diagnostic.
    ///
    /// A code is used to give the user something shorthand to refer to this specific warning or error.
    ///
    /// This may be useful if the user can disable it with that specific code, for example.
    ///
    /// # Arguments
    /// - `code`: Something that can be rendered as the new code. Give [`None`] to remove any previously assigned one.
    ///
    /// # Returns
    /// Self with the given `code`.
    #[inline]
    pub fn code_mut(&mut self, code: Option<impl Display>) -> &mut Self {
        self.code = code.map(|c| c.to_string());
        self
    }

    /// Changes the assigned style of this Diagnostic.
    ///
    /// This is used to change the colours used when rendering the Diagnostic.
    ///
    /// # Arguments
    /// - `style`: Some [`Style`] that determines the colour pattern of renderings of this Diagnostic.
    ///
    /// # Returns
    /// Self with the given `style`.
    #[inline]
    pub fn style_mut(&mut self, style: impl 'static + Style) -> &mut Self {
        self.style = Box::new(style);
        self
    }



    /// Changes the main message of the Diagnostic.
    ///
    /// This is the message that is displayed at the top of the source text snippet(s).
    ///
    /// # Arguments
    /// - `msg`: The message to display at the top of the diagnostic.
    ///
    /// # Returns
    /// Self with the given main message.
    #[inline]
    pub fn message_mut(&mut self, msg: impl Display) -> &mut Self {
        self.main.msg = msg.to_string();
        self
    }

    /// Changes the main message span of the Diagnostic.
    ///
    /// This is the part of the source text that is highlighted as the warning or error in question.
    ///
    /// # Arguments
    /// - `span`: The [`Span`]ned source text to highlight.
    ///
    /// # Returns
    /// Self with the given main `span`.
    pub fn span_mut(&mut self, span: Span<F, S>) -> &mut Self {
        self.main.span = span;
        self
    }



    /// Adds an [`Annotation`] of any sort to this Diagnostic.
    ///
    /// This is some additional highlighted part of the source text, or some note.
    ///
    /// # Arguments
    /// - `annot`: The [`Annotation`]-like to add.
    ///
    /// # Returns
    /// Self with the given annotation added.
    pub fn annotation_mut(&mut self, annot: impl IntoAnnotation<F, S>) -> &mut Self {
        self.annots.push(annot.into_annotation());
        self
    }
}
impl<F, S> Diagnostic<F, S> {
    /// Changes the severity level of this Diagnostic.
    ///
    /// The severity level determines whether this Diagnostic emits a [warning](Level::Warning) or an [error](Level::Error).
    ///
    /// # Arguments
    /// - `level`: The new [`Level`] to assign to this Diagnostic.
    ///
    /// # Returns
    /// Self with the given `level`.
    #[inline]
    pub fn level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }

    /// Sets a _code_ for this Diagnostic.
    ///
    /// A code is used to give the user something shorthand to refer to this specific warning or error.
    ///
    /// This may be useful if the user can disable it with that specific code, for example.
    ///
    /// # Arguments
    /// - `code`: Something that can be rendered as the new code. Give [`None`] to remove any previously assigned one.
    ///
    /// # Returns
    /// Self with the given `code`.
    #[inline]
    pub fn code(mut self, code: Option<impl Display>) -> Self {
        self.code = code.map(|c| c.to_string());
        self
    }

    /// Changes the assigned style of this Diagnostic.
    ///
    /// This is used to change the colours used when rendering the Diagnostic.
    ///
    /// # Arguments
    /// - `style`: Some [`Style`] that determines the colour pattern of renderings of this Diagnostic.
    ///
    /// # Returns
    /// Self with the given `style`.
    #[inline]
    pub fn style(mut self, style: impl 'static + Style) -> Self {
        self.style = Box::new(style);
        self
    }



    /// Changes the main message of the Diagnostic.
    ///
    /// This is the message that is displayed at the top of the source text snippet(s).
    ///
    /// # Arguments
    /// - `msg`: The message to display at the top of the diagnostic.
    ///
    /// # Returns
    /// Self with the given main message.
    #[inline]
    pub fn message(mut self, msg: impl Display) -> Self {
        self.main.msg = msg.to_string();
        self
    }

    /// Changes the main message span of the Diagnostic.
    ///
    /// This is the part of the source text that is highlighted as the warning or error in question.
    ///
    /// # Arguments
    /// - `span`: The [`Span`]ned source text to highlight.
    ///
    /// # Returns
    /// Self with the given main `span`.
    pub fn span(mut self, span: Span<F, S>) -> Self {
        self.main.span = span;
        self
    }



    /// Adds an [`Annotation`] of any sort to this Diagnostic.
    ///
    /// This is some additional highlighted part of the source text, or some note.
    ///
    /// # Arguments
    /// - `annot`: The [`Annotation`]-like to add.
    ///
    /// # Returns
    /// Self with the given annotation added.
    pub fn annotation(mut self, annot: impl IntoAnnotation<F, S>) -> Self {
        self.annots.push(annot.into_annotation());
        self
    }
}

// From
impl<F, S> IntoDiagnostic<F, S> for Diagnostic<F, S> {
    #[inline]
    fn into_diagnostic(self) -> Diagnostic<F, S> { self }
}
