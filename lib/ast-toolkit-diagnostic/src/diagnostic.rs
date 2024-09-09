//  DIAGNOSTIC.rs
//    by Lut99
//
//  Created:
//    24 May 2024, 17:22:17
//  Last edited:
//    09 Sep 2024, 14:34:37
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the main [`Diagnostic`]-interface itself.
//

use crate::annotations::Annotation;
use crate::style::{Plain, Style};


/***** AUXILLARY *****/
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
pub struct Diagnostic<F, S> {
    /// The style to apply while rendering.
    pub style:  Box<dyn Style>,
    /// The things to display in this Diagnostic.
    pub annots: Vec<Annotation<F, S>>,
}

// Constructors
impl<F, S> Diagnostic<F, S> {
    /// Constructor for a Diagnostic.
    ///
    /// # Returns
    /// A new Diagnostic without any annotations yet.
    #[inline]
    pub fn new() -> Self { Self { style: Box::new(Plain), annots: Vec::new() } }
    /// Constructor for a Diagnostic with space pre-allocated for a given number of annotations.
    ///
    /// # Arguments
    /// - `capacity`: The (minimum) number of annotations to allocate for.
    ///
    /// # Returns
    /// A new Diagnostic without any annotations yet but with space for _at least_ `capacity` annotations.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self { Self { style: Box::new(Plain), annots: Vec::with_capacity(capacity) } }

    /// Constructor for a Diagnostic with a custom style.
    ///
    /// # Arguments
    /// - `style`: Some [`Style`] that determines the colour palette for this Diagnostic's renderings.
    ///
    /// # Returns
    /// A new Diagnostic without any annotations yet but with the given `style`.
    #[inline]
    pub fn with_style(style: impl 'static + Style) -> Self { Self { style: Box::new(style), annots: Vec::new() } }
    /// Constructor for a Diagnostic with a custom style and space pre-allocated for a given number of annotations.
    ///
    /// # Arguments
    /// - `style`: Some [`Style`] that determines the colour palette for this Diagnostic's renderings.
    /// - `capacity`: The (minimum) number of annotations to allocate for.
    ///
    /// # Returns
    /// A new Diagnostic without any annotations yet but with the given `style` and space for _at least_ `capacity` annotations.
    #[inline]
    pub fn with_style_and_capacity(style: impl 'static + Style, capacity: usize) -> Self {
        Self { style: Box::new(style), annots: Vec::with_capacity(capacity) }
    }
}

// Factory methods
impl<F, S> Diagnostic<F, S> {
    /// Changes the assigned style of this Diagnostic.
    ///
    /// This is used to change the colours used when rendering the Diagnostic.
    ///
    /// This overload takes `self` by mutable reference, returning a mutable reference for
    /// chaining. If you're creating the Diagnostic for direct in-place consumption, consider using
    /// [`Self::style()`](Diagnostic::style()) instead.
    ///
    /// # Arguments
    /// - `style`: Some [`Style`] that determines the colour pattern of renderings of this Diagnostic.
    ///
    /// # Returns
    /// Self with the given `style`.
    #[inline]
    pub fn mut_style(&mut self, style: impl 'static + Style) -> &mut Self {
        self.style = Box::new(style);
        self
    }

    /// Adds an [`Annotation`] of any sort to this Diagnostic.
    ///
    /// This is some additional highlighted part of the source text, or some note.
    ///
    /// This overload takes `self` by mutable reference, returning a mutable reference for
    /// chaining. If you're creating the Diagnostic for direct in-place consumption, consider using
    /// [`Self::annotation()`](Diagnostic::annotation()) instead.
    ///
    /// # Arguments
    /// - `annot`: The [`Annotation`]-like to add.
    ///
    /// # Returns
    /// Self with the given annotation added.
    pub fn mut_annotation(&mut self, annot: impl Into<Annotation<F, S>>) -> &mut Self {
        self.annots.push(annot.into());
        self
    }
}
impl<F, S> Diagnostic<F, S> {
    /// Changes the assigned style of this Diagnostic.
    ///
    /// This is used to change the colours used when rendering the Diagnostic.
    ///
    /// This overload takes `self` by ownership, returning it for chaining. If you are in use-case
    ///
    /// chaining. If you're creating the Diagnostic for direct in-place consumption, consider using
    /// [`Self::style()`](Diagnostic::style()) instead.
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

    /// Adds an [`Annotation`] of any sort to this Diagnostic.
    ///
    /// This is some additional highlighted part of the source text, or some note.
    ///
    /// # Arguments
    /// - `annot`: The [`Annotation`]-like to add.
    ///
    /// # Returns
    /// Self with the given annotation added.
    pub fn annotation(mut self, annot: impl Into<Annotation<F, S>>) -> Self {
        self.annots.push(annot.into());
        self
    }
}

// From
impl<F, S> IntoDiagnostic<F, S> for Diagnostic<F, S> {
    #[inline]
    fn into_diagnostic(self) -> Diagnostic<F, S> { self }
}
