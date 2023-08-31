//  NOM.rs
//    by Lut99
// 
//  Created:
//    31 Aug 2023, 21:17:55
//  Last edited:
//    31 Aug 2023, 23:36:26
//  Auto updated?
//    Yes
// 
//  Description:
//!   Implements things useful for `nom`-ASTs specifically.
//!   
//!   Most notable, this module adds:
//!   - The [`NomError`]-struct, which is a drop-in to pretty-print [`nom`]-
//!     errors using [`Diagnostic`]s.
// 

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use enum_debug::EnumDebug;
use nom::Needed;
use nom::error::{ContextError, ErrorKind, FromExternalError, ParseError};

use crate::diagnostic::Diagnostic;
use crate::span::{Span, Spanning};


/***** AUXILLARY *****/
/// Formatter that writes a character quite readably to stdout.
/// 
/// # Example
/// ```rust
/// use ast_toolkit::nom::CharFormatter;
/// 
/// assert_eq!(CharFormatter('A').to_string(), "A");
/// assert_eq!(CharFormatter('d').to_string(), "d");
/// assert_eq!(CharFormatter('/').to_string(), "/");
/// assert_eq!(CharFormatter('ÿ').to_string(), "ÿ");
/// assert_eq!(CharFormatter(' ').to_string(), " ");
/// assert_eq!(CharFormatter('\n').to_string(), "\\n");
/// assert_eq!(CharFormatter('\r').to_string(), "\\r");
/// assert_eq!(CharFormatter('\t').to_string(), "\\t");
/// assert_eq!(CharFormatter('\0').to_string(), "\\0");
/// ```
#[derive(Debug)]
pub struct CharFormatter(char);
impl Display for CharFormatter {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "{:#?}", self.0)
    }
}



/// Specializes the [`NomError`] to a particular variant.
#[derive(Debug, EnumDebug)]
pub enum NomErrorVariant<'e, I> {
    /// It's a simple error from an unexpected [`char`].
    Char(I, char),
    /// It's a simple error from an [`ErrorKind`].
    ErrorKind(I, ErrorKind),
    /// It's an error with an external error attached to it.
    FromExternalError(I, Box<dyn 'e + Error>),

    /// It's an error that accumulates error contexts.
    Stack(Vec<NomError<'e, I>>),
    /// It's an error that can be one of multiple errors.
    Branch(Vec<NomError<'e, I>>),
}

/// Defines what we need to know when the user [adds a context](nom::error::ContextError).
#[derive(Clone, Debug)]
pub struct Context<I> {
    /// The input span relating the context to some location
    pub input   : I,
    /// The context string describing what we're doing.
    pub context : String,
}
impl<I> Context<I> {
    /// Constructor for the Context.
    /// 
    /// # Arguments
    /// - `input`: The input [`Spanning`] that relates this context to the source.
    /// - `ctx`: The string describing the context.
    /// 
    /// # Returns
    /// A new instance of Self.
    #[inline]
    pub fn new(input: I, ctx: impl Into<String>) -> Self {
        Self {
            input,
            context : ctx.into(),
        }
    }
}





/***** LIBRARY *****/
/// Provides a drop-in error for use in [`nom`].
/// 
/// This error is meant to produce verbose, as-intuitive-as-possible errors using this crate's [`Diagnostic`]s.
/// This verbosity will mostly come when you, naturally, use a [`Spanning`](crate::span::Spanning)-type in it.
/// 
/// # Example
/// ```rust
/// todo!();
/// ```
#[derive(Debug)]
pub struct NomError<'e, I> {
    /// The specific variant of this error.
    pub variant : NomErrorVariant<'e, I>,
    /// Any context carried by this error.
    pub context : Option<Context<I>>,
}

impl<'e, I> NomError<'e, I> {
    /// Constructor for the NomError that initializes it from an [`ErrorKind`].
    /// 
    /// # Arguments
    /// - `input`: The input that relates this error to the source.
    /// - `kind`: The [`ErrorKind`] which roughly describes what went wrong.
    /// 
    /// # Returns
    /// A new NomError based on the given `kind`.
    #[inline]
    fn error_kind(input: I, kind: ErrorKind) -> Self {
        Self {
            variant : NomErrorVariant::ErrorKind(input, kind),
            context : None,
        }
    }

    /// Constructor for the NomError that initializes it from an unexpected [`char`]acter.
    /// 
    /// # Arguments
    /// - `input`: The input that relates this error to the source.
    /// - `c`: The [`char`] which was unexpected.
    /// 
    /// # Returns
    /// A new NomError based on the given `c`.
    #[inline]
    fn char(input: I, c: char) -> Self {
        Self {
            variant : NomErrorVariant::Char(input, c),
            context : None,
        }
    }

    /// Constructor for the NomError that initializes it from a conjunctive stack of errors.
    /// 
    /// # Arguments
    /// - `errs`: Some iterator enumerating all the errors we want to join.
    /// 
    /// # Returns
    /// A new NomError based on the given stack of `errs`.
    #[inline]
    fn stack(errs: impl IntoIterator<Item = Self>) -> Self {
        Self {
            variant : NomErrorVariant::Stack(errs.into_iter().collect()),
            context : None,
        }
    }

    /// Constructor for the NomError that initializes it from a disjunctive list of possible brach errors.
    /// 
    /// # Arguments
    /// - `errs`: Some iterator enumerating all the errors we want to branch.
    /// 
    /// # Returns
    /// A new NomError based on the given list of `errs`.
    #[inline]
    fn branch(errs: impl IntoIterator<Item = Self>) -> Self {
        Self {
            variant : NomErrorVariant::Branch(errs.into_iter().collect()),
            context : None,
        }
    }
}

impl<'e, I> Display for NomError<'e, I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match &self.variant {
            NomErrorVariant::Char(_, _)                => write!(f, "Unexpected character"),
            NomErrorVariant::ErrorKind(_, kind)        => write!(f, "An error of kind {kind:?} occurred"),
            NomErrorVariant::FromExternalError(_, err) => write!(f, "{err}"),

            // These are implemented for direct usage but aren't used in Diagnostics.
            NomErrorVariant::Stack(_)  => write!(f, "Multiple errors occurred"),
            NomErrorVariant::Branch(_) => write!(f, "One of multiple errors occurred"),
        }
    }
}

impl<'e, I: Debug> Error for NomError<'e, I> {}
impl<'e, I> ContextError<I> for NomError<'e, I> {
    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.context = Some(Context::new(input, ctx));
        other
    }
}
impl<'e, I: Clone, E: 'e + Error> FromExternalError<I, E> for NomError<'e, I> {
    #[inline]
    fn from_external_error(input: I, _kind: ErrorKind, e: E) -> Self {
        // Compute the full stack of errors if `e` has a source
        if let Some(source) = e.source() {
            // Compute a full stack; put the main error in there first
            let mut stack: Vec<NomError<I>> = vec![ Self { variant: NomErrorVariant::FromExternalError(input.clone(), Box::new(e)), context: None } ];

            // Add the sources while there are any
            let mut source: Option<&dyn Error> = Some(source);
            while let Some(err) = source {
                source = err.source();
                stack.push(Self { variant: NomErrorVariant::FromExternalError(input.clone(), Box::new(err)), context: None })
            }

            // Alright cool, now return
            Self {
                variant : NomErrorVariant::Stack(stack),
                context : None, // Some(Context::new(input, kind.description())),
            }
        } else {
            // Return it as a simple error
            Self {
                variant : NomErrorVariant::FromExternalError(input, Box::new(e)),
                context : None, // Some(Context::new(input, kind.description())),
            }
        }
    }
}
impl<'e, I> ParseError<I> for NomError<'e, I> {
    #[inline]
    fn from_error_kind(input: I, kind: ErrorKind) -> Self { Self::error_kind(input, kind) }
    #[inline]
    fn from_char(input: I, c: char) -> Self { Self::char(input, c) }

    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        match &mut other.variant {
            // First, merge stacks
            NomErrorVariant::Stack(other_errs) => {
                other_errs.push(Self::error_kind(input, kind));
                other
            },

            // The rest we can just pass as-is
            _ => Self::stack(vec![ other, Self::error_kind(input, kind) ]),
        }
    }
    fn or(mut self, mut other: Self) -> Self {
        match (&mut self.variant, &mut other.variant) {
            // First: let's merge branches
            (NomErrorVariant::Branch(self_errs), NomErrorVariant::Branch(other_errs)) => {
                self_errs.extend(other_errs.drain(..));
                self
            },

            // Next, handle branch extends
            (NomErrorVariant::Branch(self_errs), _) => {
                self_errs.push(other);
                self
            },
            (_, NomErrorVariant::Branch(other_errs)) => {
                other_errs.push(self);
                other
            },

            // Any other case simply composes into a branch
            (_, _) => Self::branch(vec![ self, other ]),
        }
    }
}

impl<'e, I: Spanning> From<NomError<'e, I>> for Diagnostic {
    /// Builds a [`Diagnostic`] tree from the given [`NomError`].
    /// 
    /// # Arguments
    /// - `value`: The [`NomError`] to build the tree out of.
    /// 
    /// # Returns
    /// A new [`Diagnostic`] tree based on the given error.
    /// 
    /// # Panics
    /// This function may panic if we were an empty [`Stack`](NomErrorVariant::Stack) or [`Branch`](NomErrorVariant::Branch).
    #[inline]
    #[track_caller]
    fn from(value: NomError<'e, I>) -> Self {
        // Build and match a Diagnostic out of this
        let mut diag: Diagnostic = match value.variant {
            NomErrorVariant::Char(input, c) => {
                // Prepare the span to use
                let span: Span = if let Some(start) = input.start_idx() {
                    Span::ranged(input.file(), input.source(), start..=start)
                } else {
                    Span::empty(input.file(), input.source())
                };

                // Build the error
                Diagnostic::error(format!("Unexpected character"), span)
                    .set_remark(format!("Expected character: {}", CharFormatter(c)))
            },

            NomErrorVariant::ErrorKind(input, kind) => Diagnostic::error(
                format!("Syntax error while parsing ({})", kind.description()),
                input,
            ),

            NomErrorVariant::FromExternalError(input, err) => Diagnostic::error(
                err.to_string(),
                input,
            ),



            NomErrorVariant::Stack(errs) => {
                // Loop over the errors to create the tree
                let mut diag: Option<Diagnostic> = None;
                for err in errs {
                    // Recursively create it
                    if let Some(d) = diag {
                        diag = Some(d.add(Self::from(err)));
                    } else {
                        diag = Some(Self::from(err));
                    }
                }

                // Then unpack the diagnostic
                match diag {
                    Some(diag) => diag,
                    None       => { panic!("Cannot convert NomError into a Diagnostic on an empty NomError::Stack"); },
                }
            },
            NomErrorVariant::Branch(errs) => {
                todo!();
            },
        };

        // Add the context and extra things if necessary
        if let Some(Context { input, context }) = value.context {
            diag = diag.add_note(
                format!("The above occurred while parsing {context}"),
                input,
            );
        }

        // Alrighty that's it!
        diag
    }
}
impl<'e, I: Spanning> From<nom::Err<NomError<'e, I>>> for Diagnostic {
    /// Builds a [`Diagnostic`] tree from the given [`nom::Err<NomError>`].
    /// 
    /// # Arguments
    /// - `value`: The [`nom::Err<NomError>`] to build the tree out of.
    /// 
    /// # Returns
    /// A new [`Diagnostic`] tree based on the given error.
    /// 
    /// # Panics
    /// This function may panic if were an empty [`Stack`](NomErrorVariant::Stack) or [`Branch`](NomErrorVariant::Branch).
    #[inline]
    #[track_caller]
    fn from(value: nom::Err<NomError<'e, I>>) -> Self {
        match value {
            nom::Err::Error(err)    => Diagnostic::from(err),
            nom::Err::Failure(err)  => Diagnostic::from(err),
            nom::Err::Incomplete(_) => Diagnostic::error(
                "Unexpected end-of-file",
                Span::empty("", ""),
            ),
        }
    }
}
