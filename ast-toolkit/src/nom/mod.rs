//  MOD.rs
//    by Lut99
// 
//  Created:
//    31 Aug 2023, 21:17:55
//  Last edited:
//    10 Sep 2023, 11:30:22
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

#[cfg(feature = "nom-combinators")]
pub mod multi;


use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FResult};

use enum_debug::EnumDebug;
use nom::error::{ContextError, FromExternalError, ParseError};
use num_traits::AsPrimitive;

use crate::diagnostic::Diagnostic;
use crate::span::{Combining, Span, Spanning, SpanningExt};


/***** HELPER MACROS *****/
/// Returns either a character debug-style printed if given [`Some(...)`] or `end of input` if given [`None`].
macro_rules! expected_char {
    ($c:expr) => {
        if let Some(c) = $c { format!("{c:?}") } else { "end of input".into() }
    };
}





/***** HELPER FUNCTIONS *****/
/// Extracts the starting character of the given input.
/// 
/// # Arguments
/// - `input`: The [`Spanning`] object to try and get the character from.
/// 
/// # Returns
/// The character at the start position, or [`None`] if the input is empty.
#[inline]
fn get_input_char<I: ?Sized + Spanning>(input: &I) -> Option<char> {
    if let Some(start) = input.start_idx() {
        input.source().chars().nth(start)
    } else {
        None
    }
}





/***** FORMATTER *****/
/// Implements [`Display`] for the [`ErrorKind`].
#[derive(Debug)]
pub struct ErrorKindFormatter<'e, 'i, I: ?Sized> {
    err   : &'e ErrorKind,
    input : &'i I,
}
impl<'e, 'i, I: ?Sized + Spanning + SpanningExt> Display for ErrorKindFormatter<'e, 'i, I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use ErrorKind::*;
        match self.err {
            Alpha            => write!(f, "Syntax error: Expected alphabetic character, got {}", expected_char!(get_input_char(self.input))),
            AlphaNumeric     => write!(f, "Syntax error: Expected alphanumeric character, got {}", expected_char!(get_input_char(self.input))),
            Complete         => write!(f, "Incomplete input"),
            Count(_)         => write!(f, "Expected more repetitions"),
            CrLf             => write!(f, "Syntax error: Expected Windows-style line ending (CRLF), got {}", expected_char!(get_input_char(self.input))),
            Digit            => write!(f, "Syntax error: Expected digit, got {}", expected_char!(get_input_char(self.input))),
            Eof              => write!(f, "Unexpected end of input"),                                                                                  // NOTE: Could get more verbose input but that's essentially which parser is being run
            Escaped          |
            EscapedTransform => write!(f, "Syntax error: Incorrect escape sequence"),
            Fail             => write!(f, "Unexpected error occurred"),
            Float            => write!(f, "Syntax error: Incorrect floating-point number"),
            HexDigit         => write!(f, "Syntax error: Expected hexadecimal digit, got {}", expected_char!(get_input_char(self.input))),
            IsA(_)           => write!(f, "Expected pattern"),                                                                                         // TODO: Can get more verbose if we read parser input
            IsNot(_)         => write!(f, "Expected anything else than {}", expected_char!(get_input_char(self.input))),                                   // TODO: Can get more verbose if we read parser input
            Many0            |                                                                                                                       // NOTE: Don't need to catch like Many1 since it never propagates and can thus never be in a stack other than the end
            Many0Count       => { panic!("Parser got stuck in an infinite loop at {}", if let (Some(start), Some(end)) = (self.input.start(), self.input.end()) { format!("{start}-{end}") } else { "<unknown>".into() }); },
            MapOpt           => write!(f, "Failed to process parsed result"),
            MultiSpace       => write!(f, "Syntax error: Expected whitespace, got {}", expected_char!(get_input_char(self.input))),
            NoneOf(_)        => write!(f, "Syntax error: Expected anything else than {}", expected_char!(get_input_char(self.input))),                     // TODO: Can get more verbose if we read parser input
            Not              => todo!(),
            OctDigit         => write!(f, "Syntax error: Expected octet digit, got {}", expected_char!(get_input_char(self.input))),
            OneOf(_)         => write!(f, "Syntax error: Expected character, got {}", expected_char!(get_input_char(self.input))),                         // TODO: Can get more verbose if we read parser input
            Satisfy          => write!(f, "Syntax error: Encountered unexpected character {}", expected_char!(get_input_char(self.input))),
            SeparatedList    => { panic!("Parser got stuck in an infinite loop at {}", if let (Some(start), Some(end)) = (self.input.start(), self.input.end()) { format!("{start}-{end}") } else { "<unknown>".into() }); },
            Space            => write!(f, "Syntax error: Expected non-newline whitespace, got {}", expected_char!(get_input_char(self.input))),
            Tag(_)           => write!(f, "Expected a sequence"),                                                                                      // TODO: Can get more verbose if we read parser input
            TagBits(_)       => write!(f, "Expected a sequence of bits"),                                                                              // TODO: Can get more verbose if we read parser input
            TakeTill1        => todo!(),
            TakeUntil        => todo!(),
            TakeWhile1       => todo!(),
            TakeWhileMN      => todo!(),
            TooLarge         => { panic!("Data size reported by function is too large"); },
            Verify           => write!(f, "Encountered illegal sequence"),

            // These treated elsewhere and get default messages
            Alt         |
            Char        |
            Many1       |
            Many1Count  |
            ManyMN      |
            ManyTill    |
            MapRes      |
            Permutation => write!(f, "Syntax error: Unexpected character {}", expected_char!(get_input_char(self.input))),
        }
    }
}





/***** AUXILLARY *****/
/// Specializes the [`NomError`] to a particular variant.
/// 
/// # Example
/// ```rust
/// use nom::error::ErrorKind;
/// use ast_toolkit::Span;
/// use ast_toolkit::nom::{NomError, NomErrorKind};
/// 
/// let input: Span = Span::new("<example>", "Hello, world!");
/// 
/// let err1 = NomError::error_kind(input, ErrorKind::Tag);
/// assert_eq!(err1.kind, NomErrorKind::ErrorKind(input, ErrorKind::Tag));
/// 
/// let err2 = NomError::char(input, 'c');
/// assert_eq!(err2.kind, NomErrorKind::Char(input, 'c'));
/// 
/// let err3 = NomError::external_error(input, ErrorKind::Many0, String::from_utf8(vec![ 0, 159 ]).unwrap_err());
/// assert_eq!(err3.kind, NomErrorKind::ExternalError(input, ErrorKind::Many0, "invalid utf-8 sequence of 1 bytes from index 1".into()));
/// 
/// let err4 = NomError::stack(vec![ err1.clone(), err2.clone(), err3.clone() ]);
/// assert_eq!(err4.kind, NomErrorKind::Stack(vec![ err1.clone(), err2.clone(), err3.clone() ]));
/// 
/// let err5 = NomError::branch(vec![ err1.clone(), err2.clone(), err3.clone() ]);
/// assert_eq!(err5.kind, NomErrorKind::Branch(vec![ err1, err2, err3 ]));
/// ```
#[derive(Clone, Debug, EnumDebug, Eq, PartialEq)]
pub enum NomErrorKind<I> {
    /// It's a simple error from an unexpected [`char`].
    Char(I, char),
    /// It's a simple error from an [`ErrorKind`].
    ErrorKind(I, ErrorKind),
    /// It's an error with an external error attached to it.
    ExternalError(I, ErrorKind, String),

    /// It's an error that accumulates error contexts.
    Stack(Vec<NomError<I>>),
    /// It's an error that can be one of multiple errors.
    Branch(Vec<NomError<I>>),
}

/// Defines what we need to know when the user [adds a context](nom::error::ContextError).
/// 
/// This struct is mostly for internal use in [`NomError`], but you can also use it yourself while at it.
/// 
/// # Example
/// ```rust
/// use ast_toolkit::nom::Context;
/// 
/// struct SomethingWithContext<I> {
///     pub something : String,
///     pub context   : Context<I>,
/// }
/// 
/// let input: &str = "test";
/// assert_eq!(SomethingWithContext {
///     something : "Something!".into(),
///     context   : Context::new(input, "A context"),
/// }.context.context, "A context");
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
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
/// Shadows [`nom`]'s [`ErrorKind`](nom::error::ErrorKind) to add additional information for some kinds of errors.
#[derive(Clone, Debug, EnumDebug, Eq, Hash, PartialEq)]
pub enum ErrorKind {
    Alpha,
    AlphaNumeric,
    Alt,
    Char,
    Complete,
    Count(Option<(usize, usize)>),
    CrLf,
    Digit,
    Eof,
    Escaped,
    EscapedTransform,
    Fail,
    Float,
    HexDigit,
    IsA(Option<()>),
    IsNot(Option<()>),
    Many0,
    Many1,
    Many0Count,
    Many1Count,
    ManyMN,
    ManyTill,
    MapOpt,
    MapRes,
    MultiSpace,
    NoneOf(Option<()>),
    Not,
    OctDigit,
    OneOf(Option<()>),
    Permutation,
    Satisfy,
    SeparatedList,
    Space,
    Tag(Option<()>),
    TagBits(Option<()>),
    TakeTill1,
    TakeUntil,
    TakeWhile1,
    TakeWhileMN,
    TooLarge,
    Verify,
}
impl ErrorKind {
    /// Constructor for an [`ErrorKind::Count`] that initializes it with the additional context.
    /// 
    /// # Arguments
    /// - `got`: The number of repetitions parsed.
    /// - `expected`: The number of repetitions expected.
    /// 
    /// # Returns
    /// A new instance of Self that represents a Count error.
    #[inline]
    pub fn count(got: impl AsPrimitive<usize>, expected: impl AsPrimitive<usize>) -> Self {
        Self::Count(Some((got.as_(), expected.as_())))
    }



    /// Returns a formatter for the ErrorKind that implements [`Display`].
    /// 
    /// # Arguments
    /// - `input`: A [`Spanning`]-type that can provide context about what failed to be parsed.
    /// 
    /// # Returns
    /// A new [`ErrorKindFormatter`] that can be [`Display`]ed.
    pub fn display<'s, 'i, I: ?Sized>(&'s self, input: &'i I) -> ErrorKindFormatter<'s, 'i, I> {
        ErrorKindFormatter {
            err : self,
            input,
        }
    }
}
impl From<nom::error::ErrorKind> for ErrorKind {
    fn from(value: nom::error::ErrorKind) -> Self {
        use nom::error::ErrorKind::*;
        match value {
            Alpha            => Self::Alpha,
            AlphaNumeric     => Self::AlphaNumeric,
            Alt              => Self::Alt,
            Char             => Self::Char,
            Complete         => Self::Complete,
            Count            => Self::Count(None),
            CrLf             => Self::CrLf,
            Digit            => Self::Digit,
            Eof              => Self::Eof,
            Escaped          => Self::Escaped,
            EscapedTransform => Self::EscapedTransform,
            Fail             => Self::Fail,
            Float            => Self::Float,
            HexDigit         => Self::HexDigit,
            IsA              => Self::IsA(None),
            IsNot            => Self::IsNot(None),
            Many0            => Self::Many0,
            Many1            => Self::Many1,
            Many0Count       => Self::Many0Count,
            Many1Count       => Self::Many1Count,
            ManyMN           => Self::ManyMN,
            ManyTill         => Self::ManyTill,
            MapOpt           => Self::MapOpt,
            MapRes           => Self::MapRes,
            MultiSpace       => Self::MultiSpace,
            NoneOf           => Self::NoneOf(None),
            Not              => Self::Not,
            OctDigit         => Self::OctDigit,
            OneOf            => Self::OneOf(None),
            Permutation      => Self::Permutation,
            Satisfy          => Self::Satisfy,
            SeparatedList    => Self::SeparatedList,
            Space            => Self::Space,
            Tag              => Self::Tag(None),
            TagBits          => Self::TagBits(None),
            TakeTill1        => Self::TakeTill1,
            TakeUntil        => Self::TakeUntil,
            TakeWhile1       => Self::TakeWhile1,
            TakeWhileMN      => Self::TakeWhileMN,
            TooLarge         => Self::TooLarge,
            Verify           => Self::Verify,

            // These never occur so we can assume we can ingore them
            SeparatedNonEmptyList |
            LengthValue           |
            TagClosure            |
            LengthValueFn         |
            Switch                |
            RegexpMatch           |
            RegexpMatches         |
            RegexpCapture         |
            RegexpCaptures        |
            RegexpFind            |
            Fix                   |
            NonEmpty              => { unreachable!(); }
        }
    }
}



/// Provides a drop-in error for use in [`nom`].
/// 
/// This error is meant to produce verbose, as-intuitive-as-possible errors using this crate's [`Diagnostic`]s.
/// This verbosity will mostly come when you, naturally, use a [`Spanning`](crate::span::Spanning)-type in it.
/// 
/// # Example
/// ```rust
/// use nom::IResult;
/// use nom::bytes::complete as bc;
/// use ast_toolkit::{Diagnostic, NomError, Span};
/// 
/// type Input<'f, 's> = Span<'f, 's>;
/// type Output<'f, 's, O> = IResult<Input<'f, 's>, O, NomError<Input<'f, 's>>>;
/// 
/// fn parser<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, Span<'f, 's>> {
///     bc::tag("Hello, world!")(input)
/// }
/// 
/// let err: nom::Err<NomError<Span>> = parser(Span::new("<farewell>", "Goodbye, world!")).unwrap_err();
/// Diagnostic::from(err).emit();
/// ```
/// which should show you something like:
/// ```plain
/// error: Tag
///  --> <farewell>:1:1
///   |
/// 1 | Goodbye, world!
///   | ^~~~~~~~~~~~~~~
///   |
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NomError<I> {
    /// The specific variant of this error.
    pub kind    : NomErrorKind<I>,
    /// Any context carried by this error.
    pub context : Option<Context<I>>,
}

impl<I> NomError<I> {
    /// Constructor for the NomError that initializes it from an [`ErrorKind`].
    /// 
    /// # Arguments
    /// - `input`: The input that relates this error to the source.
    /// - `kind`: The [`ErrorKind`] which roughly describes what went wrong.
    /// 
    /// # Returns
    /// A new NomError based on the given `kind`.
    /// 
    /// # Example
    /// ```rust
    /// use nom::IResult;
    /// use nom::bytes::complete as bc;
    /// use ast_toolkit::{Diagnostic, NomError, Span};
    /// 
    /// type Input<'f, 's> = Span<'f, 's>;
    /// type Output<'f, 's, O> = IResult<Input<'f, 's>, O, NomError<Input<'f, 's>>>;
    /// 
    /// fn parser<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, Span<'f, 's>> {
    ///     bc::tag("Hello, world!")(input)
    /// }
    /// 
    /// let err: nom::Err<NomError<Span>> = parser(Span::new("<farewell>", "Goodbye, world!")).unwrap_err();
    /// Diagnostic::from(err).emit();
    /// ```
    /// which should show you something like:
    /// ```plain
    /// error: Tag
    ///  --> <farewell>:1:1
    ///   |
    /// 1 | Goodbye, world!
    ///   | ^~~~~~~~~~~~~~~
    ///   |
    /// ```
    #[inline]
    pub fn error_kind(input: I, kind: impl Into<ErrorKind>) -> Self {
        Self {
            kind : NomErrorKind::ErrorKind(input, kind.into()),
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
    /// 
    /// # Example
    /// ```rust
    /// use nom::IResult;
    /// use nom::{character::complete as cc, combinator as comb};
    /// use ast_toolkit::{Diagnostic, NomError, Span};
    /// 
    /// type Input<'f, 's> = Span<'f, 's>;
    /// type Output<'f, 's, O> = IResult<Input<'f, 's>, O, NomError<Input<'f, 's>>>;
    /// 
    /// fn parser<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, char> {
    ///     comb::cut(cc::char('H'))(input)
    /// }
    /// 
    /// let err: nom::Err<NomError<Span>> = parser(Span::new("<farewell>", "Goodbye, world!")).unwrap_err();
    /// Diagnostic::from(err).emit();
    /// ```
    /// which should show you something like:
    /// ```plain
    /// error: Syntax error: Expected 'H', got 'G'
    ///  --> <farewell>:1:1
    ///   |
    /// 1 | Goodbye, world!
    ///   | ^              
    ///   |
    /// ```
    #[inline]
    pub fn char(input: I, c: char) -> Self {
        Self {
            kind : NomErrorKind::Char(input, c),
            context : None,
        }
    }

    /// Constructor for the NomError that initializes it from an external error.
    /// 
    /// # Arguments
    /// - `input`: The input that relates this error to the source.
    /// - `kind`: Some additional [`nom::error::ErrorKind`] to contextualize this error.
    /// - `err`: The external [`Error`] to wrap..
    /// 
    /// # Returns
    /// A new NomError based on the given error.
    /// 
    /// # Example
    /// ```rust
    /// use std::str::FromStr as _;
    /// use nom::IResult;
    /// use nom::error::ErrorKind;
    /// use ast_toolkit::{Diagnostic, NomError, Span, SpanningExt as _};
    /// 
    /// type Input<'f, 's> = Span<'f, 's>;
    /// type Output<'f, 's, O> = IResult<Input<'f, 's>, O, NomError<Input<'f, 's>>>;
    /// 
    /// fn parser<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, i64> {
    ///     match i64::from_str(input.text()) {
    ///        Ok(val)  => Ok((Span::empty(input.file, input.source), val)),
    ///        Err(err) => Err(nom::Err::Error(NomError::external_error(input, ErrorKind::Fail, err))),
    ///    }
    /// }
    /// 
    /// let err: nom::Err<NomError<Span>> = parser(Span::new("<farewell>", "Goodbye, world!")).unwrap_err();
    /// Diagnostic::from(err).emit();
    /// ```
    /// which should show you something like:
    /// ```plain
    /// error: In Fail: invalid digit found in string
    ///  --> <farewell>:1:1
    ///   |
    /// 1 | Goodbye, world!
    ///   | ^~~~~~~~~~~~~~~
    ///   |
    /// ```
    #[inline]
    pub fn external_error(input: I, kind: impl Into<ErrorKind>, err: impl Error) -> Self {
        Self {
            kind : NomErrorKind::ExternalError(input, kind.into(), err.to_string()),
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
    /// 
    /// # Example
    /// ```rust
    /// use std::str::FromStr as _;
    /// use nom::IResult;
    /// use nom::error::ErrorKind;
    /// use nom::{character::complete as cc, multi};
    /// use ast_toolkit::{Diagnostic, NomError, Span, SpanningExt as _};
    /// 
    /// type Input<'f, 's> = Span<'f, 's>;
    /// type Output<'f, 's, O> = IResult<Input<'f, 's>, O, NomError<Input<'f, 's>>>;
    /// 
    /// fn parser<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, Vec<Span<'f, 's>>> {
    ///     multi::many1(cc::digit1)(input)
    /// }
    /// 
    /// let err: nom::Err<NomError<Span>> = parser(Span::new("<farewell>", "Goodbye, world!")).unwrap_err();
    /// Diagnostic::from(err).emit();
    /// ```
    /// which should show you something like:
    /// ```plain
    /// error: Digit
    ///  --> <farewell>:1:1
    ///   |
    /// 1 | Goodbye, world!
    ///   | ^~~~~~~~~~~~~~~
    ///   |
    /// error: Many1
    ///  --> <farewell>:1:1
    ///   |
    /// 1 | Goodbye, world!
    ///   | ^~~~~~~~~~~~~~~
    ///   |
    /// ```
    #[inline]
    pub fn stack(errs: impl IntoIterator<Item = Self>) -> Self {
        Self {
            kind : NomErrorKind::Stack(errs.into_iter().collect()),
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
    /// 
    /// # Example
    /// ```rust
    /// use std::str::FromStr as _;
    /// use nom::IResult;
    /// use nom::error::ErrorKind;
    /// use nom::{branch, bytes::complete as bc, character::complete as cc};
    /// use ast_toolkit::{Diagnostic, NomError, Span, SpanningExt as _};
    /// 
    /// type Input<'f, 's> = Span<'f, 's>;
    /// type Output<'f, 's, O> = IResult<Input<'f, 's>, O, NomError<Input<'f, 's>>>;
    /// 
    /// fn parser<'f, 's>(input: Input<'f, 's>) -> Output<'f, 's, Span<'f, 's>> {
    ///     branch::alt((
    ///         cc::digit1,
    ///         bc::tag("Hello, world!"),
    ///     ))(input)
    /// }
    /// 
    /// let err: nom::Err<NomError<Span>> = parser(Span::new("<farewell>", "Goodbye, world!")).unwrap_err();
    /// Diagnostic::from(err).emit();
    /// ```
    /// which should show you something like:
    /// ```plain
    /// error: Failed to parse as one of multiple possibilities
    ///  --> <farewell>:1:1
    ///   |
    /// 1 | Goodbye, world!
    ///   | ^~~~~~~~~~~~~~~
    ///   |
    /// error: Digit
    ///  --> <farewell>:1:1
    ///   |
    /// 1 | Goodbye, world!
    ///   | ^~~~~~~~~~~~~~~
    ///   = note: This is possibility 1/2
    /// error: Tag
    ///  --> <farewell>:1:1
    ///   |
    /// 1 | Goodbye, world!
    ///   | ^~~~~~~~~~~~~~~
    ///   = note: This is possibility 2/2
    /// error: Alt
    ///  --> <farewell>:1:1
    ///   |
    /// 1 | Goodbye, world!
    ///   | ^~~~~~~~~~~~~~~
    ///   |
    /// ```
    #[inline]
    pub fn branch(errs: impl IntoIterator<Item = Self>) -> Self {
        Self {
            kind : NomErrorKind::Branch(errs.into_iter().collect()),
            context : None,
        }
    }



    /// Equivalent to [`FromExternalError::from_external_error()`], except that it works with our more contextful [`ErrorKind`].
    /// 
    /// # Arguments
    /// - `input`: The input that describes where in the source text the error occurs.
    /// - `kind`: The [`ErrorKind`] describing (potentially more contextfully) what is wrong about the input.
    /// - `e`: The external error that occurred.
    /// 
    /// # Returns
    /// A new instance of Self that is based around the kind `kind` and `e`.
    pub fn from_external_error_kind(input: I, kind: impl Into<ErrorKind>, e: impl Error) -> Self
    where
        I: Clone,
    {
        let kind: ErrorKind = kind.into();

        // Compute the full stack of errors if `e` has a source
        if let Some(source) = e.source() {
            // Compute a full stack; put the main error in there first
            let mut stack: Vec<NomError<I>> = vec![ Self { kind: NomErrorKind::ExternalError(input.clone(), kind.clone(), e.to_string()), context: None } ];

            // Add the sources while there are any
            let mut source: Option<&dyn Error> = Some(source);
            while let Some(err) = source {
                source = err.source();
                stack.push(Self { kind: NomErrorKind::ExternalError(input.clone(), kind.clone(), e.to_string()), context: None })
            }

            // Alright cool, now return
            Self {
                kind : NomErrorKind::Stack(stack),
                context : None, // Some(Context::new(input, kind.description())),
            }
        } else {
            // Return it as a simple error
            Self {
                kind : NomErrorKind::ExternalError(input, kind.into(), e.to_string()),
                context : None, // Some(Context::new(input, kind.description())),
            }
        }
    }

    /// Equivalent to [`ParseError::append()`], except that it works with our more contextful [`ErrorKind`].
    /// 
    /// # Arguments
    /// - `input`: The input that describes where in the source text the error occurs.
    /// - `kind`: The [`ErrorKind`] describing (potentially more contextfully) what is wrong about the input.
    /// - `other`: The already existing instance of Self to append to.
    /// 
    /// # Returns
    /// The given instance but either with the error appended if it was a [`NomErrorKind::Stack`], or else it but translated into a stack with this error.
    pub fn append_kind(input: I, kind: impl Into<ErrorKind>, mut other: Self) -> Self {
        match &mut other.kind {
            // First, merge stacks
            NomErrorKind::Stack(other_errs) => {
                other_errs.push(Self::error_kind(input, kind));
                other
            },

            // The rest we can just pass as-is
            _ => Self::stack(vec![ other, Self::error_kind(input, kind) ]),
        }
    }



    /// Returns the input spanned by this error.
    /// 
    /// # Returns
    /// An instance of type `I` which spans the entire coverage of self.
    /// 
    /// # Panics
    /// This function may panic if we are a [`Stack`](NomErrorKind::Stack) but empty.
    /// 
    /// # Example
    /// ```rust
    /// use nom::error::ErrorKind;
    /// use ast_toolkit::{Combining as _, NomError, Span};
    /// 
    /// let span1 = Span::ranged("<example>", "Hello, world!", 0..5);
    /// let span2 = Span::ranged("<example>", "Hello, world!", 7..12);
    /// 
    /// let err1 = NomError::char(span1, 'h');
    /// assert_eq!(err1.input(), span1);
    /// let err2 = NomError::error_kind(span2, ErrorKind::Fail);
    /// assert_eq!(err2.input(), span2);
    /// 
    /// assert_eq!(NomError::stack(vec![ err1.clone(), err2.clone() ]).input(), Span::combined(span1, span2));
    /// assert_eq!(NomError::branch(vec![ err1, err2 ]).input(), Span::combined(span1, span2));
    /// ```
    pub fn input(&self) -> I where I: Clone + Combining {
        match &self.kind {
            NomErrorKind::ErrorKind(input, _)        |
            NomErrorKind::Char(input, _)             |
            NomErrorKind::ExternalError(input, _, _) => input.clone(),

            NomErrorKind::Stack(errs) | NomErrorKind::Branch(errs) => {
                // Get the first two elements in self
                let mut errs = errs.iter();
                let (first, second): (Option<&Self>, Option<&Self>) = (errs.next(), errs.next());
                match (first, second) {
                    // If both are given, then combine them into a range and consume the rest
                    (Some(first), Some(second)) => {
                        let mut input: I = I::combined(first.input(), second.input());
                        while let Some(third) = errs.next() {
                            input.consume(third.input());
                        }
                        input
                    },

                    // If there's only one, return that
                    (Some(err), _) | (_, Some(err)) => err.input(),

                    // Otherwise, panic
                    (_, _) => { panic!("Cannot get input of empty Stack error"); }
                }
            },
        }
    }
}

impl<I> Display for NomError<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match &self.kind {
            NomErrorKind::Char(_, c)                  => write!(f, "Syntax error: Expected {c:?}"),
            NomErrorKind::ErrorKind(_, kind)          => write!(f, "An error of kind {kind:?} occurred"),
            NomErrorKind::ExternalError(_, kind, err) => write!(f, "In {kind:?}: {err}"),

            // These are implemented for direct usage but aren't used in Diagnostics.
            NomErrorKind::Stack(_)  => write!(f, "Multiple errors occurred"),
            NomErrorKind::Branch(_) => write!(f, "One of multiple errors occurred"),
        }
    }
}

impl<I: Debug> Error for NomError<I> {}
impl<I> ContextError<I> for NomError<I> {
    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.context = Some(Context::new(input, ctx));
        other
    }
}
impl<I: Clone, E: Error> FromExternalError<I, E> for NomError<I> {
    #[inline]
    fn from_external_error(input: I, kind: nom::error::ErrorKind, e: E) -> Self { Self::from_external_error_kind(input, kind, e) }
}
impl<I> ParseError<I> for NomError<I> {
    #[inline]
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self { Self::error_kind(input, kind) }
    #[inline]
    fn from_char(input: I, c: char) -> Self { Self::char(input, c) }

    #[inline]
    fn append(input: I, kind: nom::error::ErrorKind, other: Self) -> Self { Self::append_kind(input, kind, other) }
    fn or(mut self, mut other: Self) -> Self {
        match (&mut self.kind, &mut other.kind) {
            // First: let's merge branches
            (NomErrorKind::Branch(self_errs), NomErrorKind::Branch(other_errs)) => {
                self_errs.extend(other_errs.drain(..));
                self
            },

            // Next, handle branch extends
            (NomErrorKind::Branch(self_errs), _) => {
                self_errs.push(other);
                self
            },
            (_, NomErrorKind::Branch(other_errs)) => {
                other_errs.push(self);
                other
            },

            // Any other case simply composes into a branch
            (_, _) => Self::branch(vec![ self, other ]),
        }
    }
}

impl<I: Clone + Combining + Spanning + SpanningExt> From<NomError<I>> for Diagnostic {
    /// Builds a [`Diagnostic`] tree from the given [`NomError`].
    /// 
    /// # Arguments
    /// - `value`: The [`NomError`] to build the tree out of.
    /// 
    /// # Returns
    /// A new [`Diagnostic`] tree based on the given error.
    /// 
    /// # Panics
    /// This function may panic if we were an empty [`Stack`](NomErrorKind::Stack) or [`Branch`](NomErrorKind::Branch).
    #[inline]
    #[track_caller]
    fn from(value: NomError<I>) -> Self {
        // Build and match a Diagnostic out of this
        let input: I = value.input();
        let mut diag: Diagnostic = match value.kind {
            NomErrorKind::Char(input, c) => {
                // Prepare the span to use
                let (span, actual): (Span, Option<char>) = if let Some(start) = input.start_idx() {
                    let source: &str = input.source();
                    (Span::ranged(input.file(), input.source(), start..=start), source.chars().nth(start))
                } else {
                    (Span::empty(input.file(), input.source()), None)
                };

                // Build the error
                if let Some(actual) = actual {
                    Diagnostic::error(format!("Syntax error: Expected {c:?}, got {actual:?}"), span)
                } else {
                    Diagnostic::error(format!("Syntax error: Expected {c:?}, got end of input"), span)
                }
            },

            NomErrorKind::ErrorKind(input, kind) => Diagnostic::error(
                kind.display(&input).to_string(),
                input,
            ),

            NomErrorKind::ExternalError(input, kind, err) => Diagnostic::error(
                format!("In {kind:?}: {err}"),
                input,
            ),



            NomErrorKind::Stack(errs) => {
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
            NomErrorKind::Branch(errs) => {
                // Generate a general diagnostic
                let mut diag = Diagnostic::error("Failed to parse as one of multiple possibilities", input);

                // Generate explaining diagnostics for each branch
                let n_errs: usize = errs.len();
                for (i, err) in errs.into_iter().enumerate() {
                    diag = diag.add(Self::from(err).set_remark(format!("This is possibility {}/{}", i + 1, n_errs)));
                }

                // Alright done
                diag
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
impl<I: Clone + Combining + Spanning + SpanningExt> From<nom::Err<NomError<I>>> for Diagnostic {
    /// Builds a [`Diagnostic`] tree from the given [`nom::Err<NomError>`].
    /// 
    /// # Arguments
    /// - `value`: The [`nom::Err<NomError>`] to build the tree out of.
    /// 
    /// # Returns
    /// A new [`Diagnostic`] tree based on the given error.
    /// 
    /// # Panics
    /// This function may panic if were an empty [`Stack`](NomErrorKind::Stack) or [`Branch`](NomErrorKind::Branch).
    #[inline]
    #[track_caller]
    fn from(value: nom::Err<NomError<I>>) -> Self {
        match value {
            nom::Err::Error(err)    => Diagnostic::from(err),
            nom::Err::Failure(err)  => Diagnostic::from(err),
            nom::Err::Incomplete(_) => Diagnostic::warn(
                "Given text parses OK but is incomplete",
                Span::empty("", ""),
            ),
        }
    }
}
