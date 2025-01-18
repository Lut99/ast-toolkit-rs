//  SEPARATED MANY 0.rs
//    by Lut99
//
//  Created:
//    18 Jan 2025, 18:56:39
//  Last edited:
//    18 Jan 2025, 18:56:39
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`separated_many0()`]-combinator.
//

use std::convert::Infallible;
use std::error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spanning};
use better_derive::{Debug, Eq, PartialEq};

use super::super::combinator2::recognize;
use crate::result::{Result as SResult, SnackError};
use crate::{Combinator2, ExpectsFormatter as _};


/***** ERRORS *****/
/// Defines the fatal errors thrown by [`SeparatedMany0`].
#[derive(Debug, Eq, PartialEq)]
pub enum Fatal<E1, E2, O2, F, S> {
    /// The element-combinator failed fatally.
    Comb(E1),
    /// The separator-combinator failed fatally.
    Separator(E2),
    /// Lint that will explain the user they are missing a separator.
    MissingSeparator { sep: O2, span: Span<F, S> },
    /// Lint that will explain the user they added an incorrect trailing separator.
    TrailingSeparator { span: Span<F, S> },
}
impl<E1: Display, E2: Display, O2: crate::ExpectsFormatter, F, S> Display for Fatal<E1, E2, O2, F, S> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            Self::Comb(err) => err.fmt(f),
            Self::Separator(err) => err.fmt(f),
            Self::MissingSeparator { sep, span: _ } => {
                <O2 as Display>::fmt(sep, f)?;
                write!(f, " as separator")
            },
            Self::TrailingSeparator { span: _ } => write!(f, "Encountered trailing separator"),
        }
    }
}
impl<E1: error::Error, E2: error::Error, O2: crate::ExpectsFormatter, F, S> error::Error for Fatal<E1, E2, O2, F, S> {
    #[inline]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::Comb(err) => err.source(),
            Self::Separator(err) => err.source(),
            Self::MissingSeparator { sep: _, span: _ } => None,
            Self::TrailingSeparator { span: _ } => None,
        }
    }
}
impl<E1: Spanning<F, S>, E2: Spanning<F, S>, O2, F: Clone, S: Clone> Spanning<F, S> for Fatal<E1, E2, O2, F, S> {
    #[inline]
    fn span(&self) -> Span<F, S> {
        match self {
            Self::Comb(err) => err.span(),
            Self::Separator(err) => err.span(),
            Self::MissingSeparator { sep: _, span } => span.clone(),
            Self::TrailingSeparator { span } => span.clone(),
        }
    }

    #[inline]
    fn into_span(self) -> Span<F, S> {
        match self {
            Self::Comb(err) => err.into_span(),
            Self::Separator(err) => err.into_span(),
            Self::MissingSeparator { sep: _, span } => span,
            Self::TrailingSeparator { span } => span,
        }
    }
}





/***** FORMATTERS *****/
/// ExpectsFormatter for the [`SeparatedMany0`] combinator.
#[derive(Debug, Eq, PartialEq)]
pub struct ExpectsFormatter<O1, O2> {
    /// The thing we expect multiple times.
    pub fmt: O1,
    /// The thing interleaving the thing we expected multiple times.
    pub sep: O2,
}
impl<O1: crate::ExpectsFormatter, O2: crate::ExpectsFormatter> Display for ExpectsFormatter<O1, O2> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "Expected ")?;
        self.expects_fmt(f, 0)
    }
}
impl<O1: crate::ExpectsFormatter, O2: crate::ExpectsFormatter> crate::ExpectsFormatter for ExpectsFormatter<O1, O2> {
    #[inline]
    fn expects_fmt(&self, f: &mut Formatter, indent: usize) -> FResult {
        write!(f, "multiple repetitions of ")?;
        self.fmt.expects_fmt(f, indent)?;
        write!(f, " interleaved with ")?;
        self.sep.expects_fmt(f, indent)
    }
}





/***** COMBINATORS *****/
/// Actual implementation of the [`separated_many0()`]-combinator.
pub struct SeparatedMany0<C1, C2, F, S> {
    comb: C1,
    sep: C2,
    _f: PhantomData<F>,
    _s: PhantomData<S>,
}
impl<'t, C1, C2, F, S> Combinator2<'t, F, S> for SeparatedMany0<C1, C2, F, S>
where
    F: Clone,
    S: Clone,
    C1: Combinator2<'t, F, S>,
    C2: Combinator2<'t, F, S>,
{
    type ExpectsFormatter = ExpectsFormatter<C1::ExpectsFormatter, C2::ExpectsFormatter>;
    type Output = Vec<C1::Output>;
    type Recoverable = Infallible;
    type Fatal = Fatal<C1::Fatal, C2::Fatal, C2::ExpectsFormatter, F, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { ExpectsFormatter { fmt: self.comb.expects(), sep: self.sep.expects() } }

    #[inline]
    fn parse(&mut self, input: Span<F, S>) -> SResult<F, S, Self::Output, Self::Recoverable, Self::Fatal> {
        // Parse the first element
        let mut res: Vec<C1::Output> = Vec::new();
        let mut rem: Span<F, S> = match self.comb.parse(input.clone()) {
            Ok((rem, elem)) => {
                if res.len() >= res.capacity() {
                    res.reserve(1 + res.len())
                }
                res.push(elem);
                rem
            },
            Err(SnackError::Recoverable(_)) => 
        };

        let mut rem: Span<F, S> = input;
        loop {
            // If we've already parsed an element, expected a comma
            let next: Span<F, S> = if !res.is_empty() {
                match self.sep.parse(rem.clone()) {
                    Ok((rem, _)) => rem,
                    Err(SnackError::Recoverable(_)) => {
                        // In principle, we are done. However, we may want to lint for a missing separator.
                        if self.lint_missing_sep {
                            if let Ok((_, span)) = recognize(&mut self.comb).parse(rem.clone()) {
                                return Err(SnackError::Fatal(Fatal::MissingSeparator { sep: self.sep.expects(), span }));
                            }
                        }

                        // If it doesn't detect, we're gucci
                        return Ok((rem, res));
                    },
                    Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Separator(err))),
                    Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
                }
            } else {
                rem
            };

            // Parse the element
            match self.comb.parse(next.clone()) {
                Ok((rem2, res2)) => {
                    if res.len() >= res.capacity() {
                        res.reserve(1 + res.len())
                    }
                    res.push(res2);
                    rem = rem2;
                },
                Err(SnackError::Recoverable(_)) => {
                    // We're eager, which means that we won't stop here (except for the first element, since we're 0)
                    if !res.is_empty() && self.lint_trailing_comma {
                        return Err(SnackError::Fatal(Fatal::TrailingSeparator { span: next }));
                    } else {
                        return Ok((next, res));
                    }
                },
                Err(SnackError::Fatal(err)) => return Err(SnackError::Fatal(Fatal::Comb(err))),
                Err(SnackError::NotEnough { needed, span }) => return Err(SnackError::NotEnough { needed, span }),
            }
        }
    }
}
