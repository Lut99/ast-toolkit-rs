//  FORGET TY.rs
//    by Lut99
//
//  Created:
//    09 May 2025, 09:52:35
//  Last edited:
//    09 May 2025, 10:03:59
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the [`forget_ty`]-combinator.
//

use std::marker::PhantomData;

use ast_toolkit_span::{Span, Spannable};

use crate::boxed::BoxedParseError;
use crate::result::{Result as SResult, SnackError};
use crate::{Combinator, ExpectsFormatter};


/***** COMBINATORS *****/
/// Actual implementation of [`forget_ty()`].
pub struct ForgetTy<'f, 'e1, 'e2, C, S> {
    comb: C,
    _f:   PhantomData<&'f ()>,
    _e1:  PhantomData<&'e1 ()>,
    _e2:  PhantomData<&'e2 ()>,
    _s:   PhantomData<S>,
}
impl<'c, 'f, 'e1, 'e2, 's, C, S> Combinator<'c, 's, S> for ForgetTy<'f, 'e1, 'e2, C, S>
where
    C: Combinator<'c, 's, S>,
    C::ExpectsFormatter: 'f,
    C::Recoverable: 'e1,
    C::Fatal: 'e2,
    S: Clone + Spannable<'s>,
{
    type ExpectsFormatter = Box<dyn 'f + ExpectsFormatter>;
    type Output = C::Output;
    type Recoverable = BoxedParseError<'e1, S>;
    type Fatal = BoxedParseError<'e2, S>;

    #[inline]
    fn expects(&self) -> Self::ExpectsFormatter { Box::new(self.comb.expects()) }

    #[inline]
    fn parse(&mut self, input: Span<S>) -> SResult<Self::Output, Self::Recoverable, Self::Fatal, S> {
        match self.comb.parse(input) {
            Ok(res) => Ok(res),
            Err(SnackError::Recoverable(err)) => Err(SnackError::Recoverable(BoxedParseError::new(err))),
            Err(SnackError::Fatal(err)) => Err(SnackError::Fatal(BoxedParseError::new(err))),
        }
    }
}





/***** LIBRARY *****/
/// Highly specialized combinator for "forgetting" or "erasing" associated types of another
/// combinator.
///
/// Essentially, this combinator turns any [`ExpectsFormatter`] or [`ParseError`] into
/// [`Box<dyn ExpectsFormatter>`](Box) and [`BoxedParseError`]s, respectively. This is useful when
/// you find yourself needed to mention the associated types of the combinator, but either can't be
/// bothered (they are huge) or it's impossible (recursion).
///
/// If you need to box the _whole_ combinator (e.g., recursion), see the
/// [`BoxableCombinator`](crate::boxed::BoxableCombinator)-trait.
///
/// # Arguments
/// - `comb`: Some [`Combinator`] of which to erase the associated types.
///
/// # Returns
/// A combinator that does exactly the same as the given `comb`, except that its associated types
/// are behind boxes.
///
/// # Fails
/// The returned combinator fails exactly when `comb` does.
///
/// # Examples
/// The following example shows a function that requires an "associated type agnostic" version of a
/// combinator:
/// ```compile_fail
/// use ast_toolkit_snack::boxed::BoxedParseError;
/// use ast_toolkit_snack::scan::tag;
/// use ast_toolkit_snack::{Combinator, ExpectsFormatter};
/// use ast_toolkit_span::{Span, Spannable};
///
/// fn assert_associated<'c, 's, C, S>(comb: C) -> C
/// where
///     C: Combinator<
///             'c,
///             's,
///             S,
///             ExpectsFormatter = Box<dyn ExpectsFormatter>,
///             Recoverable = BoxedParseError<'static, S>,
///             Fatal = BoxedParseError<'static, S>,
///         >,
///     S: Clone + Spannable<'s>,
/// {
///     comb
/// }
///
/// let span1 = Span::new("Hello, world!");
///
/// // This is the problematic statement
/// let mut comb = assert_associated(tag(b"Hello"));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// ```
///
/// The following example is identical, but uses `forget_ty()` to make it work:
/// ```rust
/// # use ast_toolkit_snack::boxed::BoxedParseError;
/// use ast_toolkit_snack::combinator::forget_ty;
/// # use ast_toolkit_snack::scan::tag;
/// # use ast_toolkit_snack::{Combinator, ExpectsFormatter};
/// # use ast_toolkit_span::{Span, Spannable};
/// #
/// # fn assert_associated<'c, 's, C, S>(comb: C) -> C
/// # where
/// #     C: Combinator<
/// #             'c,
/// #             's,
/// #             S,
/// #             ExpectsFormatter = Box<dyn ExpectsFormatter>,
/// #             Recoverable = BoxedParseError<'static, S>,
/// #             Fatal = BoxedParseError<'static, S>,
/// #         >,
/// #     S: Clone + Spannable<'s>,
/// # {
/// #     comb
/// # }
/// #
/// # let span1 = Span::new("Hello, world!");
/// // ...
///
/// // Now it works!
/// let mut comb = assert_associated(forget_ty(tag(b"Hello")));
/// assert_eq!(comb.parse(span1).unwrap(), (span1.slice(5..), span1.slice(..5)));
/// ```
/// ```
#[inline]
pub const fn forget_ty<'c, 'f, 'e1, 'e2, 's, C, S>(comb: C) -> ForgetTy<'f, 'e1, 'e2, C, S>
where
    C: Combinator<'c, 's, S>,
    C::ExpectsFormatter: 'f,
    C::Recoverable: 'e1,
    C::Fatal: 'e2,
    S: Clone + Spannable<'s>,
{
    ForgetTy { comb, _f: PhantomData, _e1: PhantomData, _e2: PhantomData, _s: PhantomData }
}
