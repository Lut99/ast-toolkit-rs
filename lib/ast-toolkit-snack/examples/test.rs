//  TEST.rs
//    by Lut99
//
//  Created:
//    02 May 2024, 15:04:35
//  Last edited:
//    06 Aug 2024, 17:01:34
//  Auto updated?
//    Yes
//
//  Description:
//!   Tests something.
//

use std::convert::Infallible;
use std::fmt::{Display, Formatter};

use ast_toolkit_snack::{Combinator, ExpectsFormatter, Result as SResult};
use ast_toolkit_span::Span;


fn test<F, S>() -> impl Combinator<'static, F, S, Output = (), Error = Infallible> {
    struct RetFmt {
        fmt: String,
    }
    impl std::fmt::Debug for RetFmt {
        #[inline]
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "Something") }
    }
    impl Display for RetFmt {
        #[inline]
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "Expected ")?;
            self.expects_fmt(f, 0)
        }
    }
    impl ExpectsFormatter for RetFmt {
        #[inline]
        fn expects_fmt(&self, f: &mut Formatter, _indent: usize) -> std::fmt::Result { write!(f, "{}", self.fmt) }
    }
    struct Ret<C, F> {
        comb: C,
        fmt:  F,
    }
    impl<'t, C, F: 't + Fn() -> String> ::ast_toolkit_snack::Expects<'t> for Ret<C, F> {
        type Formatter = RetFmt;

        #[inline]
        fn expects(&self) -> Self::Formatter { RetFmt { fmt: (self.fmt)() } }
    }
    impl<'t, F, S, O, C: FnMut(Span<F, S>) -> SResult<'t, O, F, S>, F2: 't + Fn() -> String> Combinator<'t, F, S> for Ret<C, F2> {
        type Output = O;
        type Error = Infallible;

        #[inline]
        fn parse(&mut self, input: Span<F, S>) -> SResult<'t, Self::Output, F, S> { (self.comb)(input) }
    }
    Ret { comb: move |input: Span<F, S>| -> SResult<'static, (), F, S> { SResult::Ok(input, ()) }, fmt: || "A test".into() }
}

#[cfg(feature = "derive")]
fn test_derived<F, S>() -> impl Combinator<'static, F, S, Output = (), Error = Infallible> {
    ast_toolkit_snack_derive::comb! {
        expects "Something";

        combinator -> 'static, (), !Infallible {
            SResult::Ok(input, ())
        };
    }
}

#[cfg(feature = "derive")]
#[ast_toolkit_snack_derive::combinator(expected = "Howdy", output = (), error = Infallible)]
fn test_derived2<F, S>(input: Span<F, S>) -> ast_toolkit_snack::Result<F, S, Infallible> {}


fn main() {
    let mut test = test::<&'static str, &'static str>();
    println!("{:?}", test.parse(Span::new("<example>", "Hiya!")));

    #[cfg(feature = "derive")]
    let mut test = test_derived::<&'static str, &'static str>();
    #[cfg(feature = "derive")]
    println!("{:?}", test.parse(Span::new("<example>", "Hiya!")));
}
