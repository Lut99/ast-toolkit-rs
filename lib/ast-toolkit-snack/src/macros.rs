//  MACROS.rs
//    by Lut99
//
//  Description:
//!   Defines special combinator-like macros that will help your work.
//


/***** LIBRARY *****/
/// Conditionally adds a combinator in your parse tree based on a `cfg`-flag.
///
/// This macro is aimed to have conditional parsing be as painless as possible; that means it may
/// incur a little overhead (depending on whether Rust optimizes it away or not). Specifically, it
/// will either use your macro if the cfg succeeds, or inject a [`nop()`](crate::combinator::nop())
/// otherwise.
///
/// # Arguments
/// Call the macro by first giving a `#[cfg(...)]`'s body, and then any combinator expression
/// generating the combinator you want to conditionally use.
///
/// # Returns
/// A [`Combinator`] that is the given one if the `cfg` is enabled, or a
/// [`Nop`](crate::combinator::nop::Nop) otherwise.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::bytes::complete::tag;
/// use ast_toolkit_snack::combinator::map;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::sequence::pair;
/// use ast_toolkit_snack::{Combinator as _, comb_cfg};
/// use ast_toolkit_span::Span;
///
/// #[derive(Debug, PartialEq)]
/// struct HelloWorld {
///     #[cfg(hello)]
///     hello: Span<&'static str>,
///     world: Span<&'static str>,
/// }
///
/// let span = Span::new("HelloWorld");
///
/// let mut comb =
///     map(pair(comb_cfg!(hello, tag(b"Hello")), tag(b"World")), |(hello, world)| HelloWorld {
///         #[cfg(hello)]
///         hello,
///         world,
///     });
/// #[cfg(hello)]
/// assert_eq!(
///     comb.parse(span),
///     Ok((span.slice(10..), HelloWorld { hello: span.slice(..5), world: span.slice(5..10) }))
/// );
/// #[cfg(not(hello))]
/// assert_eq!(
///     comb.parse(span),
///     Err(SnackError::Recoverable(pair::Recoverable::Comb1(tag::Recoverable {
///         tag: b"World",
///         span,
///     })))
/// );
/// ```
#[macro_export]
macro_rules! comb_cfg {
    ($cfg:meta, $comb:expr) => {{
        #[cfg($cfg)]
        {
            $comb
        }
        #[cfg(not($cfg))]
        {
            $crate::combinator::nop()
        }
    }};
}
