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
/// # Syntax
/// Call the macro by first giving a `#[cfg(...)]`'s body, and then any combinator expression
/// generating the combinator you want to conditionally use.
///
/// # Returns
/// A [`Combinator`] that is the given one if the `cfg` is enabled, or a
/// [`Nop`](crate::combinator::nop::Nop) otherwise.
///
/// # Example
/// ```rust
/// use ast_toolkit_snack::combinator::map;
/// use ast_toolkit_snack::result::SnackError;
/// use ast_toolkit_snack::scan::tag;
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
///         is_fixable: false,
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



/// Generates a newtype combinator that wraps a given (potentially very complex) combinator type.
///
/// This is useful to reduce compile times for your parser. Rust struggles with many, very large
/// types, either explicitly or obfuscated through `impl Combinator<...>`; hence, it is fruitful to
/// "cut" the typetree by injecting a newtype.
///
/// # Syntax
/// There are different forms of syntax available for this macro. The examples below show how to
/// use each syntax.
///
/// ## Abbreviated syntax
/// This form assumes that most of the generics of your newtype are simple, only defining `S` and
/// optionally `'t` and `'s` on the newtype itself, and then porting them to the
/// [`Combinator`](crate::Combinator)-impl. If `'t` is omitted, it defaults to `'static`.
///
/// The syntax is given by: zero or more doc comments, followed by an optional visibility modifier,
/// then the name of the newtype, either `<'t, 's, S>`, `<'t, S>`, `<'s, S>` or `<S>`, and then the
/// full type that the newtype wraps delimited by parenthesis.
///
/// ## Explicit syntax
/// The final form is the most expressive, but also the most tedious. It is implemented as a list
/// of key/value pairs, separated by commas:
/// - `cfg = [...]`: Defines the content of any `#[cfg(...)]`-macros to put at the newtype.
/// - `visibility = $vis:vis`: Defines the visibility of the newtype.
/// - `name = $name:ident`: Defines the name of the newtype.
/// - `ty_gen = (...)`: Defines the generics to put at the type's definition. Optional.
/// - `impl_gen = (...)`: Defines the generics to put after the `impl` of
///   [`Combinator`](crate::Combinator) for the newtype. Optional.
/// - `comb_gen = (...)`: Defines the generics to put after the `Combinator` of the
///   [`Combinator`](crate::Combinator)-impl for the newtype.
/// - `where_preds = (...)`: Defines a list of where predicates to put at the
///   [`Combinator`](crate::Combinator)-impl for the newtype.
/// - `ty = $ty:ty`: Defines the type to wrap the newtype around.
///
/// Optionally, you can precede the key/value pairs with a bunch of doc comments to attach to the
/// newtype.
///
/// # Examples
/// ```rust
/// use ast_toolkit_span::Spannable;
/// use ast_toolkit_snack::combinator::nop;
/// use ast_toolkit_snack::comb_newtype;
///
/// // Abbreviated syntax - no lifetimes specified
/// comb_newtype!(
///     /// A newtype wrapping [`nop()`].
///     pub NopPrime<S>(nop::Nop<S>)
/// );
/// ```
///
/// ```rust
/// use ast_toolkit_span::Spannable;
/// use ast_toolkit_snack::combinator::nop;
/// use ast_toolkit_snack::comb_newtype;
///
/// // Explicit syntax
/// comb_newtype!(
///     /// A newtype wrapping [`nop()`]
///     visibility = pub,
///     name = NopPrime,
///     ty_gen = (S),
///     impl_gen = ('s, S),
///     comb_gen = ('static, 's, S),
///     where_preds = (S: Clone + Spannable<'s>),
///     ty = nop::Nop<S>,
/// );
/// ```
#[macro_export]
macro_rules! comb_newtype {
    // Abbreviated syntax, without `'t` and `'s`
    ($(#[doc = $docs:literal])* $vis:vis $name:ident<S>($comb:ty) $(;)?) => {
        $crate::comb_newtype!(
            $(#[doc = $docs])*
            visibility = $vis,
            name = $name,
            ty_gen = (S),
            impl_gen = ('s, S: Clone + $crate::Spannable<'s>),
            comb_gen = ('static, 's, S),
            ty = $comb,
        )
    };
    // Abbreviated syntax, with `'t` but without `'s`
    ($(#[doc = $docs:literal])* $vis:vis $name:ident<'t, S>($comb:ty) $(;)?) => {
        $crate::comb_newtype!(
            $(#[doc = $docs])*
            visibility = $vis,
            name = $name,
            ty_gen = ('t, S),
            impl_gen = ('t, 's, S: Clone + $crate::Spannable<'s>),
            comb_gen = ('t, 's, S),
            ty = $comb,
        )
    };
    // Abbreviated syntax, without `'t` but with `'s`
    ($(#[doc = $docs:literal])* $vis:vis $name:ident<'s, S>($comb:ty) $(;)?) => {
        $crate::comb_newtype!(
            $(#[doc = $docs])*
            visibility = $vis,
            name = $name,
            ty_gen = ('s, S),
            impl_gen = ('s, S: Clone + $crate::Spannable<'s>),
            comb_gen = ('static, 's, S),
            ty = $comb,
        )
    };
    // Abbreviated syntax, with `'t` and `'s`
    ($(#[doc = $docs:literal])* $vis:vis $name:ident<'t, 's, S>($comb:ty) $(;)?) => {
        $crate::comb_newtype!(
            $(#[doc = $docs])*
            visibility = $vis,
            name = $name,
            ty_gen = ('t, 's, S),
            impl_gen = ('t, 's, S: Clone + $crate::Spannable<'s>),
            comb_gen = ('t, 's, S),
            ty = $comb,
        )
    };

    // Explicit syntax (most verbose, so has the actual impl)
    ($(#[doc = $docs:literal])* $(cfg = [$($cfg:tt)*],)? visibility = $vis:vis, name = $name:ident $(, ty_gen = ($($ty_gen:tt)+))? $(, impl_gen = ($($impl_gen:tt)+))? , comb_gen = ($($comb_gen:tt)+) $(, where_preds = ($($where_preds:tt)+))?, ty = $comb:ty $(,)?) => {
        $(#[cfg($($cfg)*)])?
        $(#[doc = $docs])*
        $vis struct $name $(<$($ty_gen)+>)? ($comb);
        $(#[cfg($($cfg)*)])?
        impl $(<$($impl_gen)+>)? $crate::Combinator <$($comb_gen)+> for $name $(<$($ty_gen)+>)? $(where $($where_preds)+)? {
            type ExpectsFormatter = <$comb as $crate::Combinator<$($comb_gen)+>>::ExpectsFormatter;
            type Output = <$comb as $crate::Combinator<$($comb_gen)+>>::Output;
            type Recoverable = <$comb as $crate::Combinator<$($comb_gen)+>>::Recoverable;
            type Fatal = <$comb as $crate::Combinator<$($comb_gen)+>>::Fatal;

            #[inline]
            fn expects(&self) -> Self::ExpectsFormatter { <$comb as $crate::Combinator <$($comb_gen)+>>::expects(&self.0) }

            #[inline]
            fn parse(&mut self, input: $crate::Span<S>) -> $crate::result::Result<Self::Output, Self::Recoverable, Self::Fatal, S> {
                <$comb as $crate::Combinator <$($comb_gen)+>>::parse(&mut self.0, input)
            }
        }
    };
}
