//  UTILS.rs
//    by Lut99
//
//  Created:
//    16 Sep 2024, 10:25:19
//  Last edited:
//    17 Sep 2024, 09:56:38
//  Auto updated?
//    Yes
//
//  Description:
//!   Contains utilities that are used internally.
//


/***** LIBRARY *****/
/// Macro for a little more ergonomically implementing an error type.
macro_rules! error_impl {
    (
        $(#[$outer:meta])* gen struct $name:ident < $($gen:tt),* > {
            $($(#[$field_attrs:meta])* $field:ident : $field_ty:ty),* $(,)?
        } impl {
            fn fmt(&$fmt_self:ident, $f:ident: &mut Formatter) $(where $($fmt_var:ident: ($($fmt_bound:tt)+)),* $(,)?)? {
                $($fmt_body:tt)*
            }
            fn span(&$span_self:ident) $(where $($span_var:ident: ($($span_bound:tt)+)),* $(,)?)? {
                $($span_body:tt)*
            }
            $(fn source(&$src_self:ident) $(where $($src_var:ident: ($($src_bound:tt)+)),* $(,)?)? {
                $($src_body:tt)*
            })?
        }
    ) => {
        $(#[$outer])*
        pub struct $name < $($gen),* > {
            $($(#[$field_attrs])* $field: $field_ty,)*
        }
        impl< $($gen),* > ::std::fmt::Debug for $name < $($gen),* > {
            #[inline]
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                let mut f = f.debug_struct(stringify!($name));
                $(f.field(stringify!($field), &self.$field);)*
                f.finish()
            }
        }
        impl< $($gen),* > ::std::fmt::Display for $name < $($gen),* >
        $(where $($fmt_var: ($($fmt_bound)+)),*)?
        {
            #[inline]
            fn fmt(&$fmt_self, $f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                $($fmt_body)*
            }
        }
        impl< $($gen),* > ::ast_toolkit_span::Spanning<F, S> for $name < $($gen),* >
        $(where $($span_var: ($($span_bound)+)),*)?
        {
            #[inline]
            fn span(&$span_self) -> ::ast_toolkit_span::Span<F, S> {
                $($span_body)*
            }
        }
        impl< $($gen),* > crate::result::Error<F, S> for $name < $($gen),* >
        $(where $($span_var: ($($span_bound)+)),*)?
        $(where $($fmt_var: ($($fmt_bound)+)),*)?
        $($(where $($src_var: ($($src_bound)+)),*)?)?
        {
            $(
                #[inline]
                fn source(&$src_self) -> Option<&dyn crate::result::Error<F, S>> {
                    $($src_body)*
                }
            )?
        }
    };
}
pub(crate) use error_impl;

/// Macro for a little more ergonomically implementing a combinator.
macro_rules! comb_impl {
    (
        $(#[$outer:meta])* gen $name:ident < $($gen:tt),* > {
            $($(#[$field_attrs:meta])* $field:ident : $field_ty:ty),* $(,)?
        } impl {
            type Output = $output:ty;
            type Recoverable = $recoverable:ty;
            type Fatal = $fatal:ty;

            gen Formatter < $($egen:tt),* > {
                $($(#[$efield_attrs:meta])* $efield:ident : $efield_ty:ty),* $(,)?
            } impl {
                fn expects_fmt(&$self:ident, $f:ident: &mut Formatter, $indent:ident: usize) {
                    $($ebody:tt)*
                }
            }

            fn expects<$elife:lifetime>(&$eeself:ident) $(where $($evar:ident: ($($ebound:tt)+)),* $(,)?)? {
                $($eebody:tt)*
            }
            fn parse<$clife:lifetime, $cf:ident, $cs:ident>(&mut $pself:ident, $input:ident: Span<F, S>) $(where $($cvar:ident: ($($cbound:tt)+)),* $(,)?)? {
                $($pbody:tt)*
            }

            comb $comb:ident($($cargs:ident : $cargs_ty:ty),*) {
                $($cbody:tt)*
            }
        }
    ) => {
        // Generate the formatter
        ::paste::paste! {
            #[doc = concat!(" Formatter for generating \"Expected ...\"-strings for [`", stringify!($name), "`].")]
            #[derive(Debug)]
            pub struct [<$name ExpectsFormatter>]<$($egen),*> {
                $($(#[$efield_attrs])* $efield : $efield_ty,)*
            }
            impl<$($egen),*> ::std::fmt::Display for [< $name ExpectsFormatter >] <$($egen),*> {
                #[inline]
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    write!(f, "Expected ")?;
                    <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
                }
            }
            impl<$($egen),*> crate::ExpectsFormatter for [< $name ExpectsFormatter >] <$($egen),*> {
                #[inline]
                fn expects_fmt(&$self, $f: &mut ::std::fmt::Formatter, $indent: usize) -> ::std::fmt::Result {
                    $($ebody)*
                }
            }
        }



        // Generate the combinator itself
        #[doc = concat!(" Combinator returned by [`", stringify!($comb), "()`].")]
        pub struct $name <$($gen),*> {
            $($(#[$field_attrs])* $field : $field_ty,)*
        }
        ::paste::paste! {
            impl<$($gen),*> crate::Expects<$elife> for $name<$($gen),*>
            $(where
                $($evar: $($ebound)+),*)?
            {
                type Formatter = [<$name ExpectsFormatter>]<$($egen),*>;

                #[inline]
                fn expects(&$eeself) -> Self::Formatter {
                    $($eebody)*
                }
            }
        }
        impl<$($gen),*> crate::Combinator2<$clife, $cf, $cs> for $name<$($gen),*>
        $(where
            $($cvar: $($cbound)+),*)?
        {
            type Output = $output;
            type Recoverable = $recoverable;
            type Fatal = $fatal;

            fn parse(&mut $pself, $input: ::ast_toolkit_span::Span<F, S>) -> Result<(::ast_toolkit_span::Span<F, S>, Self::Output), crate::result::SnackError<F, S, Self::Recoverable, Self::Fatal>> {
                $($pbody)*
            }
        }



        // Generate the combinator function
        $(#[$outer])*
        #[inline]
        pub const fn $comb<$($gen),*>($($cargs : $cargs_ty),*) -> $name<$($gen),*>
        $(where
            $($cvar: $($cbound)+),*)?
        {
            $($cbody)*
        }
    };
}
pub(crate) use comb_impl;
