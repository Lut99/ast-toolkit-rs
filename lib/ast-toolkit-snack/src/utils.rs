//  UTILS.rs
//    by Lut99
//
//  Created:
//    16 Sep 2024, 10:25:19
//  Last edited:
//    16 Sep 2024, 11:30:32
//  Auto updated?
//    Yes
//
//  Description:
//!   Contains utilities that are used internally.
//


/***** LIBRARY *****/
/// Macro for a little more ergonomically implementing a combinator.
macro_rules! comb_impl {
    // Utility for stripping plusses
    (__strip_plus: + $(rem:tt)*) => { $(rem)* };

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
