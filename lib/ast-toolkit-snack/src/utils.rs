//  UTILS.rs
//    by Lut99
//
//  Created:
//    16 Sep 2024, 10:25:19
//  Last edited:
//    19 Sep 2024, 14:09:55
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
        #[comb = $comb:literal]
        gen struct $name:ident < $($gen:tt),* > {
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
        #[doc = concat!("Error returned by the [`", $comb, "`]-combinator.")]
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

/// Macro for a little more ergonomically implementing a combinator's formatter.
macro_rules! fmt_impl {
    (
        #[comb = $comb:expr]
        gen $name:ident < $($gen:tt),* > {
            $($(#[$field_attrs:meta])* $field:ident : $field_ty:ty),* $(,)?
        } impl {
            fn expects_fmt(&$self:ident, $f:ident: &mut Formatter, $indent:ident: usize) $(where $($var:ident: ($($bound:tt)+)),+ $(,)?)? {
                $($body:tt)*
            }
        }
    ) => {
        ::paste::paste! {
            #[doc = concat!(" Formatter for generating \"Expected ...\"-strings for [`", $comb, "`].")]
            #[derive(Debug)]
            pub struct $name<$($gen),*> {
                $($(#[$field_attrs])* $field : $field_ty,)*
            }
            impl<$($gen),*> ::std::fmt::Display for $name<$($gen),*>
            $(where
                $($var: $($bound)+),+)?
            {
                #[inline]
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    write!(f, "Expected ")?;
                    <Self as crate::ExpectsFormatter>::expects_fmt(self, f, 0)
                }
            }
            impl<$($gen),*> crate::ExpectsFormatter for $name<$($gen),*>
            $(where
                $($var: $($bound)+),+)?
            {
                #[inline]
                fn expects_fmt(&$self, $f: &mut ::std::fmt::Formatter, $indent: usize) -> ::std::fmt::Result {
                    $($body)*
                }
            }
        }
    };
}
pub(crate) use fmt_impl;

/// Macro for a little more ergonomically implementing a combinator.
macro_rules! comb_impl {
    (
        $(#[$outer:meta])*
        gen $name:ident < $($gen:tt),* > {
            $($(#[$field_attrs:meta])* $field:ident : $field_ty:ty),* $(,)?
        } impl {
            type Formatter = $formatter:ty;
            type Output = $output:ty;
            type Recoverable = $recoverable:ty;
            type Fatal = $fatal:ty;


            fn<$($exp_ty_gen:tt),*> Expects<$exp_life:lifetime>::expects(&$exp_self:ident: Self<$($exp_self_gen:tt),*>) $(where $($exp_var:ident: ($($exp_bound:tt)+)),+ $(,)?)? {
                $($exp_body:tt)*
            }

            fn<$($parse_ty_gen:tt),*> Combinator<$($parse_comb_gen:tt),*>::parse(&mut $parse_self:ident: Self<$($parse_self_gen:tt),*>, $input:ident: Span<F, S>) $(where $($parse_var:ident: ($($parse_bound:tt)+)),+ $(,)?)? {
                $($parse_body:tt)*
            }

            comb<$($comb_ty_gen:tt),*> $comb:ident($($comb_args:ident : $comb_args_ty:ty),*) -> Self<$($comb_self_gen:tt),*> {
                $($comb_body:tt)*
            }
        }
    ) => {
        #[doc = concat!(" Combinator returned by [`", stringify!($comb), "()`].")]
        pub struct $name <$($gen),*> {
            $($(#[$field_attrs])* $field : $field_ty,)*
        }
        impl<$($exp_ty_gen),*> crate::Expects<$exp_life> for $name<$($exp_self_gen),*>
        $(where
            $($exp_var: $($exp_bound)+),+)?
        {
            type Formatter = $formatter;

            #[inline]
            fn expects(&$exp_self) -> Self::Formatter {
                $($exp_body)*
            }
        }
        impl<$($parse_ty_gen),*> crate::Combinator2<$($parse_comb_gen),*> for $name<$($parse_self_gen),*>
        $(where
            $($parse_var: $($parse_bound)+),+)?
        {
            type Output = $output;
            type Recoverable = $recoverable;
            type Fatal = $fatal;

            fn parse(&mut $parse_self, $input: ::ast_toolkit_span::Span<F, S>) -> Result<(::ast_toolkit_span::Span<F, S>, Self::Output), crate::result::SnackError<F, S, Self::Recoverable, Self::Fatal>> {
                $($parse_body)*
            }
        }



        // Generate the combinator function
        $(#[$outer])*
        #[inline]
        pub const fn $comb<$($comb_ty_gen),*>($($comb_args : $comb_args_ty),*) -> $name<$($gen),*>
        $(where
            $($parse_var: $($parse_bound)+),*)?
        {
            $($comb_body)*
        }
    };
}
pub(crate) use comb_impl;
