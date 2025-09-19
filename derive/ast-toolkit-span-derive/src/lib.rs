//  LIB.rs
//    by Lut99
//
//  Description:
//!   Implements the derive macros for the `ast-toolkit-span`-crate.
//

// Modules
mod analyze;
mod generate;
mod spanning;
mod spanning_inf;
mod spanning_mut;
mod spanning_ref;

// Imports
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::{Attribute, Data, DeriveInput, Error, Generics, Ident, parse_macro_input};

use crate::analyze::{ToplevelAttrs, VariantAttrs, analyze_fields, analyze_fields_staircase};
use crate::generate::{EnumImplInfo, inject_attrs_and_gens, staircase_inject_attrs_and_gens};
use crate::spanning::SpanningImplInfoEnumImplFinish;
use crate::spanning_inf::SpanningInfImplInfoEnumImplFinish;
use crate::spanning_mut::SpanningMutImplInfoEnumImplFinish;
use crate::spanning_ref::SpanningRefImplInfoEnumImplFinish;


/***** LIBRARY *****/
/// Implements a derive macro for the `Spanning`-trait.
///
/// The `Spanning`-trait encodes that a type carries a span with it. In most cases, this span is
/// obtainable either by a field of type `Span<S>`, or by a field that recursively implements
/// `Spanning`. In others, it is needed to join multiple fields together into one span. This derive
/// macro supports all.
///
/// # Usage
/// To use this derive macro, simply add it to your struct:
/// ```ignore
/// use ast_toolkit_span::{Span, Spanning};
///
/// #[derive(Spanning)]
/// struct Token<S> {
///     span: Span<S>,
/// }
///
/// // Works
/// let span = Token { span: Span::new("Howdy") }.get_span();
/// ```
///
/// It can be implemented for both `struct`s and `enum`s. See the example at `ast_toolkit::span`
/// for more information.
///
/// ## Configuration
/// This derive macro can be configured by using the `spanning`- and `span`-attributes. The first
/// works at the toplevel- and variant level, whereas the second works on fieldlevel.
///
/// At toplevel, the following attributes are supported (**note**: the default values are given as
/// example values):
/// - `#[spanning(crate = ::ast_toolkit::span)]` changes the location of the parent
///   `ast_toolkit_span`-crate. Sometimes (e.g., in examples), you're not importing the crate
///   through `ast_toolkit` but rather standalone. Use this attribute to reconfigure the location.
/// - `#[spanning(s = S)]` changes the default identifier for the `S`ource-generic. This is the
///   type of the source text embedded in `Span`s. You should use this if your codebase assigns a
///   different canonical usage of `S`, causing a conflict otherwise.
/// - `#[spanning(lifetime = 's)]` changes the lifetime generated when we need to enforce
///   `Spannable<'s>` on `S`. You should use this if your codebase assigns a different canonical
///   usage of `'s`, causing a conflict otherwise.
/// - `#[spanning(impl_gen = <S>)]` changes the generics defined after `impl` for the generated
///   implementation. This is not _always_ `<S>` when omitted, it may include `'s` and other
///   generics already required by your type. In some cases, however, the default bounds are not
///   enough (see below) and you need to specify additional lifetimes or types to make them work.
///   You can do so here.
/// - `#[spanning(bounds = (S: Clone))]` changes the where-clauses generated for the `impl`. This
///   is not really literally this, it also includes bounds for types on which we recursively call
///   `Spanning::get_span()` and/or `Spannable<'s>` on `S`. To see a concrete list for your type,
///   use the excellent [cargo expand](https://github.com/dtolnay/cargo-expand) binary.
///
/// At the variant level, the following attributes are supported:
/// - `#[spanning(staircase)]` determines that the implementation that is generated will follow the
///   "staircase" algorithm (see below). If omitted, defaults to the normal one.
///
/// _Note: for structs, toplevel and variant-level are identical._
///
/// At the field level, the following attributes are supported:
/// - `#[span]` marks the current field as the field which we call `Spanning::get_span()` on in the
///   generate impl. If omitted, will default to a field called `span` OR the only field given in
///   case the struct/variant uses tuple syntax.
/// - `#[span(join)]` marks this field as to-be-joined with the `#[span]`-field. Multiple can be
///   given.
///
/// ## Staircase algorithm
/// This trait (not any of the other `Spanning*`-traits) supports a special algorithm called the
/// "staircase"-algorithm.
///
/// This algorithm is intended to be practical on AST nodes that closely follow the syntax. Such a
/// node may literally following the order of nodes or tokens as given by the user; e.g.,
/// ```ignore
/// struct BinOp<S> {
///     lhs: Expr<S>,
///     op: Op<S>,
///     rhs: Expr<S>,
/// }
/// ```
///
/// In this case, it can be practical to try and join the **outermost spans**, and if either of
/// those are not spannable, progressively move in. For example, for the above struct, this will
/// generate something like:
/// ```ignore
/// // ...
///
/// impl<S: Clone> Spanning<S> for BinOp<S> {
///     fn get_span(&self) -> Option<std::borrow::Cow<'_, Span<S>>> {
///         let lhs = self.lhs.get_span().or_else(self.op.get_span()).or_else(self.rhs.get_span())?;
///         let rhs = self.rhs.get_span().or_else(self.op.get_span()).or_else(self.lhs.get_span())?;
///         lhs.join(rhs.as_ref()).map(std::borrow::Cow::Owned)
///     }
///
///     fn take_span(self) -> Option<Span<S>> {
///         self.get_span().map(std::borrow::Cow::into_owned)
///     }
/// }
/// ```
///
/// It is surprising how often this pattern is useful.
///
/// There are a few additional fieldlevel attributes available when you are implementing this:
/// - `#[span(skip)]` encodes that this field should not be used in the staircase algorithm.
/// - `#[span(infallible)]` encodes that this field **cannot return `None`**. In that case, we
///   rely on its `SpanningInf`-implementation and end the chain there (either direction). The
///   closes infallible field is used to end the chain. This can optimize the generated code
///   somewhat, but because we still need to call `Span`, the result doesn't implement
///   `SpannableInf` itself.
#[proc_macro_derive(Spanning, attributes(span, spanning))]
pub fn spanning_derive(input: TokenStream) -> TokenStream {
    /// Internal wrapper for easier error handling.
    fn inner(attrs: Vec<Attribute>, ident: Ident, mut gens: Generics, data: Data) -> Result<TokenStream2, Error> {
        let mut tattrs = ToplevelAttrs::parse(&attrs)?;

        // Decide on the kind of input
        match data {
            // Normal impls
            Data::Struct(s) => {
                let vattrs = VariantAttrs::parse(&attrs)?;
                if !vattrs.staircase {
                    // Analyze the fields
                    let info = analyze_fields(s.fields)?;
                    inject_attrs_and_gens(&info, &mut tattrs, &mut gens);
                    Ok(info.spanning(&tattrs).struct_impl(&ident, &gens).into_token_stream().into())
                } else {
                    // Analyze the fields
                    let info = analyze_fields_staircase(s.fields)?;
                    staircase_inject_attrs_and_gens(&mut tattrs, &mut gens);
                    Ok(info.spanning(&tattrs).struct_impl(&ident, &gens).into_token_stream().into())
                }
            },
            Data::Enum(e) => {
                let mut info = EnumImplInfo::new();
                for variant in e.variants {
                    let vattrs = VariantAttrs::parse(&variant.attrs)?;
                    if vattrs.staircase {
                        return Err(Error::new(variant.ident.span(), "Staircase on enum variants is not (yet) supported"));
                    }

                    let vinfo = analyze_fields(variant.fields)?;
                    inject_attrs_and_gens(&vinfo, &mut tattrs, &mut gens);
                    info.add_variant(
                        variant.ident,
                        vinfo.ty_pat(),
                        vinfo.spanning(&tattrs).get_span(),
                        vinfo.spanning(&tattrs).take_span(),
                        vinfo.where_preds(&tattrs, "Spanning"),
                    );
                }
                Ok(info.finish::<SpanningImplInfoEnumImplFinish>(&tattrs, &gens, &ident).into_token_stream().into())
            },

            // Unsupported
            Data::Union(u) => Err(Error::new(u.union_token.span, "Cannot derive Spanning on unions")),
        }
    }


    // Let the external one handle it
    let DeriveInput { attrs, vis: _, ident, generics, data } = parse_macro_input!(input);
    match inner(attrs, ident, generics, data) {
        Ok(res) => res.into(),
        Err(err) => err.into_compile_error().into(),
    }
}



/// Implements a derive macro for the `SpanningInf`-trait.
///
/// The `SpanningInf`-trait is a subtrait of `Spanning`, and encodes that **it always returns
/// [`Some`] `Span`**. This is useful in contexts where you statically know this is the case, and
/// don't want to deal with the [`Option`].
///
/// As such, the fields you use to obtain the `Span` must themselves implement `SpanningInf`, not
/// just `Spanning`. Further, if you use `#[span(join)]` anywhere, it may return [`None`] when its
/// constituents are from different sources; in that case, it cannot be derived either.
///
/// # Usage
/// To use this derive macro, simply add it to your struct:
/// ```ignore
/// use ast_toolkit_span::{Span, SpanningInf};
///
/// #[derive(SpanningInf)]
/// struct Token<S> {
///     span: Span<S>,
/// }
///
/// // Works
/// let span = Token { span: Span::new("Howdy") }.span();
/// ```
///
/// It can be implemented for both `struct`s and `enum`s. See the example at `ast_toolkit::span`
/// for more information.
///
/// ## Configuration
/// This derive macro can be configured by using the `spanning`- and `span`-attributes. The first
/// works at the toplevel- and variant level, whereas the second works on fieldlevel.
///
/// See the [`Spanning`]-derive macro for more information. It uses the same configuration options,
/// except that `#[span(join)]` and `#[spanning(staircase)]` (and related) are not supported by
/// this derive macro.
#[proc_macro_derive(SpanningInf, attributes(span, spanning))]
pub fn spanning_inv_derive(input: TokenStream) -> TokenStream {
    /// Internal wrapper for easier error handling.
    fn inner(attrs: Vec<Attribute>, ident: Ident, mut gens: Generics, data: Data) -> Result<TokenStream2, Error> {
        let mut tattrs = ToplevelAttrs::parse(&attrs)?;

        // Decide on the kind of input
        match data {
            Data::Struct(s) => {
                let vattrs = VariantAttrs::parse(&attrs)?;
                if vattrs.staircase {
                    return Err(Error::new(ident.span(), "Cannot implement `SpanningInf` in a staircase-wise manner (always involves a join)"));
                }

                // Analyze the fields
                let info = analyze_fields(s.fields)?;
                if let Some((ident, _)) = info.joining().next() {
                    return Err(Error::new(ident.span(), "Cannot implement `SpanningInf` on a joinable type (the join may return `None`)"));
                }
                inject_attrs_and_gens(&info, &mut tattrs, &mut gens);
                Ok(info.spanning_inf(&tattrs).struct_impl(&ident, &gens).into_token_stream().into())
            },
            Data::Enum(e) => {
                let mut info = EnumImplInfo::new();
                for variant in e.variants {
                    let vattrs = VariantAttrs::parse(&variant.attrs)?;
                    if vattrs.staircase {
                        return Err(Error::new(ident.span(), "Cannot implement `SpanningInf` in a staircase-wise manner (always involves a join)"));
                    }

                    let vinfo = analyze_fields(variant.fields)?;
                    if let Some((ident, _)) = vinfo.joining().next() {
                        return Err(Error::new(ident.span(), "Cannot implement `SpanningInf` on a joinable type (the join may return `None`)"));
                    }
                    inject_attrs_and_gens(&vinfo, &mut tattrs, &mut gens);
                    info.add_variant(
                        variant.ident,
                        vinfo.ty_pat(),
                        vinfo.spanning_inf(&tattrs).span(),
                        vinfo.spanning_inf(&tattrs).into_span(),
                        vinfo.where_preds(&tattrs, "SpanningInf"),
                    );
                }
                Ok(info.finish::<SpanningInfImplInfoEnumImplFinish>(&tattrs, &gens, &ident).into_token_stream().into())
            },

            // Unsupported
            Data::Union(u) => Err(Error::new(u.union_token.span, "Cannot derive SpanningInf on unions")),
        }
    }


    // Let the external one handle it
    let DeriveInput { attrs, vis: _, ident, generics, data } = parse_macro_input!(input);
    match inner(attrs, ident, generics, data) {
        Ok(res) => res.into(),
        Err(err) => err.into_compile_error().into(),
    }
}



/// Implements a derive macro for the `SpanningRef`-trait.
///
/// The `SpanningRef`-trait is a subtrait of `SpanningInf`, and encodes that not only always
/// returns a `Span`, but only that that span is directly accessible (i.e., doesn't need to be
/// constructed).
///
/// As such, the fields you use to obtain the `Span` must themselves implement `SpanningRef`, not
/// just `Spanning`. Further, if you use `#[span(join)]` anywhere, the `Span` is constructed
/// instead of accessed, and `SpanningRef` can no longer be implemented.
///
/// # Usage
/// To use this derive macro, simply add it to your struct:
/// ```ignore
/// use ast_toolkit_span::{Span, SpanningRef};
///
/// #[derive(SpanningRef)]
/// struct Token<S> {
///     span: Span<S>,
/// }
///
/// // Works
/// let span = Token { span: Span::new("Howdy") }.span_ref();
/// ```
///
/// It can be implemented for both `struct`s and `enum`s. See the example at `ast_toolkit::span`
/// for more information.
///
/// ## Configuration
/// This derive macro can be configured by using the `spanning`- and `span`-attributes. The first
/// works at the toplevel- and variant level, whereas the second works on fieldlevel.
///
/// See the [`Spanning`]-derive macro for more information. It uses the same configuration options,
/// except that `#[span(join)]` and `#[spanning(staircase)]` (and related) are not supported by
/// this derive macro.
#[proc_macro_derive(SpanningRef, attributes(span, spanning))]
pub fn spanning_ref_derive(input: TokenStream) -> TokenStream {
    /// Internal wrapper for easier error handling.
    fn inner(attrs: Vec<Attribute>, ident: Ident, mut gens: Generics, data: Data) -> Result<TokenStream2, Error> {
        let mut tattrs = ToplevelAttrs::parse(&attrs)?;

        // Decide on the kind of input
        match data {
            Data::Struct(s) => {
                let vattrs = VariantAttrs::parse(&attrs)?;
                if vattrs.staircase {
                    return Err(Error::new(ident.span(), "Cannot implement `SpanningRef` in a staircase-wise manner (always involves a join)"));
                }

                // Analyze the fields
                let info = analyze_fields(s.fields)?;
                if let Some((ident, _)) = info.joining().next() {
                    return Err(Error::new(ident.span(), "Cannot implement `SpanningRef` on a joinable type (the join may return `None`)"));
                }
                inject_attrs_and_gens(&info, &mut tattrs, &mut gens);
                Ok(info.spanning_ref(&tattrs).struct_impl(&ident, &gens).into_token_stream().into())
            },
            Data::Enum(e) => {
                let mut info = EnumImplInfo::new();
                for variant in e.variants {
                    let vattrs = VariantAttrs::parse(&variant.attrs)?;
                    if vattrs.staircase {
                        return Err(Error::new(ident.span(), "Cannot implement `SpanningRef` in a staircase-wise manner (always involves a join)"));
                    }

                    let vinfo = analyze_fields(variant.fields)?;
                    if let Some((ident, _)) = vinfo.joining().next() {
                        return Err(Error::new(ident.span(), "Cannot implement `SpanningRef` on a joinable type (the join may return `None`)"));
                    }
                    inject_attrs_and_gens(&vinfo, &mut tattrs, &mut gens);
                    info.add_variant_one(
                        variant.ident,
                        vinfo.ty_pat(),
                        vinfo.spanning_ref(&tattrs).span_ref(),
                        vinfo.where_preds(&tattrs, "SpanningRef"),
                    );
                }
                Ok(info.finish::<SpanningRefImplInfoEnumImplFinish>(&tattrs, &gens, &ident).into_token_stream().into())
            },

            // Unsupported
            Data::Union(u) => Err(Error::new(u.union_token.span, "Cannot derive SpanningRef on unions")),
        }
    }


    // Let the external one handle it
    let DeriveInput { attrs, vis: _, ident, generics, data } = parse_macro_input!(input);
    match inner(attrs, ident, generics, data) {
        Ok(res) => res.into(),
        Err(err) => err.into_compile_error().into(),
    }
}



/// Implements a derive macro for the `SpanningMut`-trait.
///
/// The `SpanningMut`-trait is a subtrait of `SpanningRef`, and encodes that not only always
/// returns a `Span` and is directly accessible, but also that it's **mutably** accessible. This is
/// the most restrictive form of `Spanning`.
///
/// As such, the fields you use to obtain the `Span` must themselves implement `SpanningMut`, not
/// just `Spanning`. Further, if you use `#[span(join)]` anywhere, the `Span` is constructed
/// instead of accessed, and `SpanningMut` can no longer be implemented.
///
/// # Usage
/// To use this derive macro, simply add it to your struct:
/// ```ignore
/// use ast_toolkit_span::{Span, SpanningMut};
///
/// #[derive(SpanningMut)]
/// struct Token<S> {
///     span: Span<S>,
/// }
///
/// // Works
/// let span = Token { span: Span::new("Howdy") }.span_mut();
/// ```
///
/// It can be implemented for both `struct`s and `enum`s. See the example at `ast_toolkit::span`
/// for more information.
///
/// ## Configuration
/// This derive macro can be configured by using the `spanning`- and `span`-attributes. The first
/// works at the toplevel- and variant level, whereas the second works on fieldlevel.
///
/// See the [`Spanning`]-derive macro for more information. It uses the same configuration options,
/// except that `#[span(join)]` and `#[spanning(staircase)]` (and related) are not supported by
/// this derive macro.
#[proc_macro_derive(SpanningMut, attributes(span, spanning))]
pub fn spanning_mut_derive(input: TokenStream) -> TokenStream {
    /// Internal wrapper for easier error handling.
    fn inner(attrs: Vec<Attribute>, ident: Ident, mut gens: Generics, data: Data) -> Result<TokenStream2, Error> {
        let mut tattrs = ToplevelAttrs::parse(&attrs)?;

        // Decide on the kind of input
        match data {
            Data::Struct(s) => {
                let vattrs = VariantAttrs::parse(&attrs)?;
                if vattrs.staircase {
                    return Err(Error::new(ident.span(), "Cannot implement `SpanningMut` in a staircase-wise manner (always involves a join)"));
                }

                // Analyze the fields
                let info = analyze_fields(s.fields)?;
                if let Some((ident, _)) = info.joining().next() {
                    return Err(Error::new(ident.span(), "Cannot implement `SpanningMut` on a joinable type (the join may return `None`)"));
                }
                inject_attrs_and_gens(&info, &mut tattrs, &mut gens);
                Ok(info.spanning_mut(&tattrs).struct_impl(&ident, &gens).into_token_stream().into())
            },
            Data::Enum(e) => {
                let mut info = EnumImplInfo::new();
                for variant in e.variants {
                    let vattrs = VariantAttrs::parse(&variant.attrs)?;
                    if vattrs.staircase {
                        return Err(Error::new(ident.span(), "Cannot implement `SpanningInf` in a staircase-wise manner (always involves a join)"));
                    }

                    let vinfo = analyze_fields(variant.fields)?;
                    if let Some((ident, _)) = vinfo.joining().next() {
                        return Err(Error::new(ident.span(), "Cannot implement `SpanningMut` on a joinable type (the join may return `None`)"));
                    }
                    inject_attrs_and_gens(&vinfo, &mut tattrs, &mut gens);
                    info.add_variant_one(
                        variant.ident,
                        vinfo.ty_pat(),
                        vinfo.spanning_mut(&tattrs).span_mut(),
                        vinfo.where_preds(&tattrs, "SpanningMut"),
                    );
                }
                Ok(info.finish::<SpanningMutImplInfoEnumImplFinish>(&tattrs, &gens, &ident).into_token_stream().into())
            },

            // Unsupported
            Data::Union(u) => Err(Error::new(u.union_token.span, "Cannot derive SpanningMut on unions")),
        }
    }


    // Let the external one handle it
    let DeriveInput { attrs, vis: _, ident, generics, data } = parse_macro_input!(input);
    match inner(attrs, ident, generics, data) {
        Ok(res) => res.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
