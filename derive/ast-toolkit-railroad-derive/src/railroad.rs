//  RAILROAD.rs
//    by Lut99
//
//  Created:
//    22 Feb 2024, 11:36:17
//  Last edited:
//    25 Feb 2024, 15:23:14
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements the `#[derive(ToNode)]`-macro.
//

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_macro_error::{Diagnostic, Level};
use quote::quote;
use syn::parse::{Parse, ParseBuffer};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Comma, PathSep};
use syn::{Attribute, Data, Expr, ExprLit, Generics, Ident, Lit, LitInt, LitStr, Meta, Path, PathArguments, PathSegment, Type, Visibility};


/***** MACRO RULES *****/
/// Builds a path segment.
macro_rules! path {
    // With leading colon
    (:: $($path:tt)*) => {
        Path {
            leading_colon: Some(PathSep::default()),
            segments: {
                let mut punc: Punctuated<PathSegment,PathSep> = Punctuated::new();
                _path_segments!(punc, $($path)*);
                punc
            },
        }
    };

    // W/o leading colon
    ($($path:tt)*) => {
        Path {
            leading_colon: None,
            segments: {
                let mut punc: Punctuated = Punctuated::new();
                _path_segments!(punc, $($path)*)
                punc
            },
        }
    };
}
macro_rules! _path_segments {
    ($punc:ident,) => {};

    ($punc:ident, $seg:ident $($t:tt)*) => {
        $punc.push(PathSegment { ident: Ident::new(stringify!($seg), Span::call_site()), arguments: PathArguments::None });
        _path_segments!($punc, $($t)*);
    };

    ($punc:ident, :: $($t:tt)*) => {
        $punc.push_punct(PathSep::default());
        _path_segments!($punc, $($t)*);
    };

    // Discard exclamation marks
    ($punc:ident, ! $($t:tt)*) => {
        _path_segments!($punc, $($t)*);
    };
}





/***** PATH MACROS *****/
/// The path used to access [`std`]'s [`Box`].
macro_rules! std_box {
    () => {
        path!(::std::boxed::Box)
    };
}
/// The path used to access [`std`]'s [`Vec`].
macro_rules! std_vec {
    () => {
        path!(::std::vec::Vec)
    };
}
/// The path used to access `ast-toolkit`'s `ToNode`.
macro_rules! ast_to_node {
    () => {
        path!(::ast_toolkit_railroad::ToNode)
    };
}
/// The path used to access `ast-toolkit`'s `ToNonTerm`.
macro_rules! ast_to_non_term {
    () => {
        path!(::ast_toolkit_railroad::ToNonTerm)
    };
}
/// The path used to access `ast-toolkit`'s `ToDelimNode`.
macro_rules! ast_to_delim_node {
    () => {
        path!(::ast_toolkit_railroad::ToDelimNode)
    };
}
/// The path used to access `railroad`'s `Node`.
macro_rules! rr_node {
    () => {
        path!(::ast_toolkit_railroad::railroad::Node)
    };
}
/// The path used to access `railroad`'s `NonTerminal`.
macro_rules! rr_non_terminal {
    () => {
        path!(::ast_toolkit_railroad::railroad::NonTerminal)
    };
}
/// The path used to access `railroad`'s `Sequence`.
macro_rules! rr_sequence {
    () => {
        path!(::ast_toolkit_railroad::railroad::Sequence)
    };
}
/// The path used to access `railroad`'s `Choice`.
macro_rules! rr_choice {
    () => {
        path!(::ast_toolkit_railroad::railroad::Choice)
    };
}
/// The path used to access `railroad`'s `Optional`.
macro_rules! rr_optional {
    () => {
        path!(::ast_toolkit_railroad::railroad::Optional)
    };
}
/// The path used to access `railroad`'s `LabeledBox`.
macro_rules! rr_labeled_box {
    () => {
        path!(::ast_toolkit_railroad::railroad::LabeledBox)
    };
}
/// The path used to access `railroad`'s `Terminal`.
macro_rules! rr_terminal {
    () => {
        path!(::ast_toolkit_railroad::railroad::Terminal)
    };
}
/// The path used to access `railroad`'s `Comment`.
macro_rules! rr_comment {
    () => {
        path!(::ast_toolkit_railroad::railroad::Comment)
    };
}





/***** HELPERS *****/
/// Represents an identifier of either [`Ident`]s or [`LitInt`]s.
#[derive(Clone, PartialEq)]
enum FieldIdent {
    Ident(Ident),
    Index(LitInt),
}
impl Parse for FieldIdent {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Parse as identifier, then fallback to litint
        if let Ok(res) = input.parse::<Ident>() {
            Ok(Self::Ident(res))
        } else if let Ok(res) = input.parse::<LitInt>() {
            Ok(Self::Index(res))
        } else {
            Err(input.error("Expected either an identifier or integer literal"))
        }
    }
}

/// Represents the three traits we know.
enum NodeKind {
    /// It's a typical node.
    Node,
    /// It's a nonterm.
    NonTerm,
    /// It's a delimited node.
    Delim(Vec<FieldIdent>),
}



/// Represents things we parse the from the toplevel for the `ToDelimNode`-macro.
struct ToplevelDelimAttributes {
    /// The opening delimiter.
    open:  LitStr,
    /// The closing delimiter.
    close: LitStr,
}
impl ToplevelDelimAttributes {
    /// Parses this FieldAttributes from the given [`Attribute`]s.
    ///
    /// # Arguments
    /// - `attrs`: The [`Attribute`] to parse.
    ///
    /// # Returns
    /// A new FieldAttributes.
    ///
    /// # Errors
    /// This function may error if the given `attrs` were not understood.
    fn parse(attrs: &[Attribute]) -> Result<Self, Diagnostic> {
        // Set out to collect what we want
        let mut open: Option<LitStr> = None;
        let mut close: Option<LitStr> = None;
        let mut span: Span = Span::call_site();
        for attr in attrs {
            // Match on the meta to find `#[railroad(...)]`
            match &attr.meta {
                Meta::List(l) => {
                    if l.path.is_ident("railroad") {
                        span = l.path.span();

                        // Attempt to parse its arguments as a comma-separated list of more metas
                        let args: Vec<Meta> = match l.parse_args_with(|buffer: &ParseBuffer| {
                            // Repeatedly parsed metas separated by commands
                            let mut metas: Vec<Meta> = vec![buffer.parse()?];
                            while !buffer.is_empty() {
                                // Parse a comma then a meta
                                buffer.parse::<Comma>()?;
                                metas.push(buffer.parse()?);
                            }
                            Ok(metas)
                        }) {
                            Ok(args) => args,
                            Err(err) => {
                                return Err(proc_macro_error::Diagnostic::spanned(
                                    l.tokens.span(),
                                    proc_macro_error::Level::Error,
                                    "Failed to parse struct/enum arguments".into(),
                                )
                                .span_error(err.span(), err.to_string()));
                            },
                        };

                        // Search to match
                        for meta in args {
                            match meta {
                                Meta::NameValue(nv) => {
                                    if nv.path.is_ident("open") {
                                        // Parse the value as a string literal
                                        let value: LitStr = if let Expr::Lit(ExprLit { attrs: _, lit: Lit::Str(value) }) = nv.value {
                                            value
                                        } else {
                                            return Err(Diagnostic::spanned(
                                                nv.value.span(),
                                                Level::Error,
                                                "Expected string literal as value for `open`".into(),
                                            ));
                                        };

                                        // Store it
                                        open = Some(value);
                                    } else if nv.path.is_ident("close") {
                                        // Parse the value as a string literal
                                        let value: LitStr = if let Expr::Lit(ExprLit { attrs: _, lit: Lit::Str(value) }) = nv.value {
                                            value
                                        } else {
                                            return Err(Diagnostic::spanned(
                                                nv.value.span(),
                                                Level::Error,
                                                "Expected string literal as value for `close`".into(),
                                            ));
                                        };

                                        // Store it
                                        close = Some(value);
                                    } else {
                                        return Err(Diagnostic::spanned(
                                            nv.path.span(),
                                            Level::Error,
                                            format!(
                                                "Unknown ToDelimNode-attribute '{}' in '#[railroad(...)]'",
                                                nv.path.span().source_text().unwrap_or_else(String::new)
                                            ),
                                        ));
                                    }
                                },

                                Meta::Path(p) => {
                                    return Err(Diagnostic::spanned(
                                        p.span(),
                                        Level::Error,
                                        format!(
                                            "Unknown ToDelimNode-attribute '{}' in '#[railroad(...)]'",
                                            p.span().source_text().unwrap_or_else(String::new)
                                        ),
                                    ));
                                },
                                Meta::List(l) => {
                                    return Err(Diagnostic::spanned(
                                        l.path.span(),
                                        Level::Error,
                                        format!(
                                            "Unknown ToDelimNode-attribute '{}' in '#[railroad(...)]'",
                                            l.path.span().source_text().unwrap_or_else(String::new)
                                        ),
                                    ));
                                },
                            }
                        }
                    } else {
                        continue;
                    }
                },

                Meta::Path(_) => continue,
                Meta::NameValue(_) => continue,
            }
        }

        // Assert we have what we need
        let open: LitStr = if let Some(open) = open {
            open
        } else {
            return Err(Diagnostic::spanned(span, Level::Error, "Missing opening delimiter, i.e., '#[railroad(open = \"...\")]`".into()));
        };
        let close: LitStr = if let Some(close) = close {
            close
        } else {
            return Err(Diagnostic::spanned(span, Level::Error, "Missing closing delimiter, i.e., '#[railroad(close = \"...\")]`".into()));
        };

        // Build ourselves
        Ok(Self { open, close })
    }
}

/// Represents things we parse from field attributes.
struct FieldAttributes {
    /// The type of implementation (either normal, nonterm or delimited).
    kind:     NodeKind,
    /// A comment, if any.
    comment:  Option<LitStr>,
    /// Whether to enclose the field in optional path.
    optional: bool,
}
impl FieldAttributes {
    /// Parses this FieldAttributes from the given [`Attribute`]s.
    ///
    /// # Arguments
    /// - `what`: The trait we're parsing for.
    /// - `attrs`: The [`Attribute`] to parse.
    ///
    /// # Returns
    /// A new FieldAttributes.
    ///
    /// # Errors
    /// This function may error if the given `attrs` were not understood.
    fn parse(what: &'static str, attrs: &[Attribute]) -> Result<Self, Diagnostic> {
        // Set out to collect what we want
        let mut kind: NodeKind = NodeKind::Node;
        let mut comment: Option<LitStr> = None;
        let mut optional: bool = false;
        for attr in attrs {
            // Match on the meta to find `#[railroad(...)]`
            match &attr.meta {
                Meta::List(l) => {
                    if l.path.is_ident("railroad") {
                        // Attempt to parse its arguments as a comma-separated list of more metas
                        let args: Vec<Meta> = match l.parse_args_with(|buffer: &ParseBuffer| {
                            // Repeatedly parsed metas separated by commands
                            let mut metas: Vec<Meta> = vec![buffer.parse()?];
                            while !buffer.is_empty() {
                                // Parse a comma then a meta
                                buffer.parse::<Comma>()?;
                                metas.push(buffer.parse()?);
                            }
                            Ok(metas)
                        }) {
                            Ok(args) => args,
                            Err(err) => {
                                return Err(proc_macro_error::Diagnostic::spanned(
                                    l.tokens.span(),
                                    proc_macro_error::Level::Error,
                                    "Failed to parse struct/enum arguments".into(),
                                )
                                .span_error(err.span(), err.to_string()));
                            },
                        };

                        // Search to match
                        for meta in args {
                            match meta {
                                Meta::Path(p) => {
                                    if p.is_ident("node") {
                                        kind = NodeKind::Node;
                                    } else if p.is_ident("nonterm") {
                                        kind = NodeKind::NonTerm;
                                    } else if p.is_ident("optional") {
                                        optional = true;
                                    } else {
                                        return Err(Diagnostic::spanned(
                                            p.span(),
                                            Level::Error,
                                            format!(
                                                "Unknown {}-attribute '{}' in '#[railroad(...)]'",
                                                what,
                                                p.span().source_text().unwrap_or_else(String::new)
                                            ),
                                        ));
                                    }
                                },
                                Meta::List(l) => {
                                    if l.path.is_ident("delim") || l.path.is_ident("delimited") {
                                        // Parse the rest of the list as fieldnames that include the delimiter
                                        let fields: Vec<FieldIdent> = match l.parse_args_with(|buffer: &ParseBuffer| {
                                            // Repeatedly parsed metas separated by commands
                                            let mut idents: Vec<FieldIdent> = vec![buffer.parse()?];
                                            while !buffer.is_empty() {
                                                // Parse a comma then a meta
                                                buffer.parse::<Comma>()?;
                                                idents.push(buffer.parse()?);
                                            }
                                            Ok(idents)
                                        }) {
                                            Ok(args) => args,
                                            Err(err) => {
                                                return Err(proc_macro_error::Diagnostic::spanned(
                                                    l.tokens.span(),
                                                    proc_macro_error::Level::Error,
                                                    "Failed to parse list of delimited fields".into(),
                                                )
                                                .span_error(err.span(), err.to_string()));
                                            },
                                        };

                                        // Store them
                                        kind = NodeKind::Delim(fields);
                                    } else {
                                        return Err(Diagnostic::spanned(
                                            l.path.span(),
                                            Level::Error,
                                            format!(
                                                "Unknown {}-attribute '{}' in '#[railroad(...)]'",
                                                what,
                                                l.path.span().source_text().unwrap_or_else(String::new)
                                            ),
                                        ));
                                    }
                                },
                                Meta::NameValue(nv) => {
                                    if nv.path.is_ident("comment") {
                                        // Parse the value as a string literal
                                        let value: LitStr = if let Expr::Lit(ExprLit { attrs: _, lit: Lit::Str(value) }) = nv.value {
                                            value
                                        } else {
                                            return Err(Diagnostic::spanned(
                                                nv.value.span(),
                                                Level::Error,
                                                "Expected string literal as value for `open`".into(),
                                            ));
                                        };

                                        // Store it
                                        comment = Some(value);
                                    } else {
                                        return Err(Diagnostic::spanned(
                                            nv.path.span(),
                                            Level::Error,
                                            format!(
                                                "Unknown {}-attribute '{}' in '#[railroad(...)]'",
                                                what,
                                                nv.path.span().source_text().unwrap_or_else(String::new)
                                            ),
                                        ));
                                    }
                                },
                            }
                        }
                    } else {
                        continue;
                    }
                },

                Meta::Path(_) => continue,
                Meta::NameValue(_) => continue,
            }
        }

        // Build ourselves
        Ok(Self { kind, comment, optional })
    }
}



/// Derives the expression that generates a type's `railroad::Node`.
///
/// # Arguments
/// - `what`: The trait we're parsing for.
/// - `ident`: The identifier of the parsed struct/enum.
/// - `data`: The contents of the parsed struct/enum.
///
/// # Returns
/// A tuple with two [`TokenStream2`]s: the first represents the type returned, and the second the expression that returns it.
///
/// # Errors
/// This function may error if any of the attributes were ill-formed.
pub fn derive_railroad_expr(what: &'static str, ident: &Ident, data: &Data) -> Result<(TokenStream2, TokenStream2), Diagnostic> {
    // Serialize the chosen paths
    let (std_box, std_vec, ast_to_node, ast_to_delim_node, rr_node, rr_sequence, rr_choice, rr_optional, rr_labeled_box, rr_comment) = (
        std_box!(),
        std_vec!(),
        ast_to_node!(),
        ast_to_delim_node!(),
        rr_node!(),
        rr_sequence!(),
        rr_choice!(),
        rr_optional!(),
        rr_labeled_box!(),
        rr_comment!(),
    );

    // Match on the type of data to find the railroad diagram elements
    match data {
        Data::Struct(s) => {
            // Collect all the fields of this struct into parts of a Sequence railroad element
            let mut reorder: Vec<(usize, Vec<FieldIdent>)> = Vec::new();
            let mut parts: Vec<(FieldIdent, TokenStream2)> = Vec::with_capacity(s.fields.len());
            for (i, f) in s.fields.iter().enumerate() {
                // Parse the attributes of this type
                let attrs: FieldAttributes = FieldAttributes::parse(what, &f.attrs)?;

                // Generate the thing to implement
                let ident: FieldIdent = f
                    .ident
                    .as_ref()
                    .map(|i| FieldIdent::Ident(i.clone()))
                    .unwrap_or_else(|| FieldIdent::Index(LitInt::new(&i.to_string(), Span::call_site())));
                let ty: &Type = &f.ty;
                match attrs.kind {
                    NodeKind::Node | NodeKind::NonTerm => {
                        // The base thing
                        let mut base: TokenStream2 =
                            quote! { { let b: #std_box<dyn #rr_node> = #std_box::new(<#ty as #ast_to_node>::railroad()); b }, };

                        // Optionally prefix it with a comment
                        if let Some(comment) = attrs.comment {
                            base = quote! {
                                {
                                    let b: #std_box<dyn #rr_node> = #std_box::new(#rr_sequence::new(#std_vec::from([
                                        { let b: #std_box<dyn #rr_node> = #std_box::new(#rr_comment::new(#comment.into())); b },
                                        #base
                                    ])));
                                    b
                                },
                            };
                        }

                        // Optionally make it optional
                        if attrs.optional {
                            base = quote! {
                                {
                                    let b: #std_box<dyn #rr_node> = #std_box::new(#rr_optional::new(#base));
                                    b
                                },
                            }
                        }

                        // Add it
                        parts.push((ident, base));
                    },
                    NodeKind::Delim(fields) => {
                        // The base things
                        let mut open: TokenStream2 =
                            quote! { { let b: #std_box<dyn #rr_node> = #std_box::new(<#ty as #ast_to_delim_node>::railroad_open()); b }, };
                        let mut close: TokenStream2 =
                            quote! { { let b: #std_box<dyn #rr_node> = #std_box::new(<#ty as #ast_to_delim_node>::railroad_close()); b }, };

                        // Optionally prefix open with a comment
                        if let Some(comment) = attrs.comment {
                            open = quote! {
                                {
                                    let b: #std_box<dyn #rr_node> = #std_box::new(#rr_sequence::new(#std_vec::from([
                                        { let b: #std_box<dyn #rr_node> = #std_box::new(#rr_comment::new(#comment.into())); b },
                                        #open
                                    ])));
                                    b
                                },
                            };
                        }

                        // Optionally make both optional
                        if attrs.optional {
                            open = quote! {
                                {
                                    let b: #std_box<dyn #rr_node> = #std_box::new(#rr_optional::new(#open));
                                    b
                                },
                            };
                            close = quote! {
                                {
                                    let b: #std_box<dyn #rr_node> = #std_box::new(#rr_optional::new(#close));
                                    b
                                },
                            };
                        }

                        // Add them
                        parts.push((ident.clone(), open));
                        let idx: usize = parts.len();
                        parts.push((ident, close));

                        // Generate the re-order command
                        // Read as: push tuple.1 to index tuple.0
                        reorder.push((idx, fields));
                    },
                }
            }

            // Resolve the reorders
            for order in reorder {
                // Remove all fields with a certain name
                let mut extract: Vec<(FieldIdent, TokenStream2)> = Vec::with_capacity(order.1.len());
                parts = parts
                    .into_iter()
                    .filter_map(|(name, stream)| {
                        if order.1.contains(&name) {
                            extract.push((name, stream));
                            None
                        } else {
                            Some((name, stream))
                        }
                    })
                    .collect();

                // Inject them at the specified location
                let second = parts.split_off(order.0);
                parts = parts.into_iter().chain(extract).chain(second.into_iter()).collect();
            }
            let parts: Vec<TokenStream2> = parts.into_iter().map(|(_, stream)| stream).collect();

            // Use those to build the full impl
            Ok((
                quote! {
                    #rr_sequence<#std_box<dyn #rr_node>>
                },
                quote! {
                    #rr_sequence::new(#std_vec::from([
                        #(#parts)*
                    ]))
                },
            ))
        },
        Data::Enum(e) => {
            // Collect all the fields of this enum into parts of a Sequence railroad element
            let mut parts: Vec<TokenStream2> = Vec::with_capacity(e.variants.len());
            for v in &e.variants {
                // Extract the name of the variant as a string
                let name: String = format!("{}::{}", ident, v.ident);

                // Collect nested compilers for all the enum's variants
                let mut vparts: Vec<TokenStream2> = Vec::with_capacity(v.fields.len());
                for f in &v.fields {
                    let ty: &Type = &f.ty;
                    vparts.push(quote! { { let b: #std_box<dyn #rr_node> = #std_box::new(<#ty as #ast_to_node>::railroad()); b }, });
                }

                // Build the compiler for this variant
                parts.push(quote! {
                    #rr_labeled_box::new(
                        #rr_sequence::new(#std_vec::from([ #(#vparts)* ])),
                        #rr_comment::new(#name.into()),
                    ),
                });
            }

            // Use those to build the full impl
            Ok((
                quote! {
                    #rr_choice<#rr_labeled_box<#rr_sequence<#std_box<dyn #rr_node>>, #rr_comment>>
                },
                quote! {
                    #rr_choice::new(#std_vec::from([ #(#parts)* ]))
                },
            ))
        },
        Data::Union(u) => Err(Diagnostic::spanned(u.union_token.span, Level::Error, "Cannot derive ToNode on unions".into())),
    }
}





/***** LIBRARY *****/
/// Takes the parsed struct or enum and implements [`ToNode`] for it.
///
/// # Arguments
/// - `ident`: The identifier of the parsed struct/enum.
/// - `data`: The contents of the parsed struct/enum.
/// - `attrs`: The list of attributes parsed from the main struct/enum.
/// - `generics`: The generics part of this struct/enum.
/// - `vis`: The visibility markers for this struct/enum.
///
/// # Errors
/// This function may error if any of the attributes were ill-formed.
pub fn derive_to_node(ident: Ident, data: Data, _attrs: Vec<Attribute>, generics: Generics, _vis: Visibility) -> Result<TokenStream, Diagnostic> {
    // Serialize the chosen paths
    let ast_to_node = ast_to_node!();

    // Parse the toplevel attributes
    // TODO

    // Generate the type & expression for the impl
    let (node_ty, node_expr): (TokenStream2, TokenStream2) = derive_railroad_expr("ToNode", &ident, &data)?;


    // Use those to build the full impl
    let (impl_gen, ty_gen, where_gen) = generics.split_for_impl();
    Ok(quote! {
        impl #impl_gen #ast_to_node for #ident #ty_gen #where_gen {
            type Node = #node_ty;

            #[inline]
            fn railroad() -> Self::Node {
                #node_expr
            }
        }
    }
    .into())
}



/// Takes the parsed struct or enum and implements [`ToNonTerm`] for it.
///
/// # Arguments
/// - `ident`: The identifier of the parsed struct/enum.
/// - `data`: The contents of the parsed struct/enum.
/// - `attrs`: The list of attributes parsed from the main struct/enum.
/// - `generics`: The generics part of this struct/enum.
/// - `vis`: The visibility markers for this struct/enum.
///
/// # Errors
/// This function may error if any of the attributes were ill-formed.
pub fn derive_to_non_term(ident: Ident, data: Data, _attrs: Vec<Attribute>, generics: Generics, _vis: Visibility) -> Result<TokenStream, Diagnostic> {
    // Serialize the chosen paths
    let (ast_to_node, ast_to_non_termm, rr_non_terminal) = (ast_to_node!(), ast_to_non_term!(), rr_non_terminal!());

    // Parse the toplevel attributes
    // TODO

    // Generate the type & expression for the impl
    let (node_ty, node_expr): (TokenStream2, TokenStream2) = derive_railroad_expr("ToNonTerm", &ident, &data)?;

    // Use those to build the full impls
    let name: String = ident.to_string();
    let (impl_gen, ty_gen, where_gen) = generics.split_for_impl();
    Ok(quote! {
        impl #impl_gen #ast_to_node for #ident #ty_gen #where_gen {
            type Node = #rr_non_terminal;

            #[inline]
            fn railroad() -> Self::Node {
                #rr_non_terminal::new(#name.into())
            }
        }

        impl #impl_gen #ast_to_non_termm for #ident #ty_gen #where_gen {
            type NodeNonTerm = #node_ty;

            #[inline]
            fn railroad_nonterm() -> Self::NodeNonTerm {
                #node_expr
            }
        }
    }
    .into())
}



/// Takes the parsed struct or enum and implements [`ToDelimNode`] for it.
///
/// # Arguments
/// - `ident`: The identifier of the parsed struct/enum.
/// - `data`: The contents of the parsed struct/enum.
/// - `attrs`: The list of attributes parsed from the main struct/enum.
/// - `generics`: The generics part of this struct/enum.
/// - `vis`: The visibility markers for this struct/enum.
///
/// # Errors
/// This function may error if any of the attributes were ill-formed.
pub fn derive_to_delim_node(
    ident: Ident,
    _data: Data,
    attrs: Vec<Attribute>,
    generics: Generics,
    _vis: Visibility,
) -> Result<TokenStream, Diagnostic> {
    // Serialize the chosen paths
    let (std_vec, ast_to_node, ast_to_delim_node, rr_sequence, rr_terminal) =
        (std_vec!(), ast_to_node!(), ast_to_delim_node!(), rr_sequence!(), rr_terminal!());

    // Parse the toplevel attributes
    let toplevel: ToplevelDelimAttributes = ToplevelDelimAttributes::parse(&attrs)?;

    // Use those to build the full impls
    let (open, close): (LitStr, LitStr) = (toplevel.open, toplevel.close);
    let (impl_gen, ty_gen, where_gen) = generics.split_for_impl();
    Ok(quote! {
        impl #impl_gen #ast_to_node for #ident #ty_gen #where_gen {
            type Node = #rr_sequence<#rr_terminal>;

            #[inline]
            fn railroad() -> Self::Node {
                #rr_sequence::new(#std_vec::from([
                    #rr_terminal::new(#open.into()),
                    #rr_terminal::new(#close.into()),
                ]))
            }
        }

        impl #impl_gen #ast_to_delim_node for #ident #ty_gen #where_gen {
            type NodeOpen = #rr_terminal;
            type NodeClose = #rr_terminal;

            #[inline]
            fn railroad_open() -> Self::NodeOpen {
                #rr_terminal::new(#open.into())
            }
            #[inline]
            fn railroad_close() -> Self::NodeOpen {
                #rr_terminal::new(#close.into())
            }
        }
    }
    .into())
}