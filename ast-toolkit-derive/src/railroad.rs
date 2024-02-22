//  RAILROAD.rs
//    by Lut99
//
//  Created:
//    22 Feb 2024, 11:36:17
//  Last edited:
//    22 Feb 2024, 17:20:49
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
use syn::punctuated::Punctuated;
use syn::token::PathSep;
use syn::{Attribute, Data, Generics, Ident, Path, PathArguments, PathSegment, Type, Visibility};


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

    ($punc:ident, $seg:ident ! $($t:tt)*) => {
        $punc.push(PathSegment { ident: Ident::new(concat!(stringify!($seg), "!"), Span::call_site()), arguments: PathArguments::None });
        _path_segments!($punc, $($t)*);
    };

    ($punc:ident, $seg:ident $($t:tt)*) => {
        $punc.push(PathSegment { ident: Ident::new(stringify!($seg), Span::call_site()), arguments: PathArguments::None });
        _path_segments!($punc, $($t)*);
    };

    ($punc:ident, :: $($t:tt)*) => {
        $punc.push_punct(PathSep::default());
        _path_segments!($punc, $($t)*);
    };
}





/***** CONSTANTS *****/
thread_local! {
    /// Defines how we refer to the `ToNode`-trait.
    static TYPES: Types = Types {
        std_box: path!( ::std::boxed::Box ),
        std_vec: path! { ::std::vec! },
        to_node: path! { ::ast_toolkit::railroad::ToNode },
        rr_node: path! { ::ast_toolkit::railroad::Node },
        rr_sequence: path! { ::ast_toolkit::railroad::Sequence },
        rr_choice: path! { ::ast_toolkit::railroad::Choice },
        rr_labeled_box: path! { ::ast_toolkit::railroad::LabeledBox },
        rr_comment: path! { ::ast_toolkit::railroad::Comment },
    };
}





/***** HELPERS *****/
/// Defines a nice interface for catching all statics at once.
struct Types {
    /// Defines how to refer to `std`'s `Box`-struct.
    std_box: Path,
    /// Defines how to refer to `std`'s `vec!`-macro.
    std_vec: Path,

    /// Defines how to refer to the `ast_toolkit`'s `ToNode`-trait.
    to_node: Path,

    /// Defines how to refer to the `railroad`'s `Node`-trait.
    rr_node: Path,
    /// Defines how to refer to the `railroad`'s `Sequence`-struct.
    rr_sequence: Path,
    /// Defines how to refer to the `railroad`'s `Choice`-struct.
    rr_choice: Path,
    /// Defines how to refer to the `railroad`'s `LabeledBox`-struct.
    rr_labeled_box: Path,
    /// Defines how to refer to the `railroad`'s `Comment`-struct.
    rr_comment: Path,
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
    let (impl_gen, ty_gen, where_gen) = generics.split_for_impl();

    // Match on the type of data to find the railroad diagram elements
    let to_node_impl: TokenStream2 = match data {
        Data::Struct(s) => TYPES.with(|Types { std_box, std_vec, to_node, rr_node, rr_sequence, rr_choice: _, rr_labeled_box: _, rr_comment: _ }| {
            // Collect all the fields of this struct into parts of a Sequence railroad element
            let mut parts: Vec<TokenStream2> = Vec::with_capacity(s.fields.len());
            for f in s.fields {
                let ty: Type = f.ty;
                parts.push(quote! { #std_box::new(<#ty as #to_node>::railroad()), });
            }

            // Use those to build the full impl
            quote! {
                impl #impl_gen #to_node for #ident #ty_gen #where_gen {
                    type Node = #rr_sequence<#std_box<dyn #rr_node>>;

                    #[inline]
                    fn railroad() -> Self::Node {
                        #rr_sequence::new(#std_vec [
                            #(#parts)*
                        ])
                    }
                }
            }
        }),
        Data::Enum(e) => TYPES.with(|Types { std_vec, std_box, to_node, rr_node, rr_sequence, rr_choice, rr_labeled_box, rr_comment }| {
            // Collect all the fields of this enum into parts of a Sequence railroad element
            let mut parts: Vec<TokenStream2> = Vec::with_capacity(e.variants.len());
            for e in e.variants {
                // Extract the name of the variant as a string
                let name: String = e.ident.to_string();

                // Collect nested compilers for all the enum's variants
                let mut vparts: Vec<TokenStream2> = Vec::with_capacity(e.fields.len());
                for f in e.fields {
                    let ty: Type = f.ty;
                    vparts.push(quote! { #std_box::new(<#ty as #to_node>::railroad()), });
                }

                // Build the compiler for this variant
                parts.push(quote! {
                    #rr_labeled_box::new(
                        #rr_sequence::new(#std_vec [
                            #(#parts)*
                        ]),
                        #rr_comment::new(#name.into()),
                    ),
                });
            }
            for p in &parts {
                println!("{p:#}\n");
            }

            // Use those to build the full impl
            quote! {
                impl #impl_gen #to_node for #ident #ty_gen #where_gen {
                    type Node = #rr_labeled_box<#rr_choice<#rr_sequence<#std_box<dyn #rr_node>>>>;

                    #[inline]
                    fn railroad() -> Self::Node {
                        #rr_choice::new(#std_vec [
                            #(#parts)*
                        ]),
                    }
                }
            }
        }),
        Data::Union(u) => return Err(Diagnostic::spanned(u.union_token.span, Level::Error, "Cannot derive ToNode on unions".into())),
    };

    // Alright, return that impl
    Ok(to_node_impl.into())
}
