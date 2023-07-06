//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 18:16:24
//  Last edited:
//    06 Jul 2023, 09:37:49
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the derivation procedure for the [`Diagnostic`].
//!   
//!   Note that, technically, it's not the [`Diagnostic`] that's being derived,
//!   but rather the [`Into<Diagnostic>`].
// 

use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Data, Expr, ExprLit, Fields, Generics, Ident, FieldsNamed, FieldsUnnamed, Lit, LitStr, Meta, Token, Visibility};
use syn::__private::Span;
use syn::parse::ParseBuffer;
use syn::spanned::Spanned as _;


/***** HELPER FUNCTIONS *****/
/// Extracts the information we want from the toplevel attributes.
/// 
/// # Arguments
/// - `attrs`: The list of attributes given at toplevel.
/// 
/// # Returns
/// A new [`ToplevelAttributes`] struct that contains the parsed information.
/// 
/// # Errors
/// This function may errors if the attribute tokens were invalid.
fn parse_toplevel_attrs(attrs: impl AsRef<[Attribute]>) -> Result<ToplevelAttributes, proc_macro_error::Diagnostic> {
    let attrs: &[Attribute] = attrs.as_ref();

    // Parse the attributes
    let mut toplevel: ToplevelAttributes = ToplevelAttributes::empty();
    /* TODO */

    // Done, return the struct
    Ok(toplevel)
}

/// Extracts the information we want from the field-level attributes.
/// 
/// # Arguments
/// - `attrs`: The list of attributes given at toplevel.
/// - `span`: The span of the field's identifier so that we can emit a bit more useful error messages.
/// 
/// # Returns
/// A list of new [`FieldAttributes`] structs, one for each `#[diag(...)]` attribute parsed.
/// 
/// # Errors
/// This function may errors if the attribute tokens were invalid.
fn parse_field_attrs(attrs: impl AsRef<[Attribute]>, span: Span) -> Result<Vec<FieldAttributes>, proc_macro_error::Diagnostic> {
    let attrs: &[Attribute] = attrs.as_ref();

    // Parse the attributes
    let mut fields: Vec<(FieldAttributes, Span)> = vec![];
    'attrs: for a in attrs {
        // Examine the meta found
        match &a.meta {
            Meta::List(l) => if l.path.is_ident("diag") {
                // It's the diagnostic we are looking for *jedi handwaving

                // Attempt to parse its arguments as a comma-separated list of more metas
                let args: Vec<Meta> = match l.parse_args_with(|buffer: &ParseBuffer| {
                    // Repeatedly parsed metas separated by commands
                    let mut metas: Vec<Meta> = vec![ buffer.parse()? ];
                    while !buffer.is_empty() {
                        // Parse a comma then a meta
                        buffer.parse::<Token!(,)>()?;
                        metas.push(buffer.parse()?);
                    }
                    Ok(metas)
                }) {
                    Ok(args) => args,
                    Err(err) => { return Err(proc_macro_error::Diagnostic::spanned(l.tokens.span(), proc_macro_error::Level::Error, "Failed to parse attribute arguments".into()).span_error(err.span(), err.to_string())); },
                };

                // Now iterate over them to collect the arguments
                let mut f: FieldAttributes = FieldAttributes::empty();
                for a in args {
                    match a {
                        Meta::Path(p) => if p.is_ident("error") {
                            // Note down the type of the attribute
                            f.kind = DiagnosticKind::Error;
                        } else if p.is_ident("warn") || p.is_ident("warning") {
                            // Note down the type of the attribute
                            f.kind = DiagnosticKind::Warn;
                        } else if p.is_ident("note") {
                            // Note down the type of the attribute
                            f.kind = DiagnosticKind::Note;
                        } else if p.is_ident("suggestion") {
                            // Note down the type of the attribute
                            f.kind = DiagnosticKind::Suggestion;
                        } else {
                            proc_macro_error::Diagnostic::spanned(p.span(), proc_macro_error::Level::Error, format!("Unknown attribute '{}' for '#[diag(...)]'", p.get_ident().map(|i| i.to_string()).unwrap_or("<unknown>".into()))).emit();
                        },

                        Meta::List(l) => {
                            proc_macro_error::Diagnostic::spanned(l.path.span(), proc_macro_error::Level::Error, format!("Unknown attribute '{}' for '#[diag(...)]'", l.path.get_ident().map(|i| i.to_string()).unwrap_or("<unknown>".into()))).emit();
                        },

                        Meta::NameValue(nv) => if nv.path.is_ident("code") {
                            // Parse the value as a string literal
                            if let Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) = nv.value {
                                f.code = Some(s.value());
                            } else {
                                proc_macro_error::Diagnostic::spanned(nv.value.span(), proc_macro_error::Level::Error, "Expected string literal".into()).emit();
                                continue;
                            }
                        } else {
                            proc_macro_error::Diagnostic::spanned(nv.path.span(), proc_macro_error::Level::Error, format!("Unknown attribute '{}' for '#[diag(...)]'", nv.path.get_ident().map(|i| i.to_string()).unwrap_or("<unknown>".into()))).emit();
                        },
                    }
                }
            },

            // The rest we are not looking for
            Meta::Path(_)      |
            Meta::NameValue(_) => {},
        }
    }

    // Assert some mandatory fields are there
    for (f, diag) in &fields {
        if f.kind == DiagnosticKind::Unspecified {
            return Err(proc_macro_error::Diagnostic::spanned(span, proc_macro_error::Level::Error, "No diagnostic kind specified".into())
                .span_note(*diag, "Add either 'error', 'warn', 'note' or 'suggestion' to this attribute".into()))
        }
    }

    // Done, return the struct
    Ok(fields.into_iter().map(|(f, _)| f).collect())
}

/// Parses a struct or enum body.
/// 
/// # Arguments
/// - `tattrs`: Any toplevel attributes that we have to take into account.
/// - `fattrs`: The attributes of the given fields that we should parse. Note that there is one per diagnostic to generate in this chain of diagnostics.
/// - `fields`: The [`Fields`] to parse.
/// 
/// # Returns
/// A parsed [`DiagnosticInfo`] that contains what we need to generate the implementation.
/// 
/// # Errors
/// This function may errors if something about the fields (probably attributes) was invalid.
fn parse_fields(_tattrs: &ToplevelAttributes, fattrs: Vec<FieldAttributes>, fields: Fields) -> Result<DiagnosticInfo, proc_macro_error::Diagnostic> {
    // Done, return the info
    Ok(DiagnosticInfo {
        
    })
}





/***** HELPERS *****/
/// Defines the toplevel attributes we like to learn.
struct ToplevelAttributes {
    
}
impl ToplevelAttributes {
    /// Creates an empty instance that can be populated as attributes pop up their heads.
    /// 
    /// # Returns
    /// A new instance of Self with everything initialized to default.
    #[inline]
    fn empty() -> Self {
        Self {
            
        }
    }
}



/// Defines the field-level attributes we like to learn.
struct FieldAttributes {
    /// The kind of diagnostic we are parsing.
    kind : DiagnosticKind,
    /// The code to set, if any.
    code : Option<String>,
}
impl FieldAttributes {
    /// Creates an empty attributes that can be populated as attributes pop up their heads.
    /// 
    /// # Returns
    /// A new instance of Self with everything initialized to default.
    #[inline]
    fn empty() -> Self {
        Self {
            kind : DiagnosticKind::Unspecified,
            code : None,
        }
    }
}

/// Defines the possible types of diagnostic we have.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DiagnosticKind {
    Error,
    Warn,
    Note,
    Suggestion,

    // Special value for it being unitialized
    Unspecified,
}



/// Defines the information we extract from the body of a struct or enum.
struct DiagnosticInfo {
    
}
impl DiagnosticInfo {
    
}





/***** LIBRARY *****/
/// Takes the parsed struct or enum and implements [`Into<Diagnostic>`] for it.
/// 
/// # Arguments
/// - `ident`: The identifier of the parsed struct/enum/w/e.
/// - `data`: The contents of the parsed struct/enum/w/e.
/// - `attrs`: The list of attributes parsed from the main struct/enum/w/e.
/// - `generics`: The generics part of this struct/enum/w/e/.
/// - `vis`: The visibility markers for this struct/enum/w/e.
/// 
/// # Errors
/// This function may error if any of the attributes were ill-formed.
pub fn derive(ident: Ident, data: Data, attrs: Vec<Attribute>, generics: Generics, vis: Visibility) -> Result<TokenStream, proc_macro_error::Diagnostic> {
    // Read the given struct and extract _everything_ we need
    let diags: Vec<DiagnosticInfo> = match data {
        Data::Struct(s) => {
            // Assert the type of variant (struct, tuple or unit) is supported
            match s.fields {
                // These are supported
                Fields::Named(_)   |
                Fields::Unnamed(_) => {},

                // The other cases are also unsupported
                Fields::Unit => { return Err(proc_macro_error::Diagnostic::spanned(ident.span(), proc_macro_error::Level::Error, "Cannot derive a Diagnostic on a struct without fields".into())); },
            };

            // Now we're happy with the thing we're deriving over, we can extract toplevel attributes (if any)
            let tattrs: ToplevelAttributes = parse_toplevel_attrs(&attrs)?;
            // Derive the field-level attributes
            let fattrs: Vec<FieldAttributes> = parse_field_attrs(attrs, ident.span())?;
            if fattrs.is_empty() {
                return Err(proc_macro_error::Diagnostic::spanned(ident.span(), proc_macro_error::Level::Error, "Missing '#[diag(...)]' attribute on struct".into()));
            };
            // Then extract the attributes of the diag itself (which are the same attributes)
            let info: DiagnosticInfo = parse_fields(&tattrs, fattrs, s.fields)?;

            // Done!
            vec![ info ]
        },
        Data::Enum(e) => {
            // The rest is field-specific, so we can parse toplevel attributes already
            let tattrs: ToplevelAttributes = parse_toplevel_attrs(attrs)?;

            // Iterate over the existing variants
            let mut diags: Vec<DiagnosticInfo> = Vec::with_capacity(e.variants.len());
            for v in e.variants {
                // Assert the type of variant (struct, tuple or unit) is supported
                match v.fields {
                    // These are supported
                    Fields::Named(_)   |
                    Fields::Unnamed(_) => {},

                    // These are unsupported
                    Fields::Unit => { return Err(proc_macro_error::Diagnostic::spanned(v.ident.span(), proc_macro_error::Level::Error, "Cannot derive a Diagnostic on an enum variant without fields".into())); },
                }

                // Parse the field attributes, then the general thing
                let fattrs: Vec<FieldAttributes> = parse_field_attrs(v.attrs, v.ident.span())?;
                if fattrs.is_empty() { continue; }
                let info: DiagnosticInfo = parse_fields(&tattrs, fattrs, v.fields)?;

                // Ok, add that and continue
                diags.push(info);
            }

            // Done
            diags
        },

        // Not supported
        Data::Union(u) => { return Err(proc_macro_error::Diagnostic::spanned(u.union_token.span, proc_macro_error::Level::Error, "Cannot derive a Diagnostic on a union (only structs and enums are supported)".into())); },
    };
    // Assert we found at least one
    if diags.is_empty() { return Err(proc_macro_error::Diagnostic::spanned(ident.span(), proc_macro_error::Level::Error, "No '#[diag(...)]' attribute occurs on any of the enum fields".into())) }

    // Done!
    Ok(quote!{}.into())
}
