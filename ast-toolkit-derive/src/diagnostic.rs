//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 18:16:24
//  Last edited:
//    12 Aug 2023, 12:11:21
//  Auto updated?
//    Yes
// 
//  Description:
//!   Defines the derivation procedure for the [`Diagnostic`].
//!   
//!   Note that, technically, it's not the [`Diagnostic`] that's being derived,
//!   but rather the [`Into<Diagnostic>`].
// 

use enum_debug::EnumDebug;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span};
use rand::Rng as _;
use rand::distributions::Alphanumeric;
use quote::quote;
use syn::{Attribute, Data, Expr, ExprLit, ExprPath, Fields, Generics, Ident, Lit, Meta, Token, Visibility};
use syn::parse::ParseBuffer;
use syn::spanned::Spanned as _;


/***** HELPER FUNCTIONS *****/
/// Generates a random identifier string.
/// 
/// # Returns
/// A string that can be used as a generated identifier name.
#[inline]
fn generate_random_identifier() -> String {
    format!("___{}", rand::thread_rng().sample_iter(Alphanumeric).take(16).map(char::from).collect::<String>())
}



/// Parses the given expression as if it was a value for the `message`-attribute.
/// 
/// # Arguments
/// - `expr`: The [`Expr`] to parse. If [`None`], then a value of [`StringOrField::Field("message")`] is returned.
/// - `span`: The [`Span`] of the `message`-identifier to pass to any newly created fields.
/// 
/// # Returns
/// The parsed value, as a [`StringOrField`].
/// 
/// # Errors
/// This function may error if the given `expr` was not value for a `message` at all.
fn parse_message_value(expr: Option<Expr>, span: Span) -> Result<StringOrField, proc_macro_error::Diagnostic> {
    // Parse the value as a string literal or a direct identifier
    match expr {
        Some(Expr::Lit(ExprLit { lit: Lit::Str(s), .. })) => Ok(StringOrField::String(s.value())),
        Some(Expr::Path(ExprPath { path, qself: None, .. })) => if let Some(ident) = path.get_ident() {
            Ok(StringOrField::Field(ident.clone()))
        } else {
            Err(proc_macro_error::Diagnostic::spanned(path.span(), proc_macro_error::Level::Error, "Expected string literal or identifier".into()))
        },
        Some(expr) => Err(proc_macro_error::Diagnostic::spanned(expr.span(), proc_macro_error::Level::Error, "Expected string literal or identifier".into())),
        None => Ok(StringOrField::Field(Ident::new("message", span))),
    }
}

/// Parses the given expression as if it was a value for the `code`-attribute.
/// 
/// # Arguments
/// - `expr`: The [`Expr`] to parse. If [`None`], then a value of [`StringOrField::Field("code")`] is returned.
/// - `span`: The [`Span`] of the `code`-identifier to pass to any newly created fields.
/// 
/// # Returns
/// The parsed value, as a [`StringOrField`].
/// 
/// # Errors
/// This function may error if the given `expr` was not value for a `code` at all.
fn parse_code_value(expr: Option<Expr>, span: Span) -> Result<StringOrField, proc_macro_error::Diagnostic> {
    // Parse the value as a string literal or a direct identifier
    match expr {
        Some(Expr::Lit(ExprLit { lit: Lit::Str(s), .. })) => Ok(StringOrField::String(s.value())),
        Some(Expr::Path(ExprPath { path, qself: None, .. })) => if let Some(ident) = path.get_ident() {
            Ok(StringOrField::Field(ident.clone()))
        } else {
            Err(proc_macro_error::Diagnostic::spanned(path.span(), proc_macro_error::Level::Error, "Expected string literal or identifier".into()))
        },
        Some(expr) => Err(proc_macro_error::Diagnostic::spanned(expr.span(), proc_macro_error::Level::Error, "Expected string literal or identifier".into())),
        None => Ok(StringOrField::Field(Ident::new("code", span))),
    }
}

/// Parses the given expression as if it was a value for the `remark`-attribute.
/// 
/// # Arguments
/// - `expr`: The [`Expr`] to parse. If [`None`], then a value of [`StringOrField::Field("remark")`] is returned.
/// - `span`: The [`Span`] of the `remark`-identifier to pass to any newly created fields.
/// 
/// # Returns
/// The parsed value, as a [`StringOrField`].
/// 
/// # Errors
/// This function may error if the given `expr` was not value for a `remark` at all.
fn parse_remark_value(expr: Option<Expr>, span: Span) -> Result<StringOrField, proc_macro_error::Diagnostic> {
    // Parse the value as a string literal or a direct identifier
    match expr {
        Some(Expr::Lit(ExprLit { lit: Lit::Str(s), .. })) => Ok(StringOrField::String(s.value())),
        Some(Expr::Path(ExprPath { path, qself: None, .. })) => if let Some(ident) = path.get_ident() {
            Ok(StringOrField::Field(ident.clone()))
        } else {
            Err(proc_macro_error::Diagnostic::spanned(path.span(), proc_macro_error::Level::Error, "Expected string literal or identifier".into()))
        },
        Some(expr) => Err(proc_macro_error::Diagnostic::spanned(expr.span(), proc_macro_error::Level::Error, "Expected string literal or identifier".into())),
        None => Ok(StringOrField::Field(Ident::new("remark", span))),
    }
}

/// Parses the given expression as if it was a value for the `replace`-attribute.
/// 
/// # Arguments
/// - `expr`: The [`Expr`] to parse. If [`None`], then a value of [`StringOrField::Field("replace")`] is returned.
/// - `span`: The [`Span`] of the `replace`-identifier to pass to any newly created fields.
/// 
/// # Returns
/// The parsed value, as a [`StringOrField`].
/// 
/// # Errors
/// This function may error if the given `expr` was not value for a `replace` at all.
fn parse_replace_value(expr: Option<Expr>, span: Span) -> Result<(StringOrField, Span), proc_macro_error::Diagnostic> {
    match expr {
        Some(Expr::Lit(ExprLit { lit: Lit::Str(s), .. })) => Ok((StringOrField::String(s.value()), span)),
        Some(Expr::Path(ExprPath { path, qself: None, .. })) => if let Some(ident) = path.get_ident() {
            Ok((StringOrField::Field(ident.clone()), span))
        } else {
            Err(proc_macro_error::Diagnostic::spanned(path.span(), proc_macro_error::Level::Error, "Expected string literal or identifier".into()))
        },
        Some(expr) => Err(proc_macro_error::Diagnostic::spanned(expr.span(), proc_macro_error::Level::Error, "Expected string literal or identifier".into())),
        None => Ok((StringOrField::Field(Ident::new("replace", span)), span)),
    }
}

/// Parses the given expression as if it was a value for the `span`-attribute.
/// 
/// # Arguments
/// - `expr`: The [`Expr`] to parse. If [`None`], then a value of [`StringOrField::Field("span")`] is returned.
/// - `span`: The [`Span`] of the `span`-identifier to pass to any newly created fields.
/// 
/// # Returns
/// The parsed field name.
/// 
/// # Errors
/// This function may error if the given `expr` was not value for a `span` at all.
fn parse_span_value(expr: Option<Expr>, span: Span) -> Result<Ident, proc_macro_error::Diagnostic> {
    match expr {
        Some(Expr::Path(ExprPath { path, qself: None, .. })) => if let Some(ident) = path.get_ident() {
            Ok(ident.clone())
        } else {
            Err(proc_macro_error::Diagnostic::spanned(path.span(), proc_macro_error::Level::Error, "Expected identifier".into()))
        },
        Some(expr) => Err(proc_macro_error::Diagnostic::spanned(expr.span(), proc_macro_error::Level::Error, "Expected identifier".into())),
        None => Ok(Ident::new("span", span)),
    }
}



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
    let toplevel: ToplevelAttributes = ToplevelAttributes::empty();
    for a in attrs {
        // Examine the meta found
        match &a.meta {
            Meta::List(l) => if l.path.is_ident("diagnostic") {
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
                    Err(err) => { return Err(proc_macro_error::Diagnostic::spanned(l.tokens.span(), proc_macro_error::Level::Error, "Failed to parse struct/enum arguments".into()).span_error(err.span(), err.to_string())); },
                };

                // Now iterate over them to collect the arguments
                for a in args {
                    match a {
                        Meta::NameValue(nv) => {
                            proc_macro_error::Diagnostic::spanned(nv.path.span(), proc_macro_error::Level::Error, format!("Unknown attribute '{}' for '#[diagnostic(...)]'", nv.path.get_ident().map(|i| i.to_string()).unwrap_or("<unknown>".into()))).emit();
                        },

                        Meta::Path(p) => {
                            proc_macro_error::Diagnostic::spanned(p.span(), proc_macro_error::Level::Error, format!("Unknown attribute '{}' for '#[diagnostic(...)]'", p.get_ident().map(|i| i.to_string()).unwrap_or("<unknown>".into()))).emit();
                        },
                        Meta::List(l) => {
                            proc_macro_error::Diagnostic::spanned(l.path.span(), proc_macro_error::Level::Error, format!("Unknown attribute '{}' for '#[diagnostic(...)]'", l.path.get_ident().map(|i| i.to_string()).unwrap_or("<unknown>".into()))).emit()
                        },
                    }
                }
            },

            // The rest we are not looking for
            Meta::Path(_)      |
            Meta::NameValue(_) => {},
        }
    }

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
    let mut fields: Vec<FieldAttributes> = vec![];
    for a in attrs {
        // Examine the meta found
        match &a.meta {
            Meta::List(l) => if l.path.is_ident("diag") {
                // It's the diagnostic we are looking for *jedi handwaving*

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
                let mut f: FieldAttributes = FieldAttributes::empty(l.path.span(), l.delimiter.span().open());
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
                        } else if p.is_ident("message") {
                            f.message = match parse_message_value(None, p.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            };
                        } else if p.is_ident("code") {
                            f.code = match parse_code_value(None, p.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            };
                        } else if p.is_ident("remark") {
                            f.remark = match parse_remark_value(None, p.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            };
                        } else if p.is_ident("replace") {
                            f.replace = match parse_replace_value(None, p.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            }
                        } else if p.is_ident("span") {
                            f.span = match parse_span_value(None, p.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            };
                        } else {
                            proc_macro_error::Diagnostic::spanned(p.span(), proc_macro_error::Level::Error, format!("Unknown attribute '{}' for '#[diag(...)]'", p.get_ident().map(|i| i.to_string()).unwrap_or("<unknown>".into()))).emit();
                        },
                        
                        Meta::NameValue(nv) => if nv.path.is_ident("message") {
                            f.message = match parse_message_value(Some(nv.value), nv.path.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            };
                        } else if nv.path.is_ident("code") {
                            f.code = match parse_code_value(Some(nv.value), nv.path.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            };
                        } else if nv.path.is_ident("remark") {
                            f.remark = match parse_remark_value(Some(nv.value), nv.path.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            };
                        } else if nv.path.is_ident("replace") {
                            f.replace = match parse_replace_value(Some(nv.value), nv.path.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            }
                        } else if nv.path.is_ident("span") {
                            f.span = match parse_span_value(Some(nv.value), nv.path.span()) {
                                Ok(res) => Some(res),
                                Err(err) => { err.emit(); continue; },
                            };
                        } else {
                            proc_macro_error::Diagnostic::spanned(nv.path.span(), proc_macro_error::Level::Error, format!("Unknown attribute '{}' for '#[diag(...)]'", nv.path.get_ident().map(|i| i.to_string()).unwrap_or("<unknown>".into()))).emit();
                        },

                        Meta::List(l) => {
                            proc_macro_error::Diagnostic::spanned(l.path.span(), proc_macro_error::Level::Error, format!("Unknown attribute '{}' for '#[diag(...)]'", l.path.get_ident().map(|i| i.to_string()).unwrap_or("<unknown>".into()))).emit();
                        },
                    }
                }

                // Assert some properties are there
                if f.kind == DiagnosticKind::Unspecified {
                    return Err(proc_macro_error::Diagnostic::spanned(span, proc_macro_error::Level::Error, "No diagnostic kind specified".into())
                        .span_note(span, "Add either 'error', 'warn', 'note' or 'suggestion' to this attribute".into()))
                }

                // With all the attributes we care about parsed, add it to the list
                fields.push(f);
            },

            // The rest we are not looking for
            Meta::Path(_)      |
            Meta::NameValue(_) => {},
        }
    }

    // Done, return the struct
    Ok(fields)
}

/// Parses a struct or enum body.
/// 
/// # Arguments
/// - `fkind`: The kind of fields for which we will be parsing.
/// - `tattrs`: Any toplevel attributes that we have to take into account.
/// - `fattrs`: The attributes of the given fields that we should parse. Note that there is one per diagnostic to generate in this chain of diagnostics.
/// - `variant`: The [`Ident`]ifier of the variant.
/// - `fields`: The [`Fields`] to parse.
/// 
/// # Returns
/// A parsed [`DiagnosticInfo`] that contains what we need to generate the implementation.
/// 
/// # Errors
/// This function may errors if something about the fields (probably attributes) was invalid.
fn parse_fields(fkind: FieldKind, _tattrs: &ToplevelAttributes, fattrs: Vec<FieldAttributes>, variant: Option<Ident>, fields: Fields) -> Result<DiagnosticInfo, proc_macro_error::Diagnostic> {
    // Collect the available fields
    let fs: Vec<Ident> = if let FieldKind::TupleEnum = fkind {
        fields.into_iter().map(|f| Ident::new(&generate_random_identifier(), f.ty.span())).collect()
    } else {
        fields.into_iter().enumerate().map(|(i, f)| f.ident.unwrap_or_else(|| Ident::new(&i.to_string(), f.ty.span()))).collect()
    };

    // Iterate over the found attributes
    let mut diag: Option<DiagnosticInfo> = None;
    for attr in fattrs {
        // Resolve the message
        let message: TripleStrategy = match attr.message {
            Some(StringOrField::String(s)) => TripleStrategy::String(s),
            Some(StringOrField::Field(f))  => { if fs.iter().find(|f2| f.to_string() == f2.to_string()).is_none() { return Err(proc_macro_error::Diagnostic::spanned(f.span(), proc_macro_error::Level::Error, format!("Field `{}` not found", f.to_string()))); }; TripleStrategy::Field(f) },
            None                           => TripleStrategy::Display,
        };

        // Resolve the code
        let code: Option<DuoStrategy> = match attr.code {
            Some(StringOrField::String(s)) => Some(DuoStrategy::String(s)),
            Some(StringOrField::Field(f))  => { if fs.iter().find(|f2| f.to_string() == f2.to_string()).is_none() { return Err(proc_macro_error::Diagnostic::spanned(f.span(), proc_macro_error::Level::Error, format!("Field `{}` not found", f.to_string()))); }; Some(DuoStrategy::Field(f)) },
            None                           => None,
        };
        // Resolve the note
        let remark: Option<DuoStrategy> = match attr.remark {
            Some(StringOrField::String(s)) => Some(DuoStrategy::String(s)),
            Some(StringOrField::Field(f))  => { if fs.iter().find(|f2| f.to_string() == f2.to_string()).is_none() { return Err(proc_macro_error::Diagnostic::spanned(f.span(), proc_macro_error::Level::Error, format!("Field `{}` not found", f.to_string()))); }; Some(DuoStrategy::Field(f)) },
            None                           => None,
        };
        // Resolve the suggestion
        let suggestion: Option<DuoStrategy> = match attr.replace {
            Some((suggestion, span)) => if attr.kind == DiagnosticKind::Suggestion {
                match suggestion {
                    StringOrField::String(s) => Some(DuoStrategy::String(s)),
                    StringOrField::Field(f)  => { if fs.iter().find(|f2| f.to_string() == f2.to_string()).is_none() { return Err(proc_macro_error::Diagnostic::spanned(f.span(), proc_macro_error::Level::Error, format!("Field `{}` not found", f.to_string()))); }; Some(DuoStrategy::Field(f)) },
                }
            } else {
                proc_macro_error::Diagnostic::spanned(span, proc_macro_error::Level::Warning, "`replace` field is ignored for non-Suggestion diagnostics".into()).emit();
                None
            },
            None => if attr.kind == DiagnosticKind::Suggestion {
                // See if there is an identifier to set
                match fs.iter().find(|f| f.to_string() == "replace") {
                    Some(f) => Some(DuoStrategy::Field(f.clone())),
                    None => {
                        let full_span: Option<Span> = attr.diag.join(attr.lparen);
                        return Err(proc_macro_error::Diagnostic::spanned(attr.diag, proc_macro_error::Level::Error, "No `replace`-field defined in struct".into())
                            .span_suggestion(full_span.unwrap_or_else(|| attr.lparen), "Add a `replace = \"<message>\"` argument to set the message manually", if full_span.is_some() { "diag(replace = \"foo()\", ".into() } else { "(replace = \"foo()\", ".into() })
                            .span_suggestion(full_span.unwrap_or_else(|| attr.lparen), "Add a `replace = <field>` argument to refer to one of the struct's fields", if full_span.is_some() { "diag(replace = foo, ".into() } else { "(replace = foo, ".into() }));
                    }
                }
            } else {
                None
            },
        };
        // Resolve the span
        let span: Ident = match attr.span {
            Some(f) => { if fs.iter().find(|f2| f.to_string() == f2.to_string()).is_none() { proc_macro_error::Diagnostic::spanned(f.span(), proc_macro_error::Level::Error, format!("Field `{}` not found", f.to_string())).emit(); continue; }; f },
            None    => match fs.iter().find(|f| f.to_string() == "span") {
                Some(f) => f.clone(),
                None => {
                    let full_span: Option<Span> = attr.diag.join(attr.lparen);
                    return Err(proc_macro_error::Diagnostic::spanned(attr.diag, proc_macro_error::Level::Error, "No `span`-field defined in struct".into())
                        .span_suggestion(full_span.unwrap_or_else(|| attr.lparen), "Add a `span = <field>` argument to refer to one of the struct's fields", if full_span.is_some() { "diag(span = foo, ".into() } else { "(span = foo, ".into() }));
                },
            },
        };

        // OK - create the diagnostic info
        let mut new_diag: DiagnosticInfo = DiagnosticInfo {
            variant : variant.clone().map(|v| (v, fs.clone())),

            kind : attr.kind,
            message,
            code,
            remark,
            suggestion,
            span,

            sub : vec![],
        };

        // Now, let us potentially rewrite the tokens if we're a tuple enum variant
        if let FieldKind::TupleEnum = fkind {
            if let TripleStrategy::Field(f) = &mut new_diag.message {
                // Extract the name as an index
                let i: usize = match usize::from_str_radix("", 10) {
                    Ok(i) => i,
                    Err(_) => { proc_macro_error::Diagnostic::spanned(f.span(), proc_macro_error::Level::Error, "Given field name is not a tuple index (i.e., a number)".into()).emit(); continue; },
                };

                // Get the matching identifier
                *f = match fs.iter().nth(i) {
                    Some(f) => f.clone(),
                    None => { proc_macro_error::Diagnostic::spanned(f.span(), proc_macro_error::Level::Error, format!("Field `{i}` does not refer to an existing field")).emit(); continue; },
                };
            }
        }

        // We can push it to the better diags
        if let Some(diag) = &mut diag {
            diag.sub.push(new_diag);
        } else {
            diag = Some(new_diag);
        }
    }

    // Done, return the info
    // SAFETY: We can unwrap without checking here because we already assert that `fields` is not empty when calling this function
    Ok(diag.unwrap())
}

/// Generates the construction logic for a single [`DiagnosticInfo`].
/// 
/// # Arguments
/// - `kind`: The [`FieldKind`] that describes for what type of field we are generating. This can be a named struct, tuple struct, named enum variant or tuple enum variant.
/// - `value`: The [`Ident`]ifier that we use to refer to the would-be-called `value`-field in the [`From`].
/// - `info`: The [`DiagnosticInfo`] that represents the information we need to build the constructor of a single struct/variant.
/// 
/// # Returns
/// A tuple with the variant for which constructor is created (if any), the fields necessary to build this kind and the [`TokenStream`] with generated code for the constructor.
fn generate_constructor(fkind: FieldKind, value: &Ident, info: DiagnosticInfo) -> (Option<(Ident, Vec<Ident>)>, TokenStream2) {
    // Unwrap the diagnostic info
    let DiagnosticInfo { variant, kind, message, code, remark, suggestion, span, sub } = info;

    // Resolve the strategies
    let message: TokenStream2 = match message {
        TripleStrategy::String(s) => quote! { format!(#s) },
        TripleStrategy::Field(f) => match fkind {
            FieldKind::NamedStruct |
            FieldKind::TupleStruct => quote! { &#value.#f },
            FieldKind::NamedEnum   |
            FieldKind::TupleEnum   => quote! { #f },
        },
        TripleStrategy::Display => { let sfmt: String = format!("{{{value}}}"); quote! { format!(#sfmt) } },
    };
    // let message: TokenStream2 = quote!{ "Hello there!" };
    let code: Vec<TokenStream2> = if let Some(code) = code {
        match code {
            DuoStrategy::String(s) => vec![ quote! { format!(#s) } ],
            DuoStrategy::Field(f) => match fkind {
                FieldKind::NamedStruct |
                FieldKind::TupleStruct => vec![ quote! { &#value.#f } ],
                FieldKind::NamedEnum   |
                FieldKind::TupleEnum   => vec![ quote! { #f } ],
            },
        }
    } else {
        vec![]
    };
    // let code: Option<TokenStream2> = None;
    let remark: Vec<TokenStream2> = if let Some(remark) = remark {
        match remark {
            DuoStrategy::String(s) => vec![ quote! { format!(#s) } ],
            DuoStrategy::Field(f) => match fkind {
                FieldKind::NamedStruct |
                FieldKind::TupleStruct => vec![ quote! { &#value.#f } ],
                FieldKind::NamedEnum   |
                FieldKind::TupleEnum   => vec![ quote! { #f } ],
            },
        }
    } else {
        vec![]
    };
    // let note: Option<TokenStream2> = None;
    let suggestion: Option<TokenStream2> = suggestion.map(|suggestion| match suggestion {
        DuoStrategy::String(s) => quote! { format!(#s) },
        DuoStrategy::Field(f) => match fkind {
            FieldKind::NamedStruct |
            FieldKind::TupleStruct => quote! { &#value.#f },
            FieldKind::NamedEnum   |
            FieldKind::TupleEnum   => quote! { #f },
        },
    });
    // let suggestion: Option<TokenStream2> = suggestion.map(|_| quote!{ "Code" });
    let span: TokenStream2 = match fkind {
        FieldKind::NamedStruct |
        FieldKind::TupleStruct => quote! { &#value.#span },
        FieldKind::NamedEnum   |
        FieldKind::TupleEnum   => quote! { #span },
    };

    // Construct the constructor!
    let main: TokenStream2 = match kind {
        DiagnosticKind::Error => quote! {
            ::ast_toolkit::diagnostic::Diagnostic::error(
                #message,
                #span,
            )
            #(.set_code(#code))*
            #(.set_remark(#remark))*
        },

        DiagnosticKind::Warn => quote! {
            ::ast_toolkit::diagnostic::Diagnostic::warn(
                #message,
                #span,
            )
            #(.set_code(#code))*
            #(.set_remark(#remark))*
        },

        DiagnosticKind::Note => quote! {
            ::ast_toolkit::diagnostic::Diagnostic::note(
                #message,
                #span,
            )
            #(.set_code(#code))*
            #(.set_remark(#remark))*
        },

        DiagnosticKind::Suggestion => {
            let suggestion: TokenStream2 = suggestion.expect("Suggestion was not given in a suggestion diagnostic; this should never happen!");
            quote! {
                ::ast_toolkit::diagnostic::Diagnostic::suggestion(
                    #message,
                    #span,
                    #suggestion,
                )
                #(.set_code(#code))*
                #(.set_remark(#remark))*
            }
        },

        DiagnosticKind::Unspecified => { unreachable!(); },
    };

    // Next up, push additional diagnostics (recursively) and that's it!
    let sub: Vec<TokenStream2> = sub.into_iter().map(|i| generate_constructor(fkind, value, i).1).collect();
    (variant, quote! {
        #main
        #(.add(#sub))*
    })
}





/***** HELPERS *****/
/// Defines what kind of field we can be generating for.
#[derive(Clone, Copy, Debug, EnumDebug, Eq, PartialEq)]
enum FieldKind {
    NamedStruct,
    TupleStruct,
    NamedEnum,
    TupleEnum,
}



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
    /// The span of the 'diag' token.
    diag   : Span,
    /// The span of the left parenthesis following the diag.
    lparen : Span,

    /// The kind of diagnostic we are parsing.
    kind    : DiagnosticKind,
    /// The message given by the user.
    message : Option<StringOrField>,
    /// The code to set, if any.
    code    : Option<StringOrField>,
    /// The note to set, if any.
    remark  : Option<StringOrField>,
    /// The suggestion to give, if any.
    replace : Option<(StringOrField, Span)>,
    /// Any explicit span given by the user, which always refers to a field.
    span    : Option<Ident>,
}
impl FieldAttributes {
    /// Creates an empty attributes that can be populated as attributes pop up their heads.
    /// 
    /// # Arguments
    /// - `diag`: The [`Span`] of the `diag` token in `#[diag(...)]`.
    /// - `lparen`: The [`Span`] of the left parenthesis token in `#[diag(...)]`.
    /// 
    /// # Returns
    /// A new instance of Self with everything initialized to default.
    #[inline]
    fn empty(diag: Span, lparen: Span) -> Self {
        Self {
            diag,
            lparen,

            kind       : DiagnosticKind::Unspecified,
            message    : None,
            remark     : None,
            code       : None,
            replace : None,
            span       : None,
        }
    }
}

/// Defines the possible types of diagnostic we have.
#[derive(Clone, Copy, Debug, EnumDebug, Eq, PartialEq)]
enum DiagnosticKind {
    Error,
    Warn,
    Note,
    Suggestion,

    // Special value for it being unitialized
    Unspecified,
}

/// Defines that a value is either a direct string or that it refers to a field which contains the string.
#[derive(Clone, Debug, EnumDebug)]
enum StringOrField {
    /// It's a literal string
    String(String),
    /// It's a field reference
    Field(Ident),
}



/// Defines the information we extract from the body of a struct or enum.
struct DiagnosticInfo {
    /// Contains the identifier of the variant and the fields for which this diagnostic holds, if any.
    variant : Option<(Ident, Vec<Ident>)>,

    /// The kind of diagnostic we are parsing.
    kind       : DiagnosticKind,
    /// The message given by the user.
    message    : TripleStrategy,
    /// The code to set, if any.
    code       : Option<DuoStrategy>,
    /// The note to set, if any.
    remark     : Option<DuoStrategy>,
    /// The suggestion to give, if any. Is guaranteed to be `None` if `kind` is _not_ a [`DiagnosticKind::Suggestion`].
    suggestion : Option<DuoStrategy>,
    /// Any explicit span given by the user, which always refers to a field.
    span       : Ident,

    /// Any nested diagnostics
    sub : Vec<Self>,
}

/// Defines the possible strategies for building the `message`-field in a [`Diagnostic`].
#[derive(Clone, Debug, EnumDebug)]
enum TripleStrategy {
    /// We're given a string
    String(String),
    /// We're given a field to refer
    Field(Ident),
    /// We're given nothing, so we take Display
    Display,
}

/// Defines the possible strategies for building certain fields in a [`Diagnostic`].
#[derive(Clone, Debug, EnumDebug)]
enum DuoStrategy {
    /// We're given a string
    String(String),
    /// We're given a field to refer
    Field(Ident),
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
pub fn derive(ident: Ident, data: Data, attrs: Vec<Attribute>, generics: Generics, _vis: Visibility) -> Result<TokenStream, proc_macro_error::Diagnostic> {
    // Read the given struct and extract _everything_ we need
    let is_struct: bool = matches!(data, Data::Struct(_));
    let (_top_attrs, mut diags): (ToplevelAttributes, Vec<(FieldKind, DiagnosticInfo)>) = match data {
        Data::Struct(s) => {
            // Assert the type of variant (struct, tuple or unit) is supported
            let fkind: FieldKind = match s.fields {
                // These are supported
                Fields::Named(_)   => FieldKind::NamedStruct,
                Fields::Unnamed(_) => FieldKind::TupleStruct,

                // The other cases are also unsupported
                Fields::Unit => { return Err(proc_macro_error::Diagnostic::spanned(ident.span(), proc_macro_error::Level::Error, "Cannot derive a Diagnostic on a struct without fields".into())); },
            };

            // Now we're happy with the thing we're deriving over, we can extract toplevel attributes (if any)
            let tattrs: ToplevelAttributes = parse_toplevel_attrs(&attrs)?;
            // Derive the field-level attributes
            let fattrs: Vec<FieldAttributes> = parse_field_attrs(attrs, ident.span())?;
            if fattrs.is_empty() {
                return Err(proc_macro_error::Diagnostic::spanned(ident.span(), proc_macro_error::Level::Error, "Missing `#[diag(...)]` attribute on struct".into()));
            };
            // Then extract the attributes of the diag itself (which are the same attributes)
            let info: DiagnosticInfo = parse_fields(fkind, &tattrs, fattrs, None, s.fields)?;

            // Done!
            (tattrs, vec![ (fkind, info) ])
        },
        Data::Enum(e) => {
            // The rest is field-specific, so we can parse toplevel attributes already
            let tattrs: ToplevelAttributes = parse_toplevel_attrs(attrs)?;

            // Iterate over the existing variants
            let mut diags: Vec<(FieldKind, DiagnosticInfo)> = Vec::with_capacity(e.variants.len());
            for v in e.variants {
                // Assert the type of variant (struct, tuple or unit) is supported
                let fkind: FieldKind = match v.fields {
                    // These are supported
                    Fields::Named(_)   => FieldKind::NamedEnum,
                    Fields::Unnamed(_) => FieldKind::TupleEnum,

                    // These are unsupported
                    Fields::Unit => { proc_macro_error::Diagnostic::spanned(v.ident.span(), proc_macro_error::Level::Error, "Cannot derive a Diagnostic on an enum variant without fields".into()).emit(); continue; },
                };

                // Parse the field attributes, then the general thing
                let fattrs: Vec<FieldAttributes> = match parse_field_attrs(v.attrs, v.ident.span()) {
                    Ok(attrs) => attrs,
                    Err(err)  => { err.emit(); continue; }
                };
                if fattrs.is_empty() { continue; }
                let info: DiagnosticInfo = match parse_fields(fkind, &tattrs, fattrs, Some(v.ident), v.fields) {
                    Ok(attrs) => attrs,
                    Err(err)  => { err.emit(); continue; }
                };

                // Ok, add that and continue
                diags.push((fkind, info));
            }

            // Done
            (tattrs, diags)
        },

        // Not supported
        Data::Union(u) => { return Err(proc_macro_error::Diagnostic::spanned(u.union_token.span, proc_macro_error::Level::Error, "Cannot derive a Diagnostic on a union (only structs and enums are supported)".into())); },
    };
    // Assert we found at least one
    if diags.is_empty() { return Err(proc_macro_error::Diagnostic::spanned(ident.span(), proc_macro_error::Level::Error, "No '#[diag(...)]' attribute occurs on any of the enum fields".into())) }

    // Now generate the construction function for this one
    let value_ident: Ident = Ident::new(&generate_random_identifier(), Span::call_site());
    let construct: TokenStream2 = if is_struct {
        // It's a struct
        let (fkind, diag) = diags.swap_remove(0);
        // NOTE: No variant/fields are required here, since it's all accessed through `self`.
        generate_constructor(fkind, &value_ident, diag).1
    } else {
        // It's an enum

        // Build all of the individual constructors first
        let mut variants: Vec<TokenStream2> = Vec::with_capacity(diags.len());
        for (fkind, info) in diags {
            // Get the constructor
            let (variant, code): (Option<(Ident, Vec<Ident>)>, TokenStream2) = generate_constructor(fkind, &value_ident, info);

            // Generate the match for the variant
            let (name, fields): (Ident, Vec<Ident>) = variant.unwrap();
            match fkind {
                FieldKind::NamedEnum => {
                    variants.push(quote! {
                        #ident::#name{ #(#fields),* } => { #code },
                    });
                },
                FieldKind::TupleEnum => {
                    variants.push(quote! {
                        #ident::#name( #(#fields),* ) => { #code },
                    });
                },
                _ => { panic!("Cannot be generating code for an enum while FieldKind is {:?}!", fkind.variant()); },
            }
        }

        // Wrap that in a big match, one for every variant
        quote!{
            match #value_ident {
                #(#variants)*
                _ => { panic!("Cannot call Into<Diagnostic> for unspecified enum variant"); },
            }
        }
    };

    // Finally, generate the implementation
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    Ok(quote! {
        #[automatically_derived]
        impl #impl_generics From<#ident #type_generics> for ::ast_toolkit::diagnostic::Diagnostic #where_clause {
            #[inline]
            fn from(value: #ident #type_generics) -> Self { Self::from(&value) }
        }
        #[automatically_derived]
        #[allow(non_snake_case)]
        impl #impl_generics From<&#ident #type_generics> for ::ast_toolkit::diagnostic::Diagnostic #where_clause {
            fn from(#value_ident: &#ident #type_generics) -> Self { #construct }
        }
        #[automatically_derived]
        impl #impl_generics From<&mut #ident #type_generics> for ::ast_toolkit::diagnostic::Diagnostic #where_clause {
            #[inline]
            fn from(value: &mut #ident #type_generics) -> Self { Self::from(&*value) }
        }
    }.into())
}
