//  DIAGNOSTIC.rs
//    by Lut99
// 
//  Created:
//    05 Jul 2023, 18:16:24
//  Last edited:
//    05 Jul 2023, 18:36:47
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
use syn::{Attribute, Data, Fields, Generics, Ident, FieldsNamed, Visibility};


/***** HELPER FUNCTIONS *****/
/// Extracts the information we want from the toplevel attributes.
/// 
/// # Arguments
/// - `attrs`: The list of attributes given at toplevel.
/// 
/// # Returns
/// A new [`DirectoryAttributes`] struct that contains the parsed information.
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

/// Parses a struct or enum body.
/// 
/// # Arguments
/// - `fields`: The [`FieldsNamed`] to parse.
/// 
/// # Returns
/// A parsed [`DiagnosticInfo`] that contains what we need to generate the implementation.
/// 
/// # Errors
/// This function may errors if something about the fields (probably attributes) was invalid.
fn parse_fields(fields: FieldsNamed) -> Result<DiagnosticInfo, proc_macro_error::Diagnostic> {
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
    pub fn empty() -> Self {
        Self {
            
        }
    }
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
    
}
