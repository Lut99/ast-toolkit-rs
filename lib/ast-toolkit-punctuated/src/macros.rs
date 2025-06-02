//  MACROS.rs
//    by Lut99
//
//  Description:
//!   Defines convenient macros for building [`Punctuated`](crate::Punctuated) lists ergonomically.
//


/***** LIBRARY *****/
/// Macro for initializing a [`Punctuated`](crate::Punctuated) ergonomically.
///
/// You can use this macro by giving a comma-separated list of values interspersed by punctuations,
/// optionally giving a trailing one, much like you would with [`vec![]`](vec!).
///
/// It returns a new [`Punctuated`](crate::Punctuated).
///
/// # Examples
/// ```rust
/// use ast_toolkit_punctuated::{Punctuated, punct};
///
/// assert_eq!(format!("{:?}", punct!["Hello", ',', "world"]), "[\"Hello\", ',', \"world\"]");
/// ```
#[macro_export]
macro_rules! punct {
    /* Counting */
    // Empty base cases
    (__count) => { 0 };
    (__count $v:expr $(, $vn:expr)*) => { 1 + $crate::punct!(__count $($vn),*) };

    /* Internal recursion */
    // Empty base cases
    (__odd($var:ident)) => {};
    (__even($var:ident)) => {};
    // First / third / ... push; values
    (__odd($var:ident) $v:expr $(, $vn:expr)*) => {
        $var.push_value($v);
        $crate::punct!{__even($var) $($vn),*};
    };
    // Second / fourth / ... push; punctuation
    (__even($var:ident) $v:expr $(, $vn:expr)*) => {
        $var.push_punct($v);
        $crate::punct!{__odd($var) $($vn),*};
    };

    /* Outward syntax */
    [$($v:expr),* $(,)?] => {{
        #[allow(unused_mut)]
        let mut __res = $crate::Punctuated::with_capacity((1 + $crate::punct!(__count $($v),*)) / 2);
        $crate::punct!{__odd(__res) $($v),*};
        __res
    }};
}





#[cfg(test)]
mod tests {
    use crate::Punctuated;

    #[test]
    fn test_punct_empty() {
        let p: Punctuated<&str, char> = punct![];
        assert_eq!(p, Punctuated::new());
    }

    #[test]
    fn test_punct_singleton() {
        let p: Punctuated<&str, char> = punct!["Hello"];
        assert_eq!(p.len(), 1);
        assert_eq!(p[0], "Hello");
    }

    #[test]
    fn test_punct_nonempty() {
        let p: Punctuated<&str, char> = punct!["Hello", ',', "world"];
        assert_eq!(p.len(), 2);
        assert_eq!(p[0], "Hello");
        assert_eq!(p.get_punct(0), Some(&','));
        assert_eq!(p[1], "world");
    }
}
