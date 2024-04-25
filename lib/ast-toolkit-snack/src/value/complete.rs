//  COMPLETE.rs
//    by Lut99
//
//  Created:
//    24 Apr 2024, 10:46:03
//  Last edited:
//    25 Apr 2024, 17:49:06
//  Auto updated?
//    Yes
//
//  Description:
//!   Implements complete versions of char/byte-agnostic
//!   `value`-combinators.
//!
//!   These combinators consider not enough input a hard error.
//

use std::fmt::{Debug, Formatter, Result as FResult};
use std::marker::PhantomData;

use ast_toolkit_span::{Span, SpanRange};

use crate::error_new::{expects_tag, Common, Failure};
use crate::fail::DebugAsRef;
use crate::span::MatchBytes;
use crate::{Combinator, Expects, Result};
