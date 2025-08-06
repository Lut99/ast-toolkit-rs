//  FORMATTER.rs
//    by Lut99
//
//  Description:
//!   Adds the [`Formatter`]-class, which implements [`Write`] but such that it
//!   supports additional, AST-printing-friendly features.
//


use std::fmt::{self, Result as FResult, Write};


/***** CONSTANTS *****/
/// Defines the indentation size for the [`Formatter`].
pub const INDENT_SIZE: usize = 4;





/***** LIBRARY *****/
/// Represents a [`console::Style`]-like object, but then customized to conditionally color based
/// on the [`Formatter`].
#[cfg(feature = "color")]
pub struct Style(bool, console::Style);
#[cfg(feature = "color")]
impl Style {
    /// Applies red styling to this object.
    ///
    /// # Returns
    /// A [`Style`] that will make its foreground red.
    #[inline]
    pub const fn red(self) -> Self { Self(self.0, self.1.red()) }

    /// Applies green styling to this object.
    ///
    /// # Returns
    /// A [`Style`] that will make its foreground green.
    #[inline]
    pub const fn green(self) -> Self { Self(self.0, self.1.green()) }

    /// Applies blue styling to this object.
    ///
    /// # Returns
    /// A [`Style`] that will make its foreground blue.
    #[inline]
    pub const fn blue(self) -> Self { Self(self.0, self.1.blue()) }

    /// Applies magenta styling to this object.
    ///
    /// # Returns
    /// A [`Style`] that will make its foreground magenta.
    #[inline]
    pub const fn magenta(self) -> Self { Self(self.0, self.1.magenta()) }

    /// Applies bold styling to this object.
    ///
    /// # Returns
    /// A [`Style`] that will make its colors bold.
    #[inline]
    pub const fn bold(self) -> Self { Self(self.0, self.1.bold()) }

    /// Applies bright styling to this object.
    ///
    /// # Returns
    /// A [`Style`] that will make its foreground colors brighter.
    #[inline]
    pub const fn bright(self) -> Self { Self(self.0, self.1.bright()) }

    /// Applies dim styling to this object.
    ///
    /// # Returns
    /// A [`Style`] that will make its foreground colors dimmer.
    #[inline]
    pub const fn dim(self) -> Self { Self(self.0, self.1.dim()) }
}
#[cfg(feature = "color")]
impl Style {
    /// Applies this Style to a given object.
    ///
    /// # Returns
    /// A [`StyledObject`] that will [`std::fmt::Display`] with ANSI colors.
    #[inline]
    pub fn apply_to<T: fmt::Display>(&self, obj: T) -> console::StyledObject<T> {
        if self.0 { self.1.apply_to(obj) } else { console::Style::new().apply_to(obj) }
    }

    /// Applies this Style to a given (formatted) object.
    ///
    /// # Returns
    /// A [`StyledObject`] that will [`std::fmt::Display`] with ANSI colors.
    #[inline]
    pub fn apply_to_args<'a>(&'_ self, args: fmt::Arguments<'a>) -> console::StyledObject<fmt::Arguments<'a>> {
        if self.0 { self.1.apply_to(args) } else { console::Style::new().apply_to(args) }
    }
}



/// Custom [`fmt::Formatter`]-like formatter for formatting [`DisplayFmt`] things.
pub struct Formatter<'w, W> {
    /// The actual [`Write`]r wrapped.
    fmt: &'w mut W,
    /// Whether color is applied or not.
    #[cfg(feature = "color")]
    color: bool,
    /// The current indentation level.
    indent: usize,
    /// Whether to write indentation on the next call to write.
    write_indent: bool,
}
impl<'w, W> Formatter<'w, W> {
    /// Constructor for the Formatter.
    ///
    /// # Arguments
    /// - `fmt`: Some nested [`Write`]r to write to.
    ///
    /// # Returns
    /// A new Formatter that can be [`Write`]n to.
    #[inline]
    pub const fn new(fmt: &'w mut W) -> Self {
        Self {
            fmt,
            #[cfg(feature = "color")]
            color: false,
            indent: 0,
            write_indent: true,
        }
    }
}
impl<'w, W> Formatter<'w, W> {
    /// Add an indentiation level.
    ///
    /// # Arguments
    /// - `n`: The number of levels to add.
    ///
    /// # Returns
    /// `Self` for chaining.
    #[inline]
    pub fn add_indent(&mut self, n: usize) -> &mut Self {
        self.indent = self.indent.saturating_add(n.saturating_mul(INDENT_SIZE));
        self
    }

    /// Enters a closure that adds indentation levels for the duration of it, and then resets it.
    ///
    /// Note this reset is _hard_. If you further mutate the indentation level, it will simply snap
    /// back to what it was before.
    ///
    /// # Arguments
    /// - `n`: The number of levels to add for the duration of the `closure`.
    /// - `closure`: Some [`FnOnce`] to call that represents the work to do while indented. Note
    ///   that the closure receives this formatter to deal with the mutable borrow.
    ///
    /// # Returns
    /// The output of `closure`.
    #[inline]
    pub fn with_add_indent<R>(&mut self, n: usize, closure: impl FnOnce(&mut Self) -> R) -> R {
        let old_indent: usize = self.indent;
        self.add_indent(n);
        let res = closure(self);
        self.indent = old_indent;
        res
    }

    /// Removes an indentiation level.
    ///
    /// # Arguments
    /// - `n`: The number of levels to remove.
    ///
    /// # Returns
    /// `Self` for chaining.
    #[inline]
    pub fn rem_indent(&mut self, n: usize) -> &mut Self {
        self.indent = self.indent.saturating_sub(n.saturating_mul(INDENT_SIZE));
        self
    }



    /// Sets whether coloring is applied to this formatter.
    ///
    /// If you're not sure, and you're going to print to [stdout](std::io::stdout()) or
    /// [stderr](std::io::stderr()), refer to [`Formatter::use_color_auto_stdout()`] and
    /// [`Formatter::use_color_auto_stderr()`], respectively.
    ///
    /// # Arguments
    /// - `use_color`: Whether to use color.
    ///
    /// # Returns
    /// Self for chaining.
    #[cfg(feature = "color")]
    pub fn use_color(&mut self, use_color: bool) -> &mut Self {
        self.color = use_color;
        self
    }

    /// Sets whether coloring is applied based on whether [stdout](std::io::stdout()) is a TTY or
    /// not.
    ///
    /// If stdout is a TTY, then we assume that a user sits behind it to do more complex things
    /// with it, and who'd like to see color. Else, we're likely being piped, and so we don't
    /// render them.
    ///
    /// Refer to [`console::colors_enabled()`] to find out when exactly colors are enabled.
    ///
    /// # Returns
    /// Self for chaining.
    #[cfg(feature = "color")]
    pub fn use_color_auto_stdout(&mut self) -> &mut Self {
        self.color = console::colors_enabled();
        self
    }

    /// Sets whether coloring is applied based on whether [stderr](std::io::stderr()) is a TTY or
    /// not.
    ///
    /// If stderr is a TTY, then we assume that a user sits behind it to do more complex things
    /// with it, and who'd like to see color. Else, we're likely being piped, and so we don't
    /// render them.
    ///
    /// Refer to [`console::colors_enabled_stderr()`] to find out when exactly colors are enabled.
    ///
    /// # Returns
    /// Self for chaining.
    #[cfg(feature = "color")]
    pub fn use_color_auto_stderr(&mut self) -> &mut Self {
        self.color = console::colors_enabled_stderr();
        self
    }

    /// Returns a [`Style`] that will conditionally apply the given colors.
    ///
    /// # Returns
    /// A custom [`Style`]-wrapped that will, depending on whether colors are enabled or not, can
    /// be used to add ANSI-colors to the serialization of another object.
    #[cfg(feature = "color")]
    #[inline]
    pub fn style(&self) -> Style { Style(self.color, console::Style::new()) }
}
impl<'w, W: Write> Formatter<'w, W> {
    /// Writes the internal indentation level to the internal `W`riter.
    ///
    /// # Errors
    /// This function can error if it failed to write to the `W`riter.
    #[inline]
    pub fn write_indent(&mut self) -> FResult {
        const THIRTY_TWO_SPACES: &str = "                                ";

        // No-op when nothing to do
        if self.indent == 0 {
            return Ok(());
        }

        // Optimization: give a chance for the backend to allocate larger chunks by trying for
        // large chunks and then progressively shrinking down.
        let mut left: usize = self.indent;
        for spaces in &[
            THIRTY_TWO_SPACES,
            &THIRTY_TWO_SPACES[..24],
            &THIRTY_TWO_SPACES[..16],
            &THIRTY_TWO_SPACES[..12],
            &THIRTY_TWO_SPACES[..8],
            &THIRTY_TWO_SPACES[..4],
        ] {
            while left > spaces.len() {
                self.fmt.write_str(spaces)?;
                left -= spaces.len();
            }
        }

        // Write any remaining spaces as individual characters
        while left > 0 {
            self.fmt.write_char(' ')?;
            left -= 1;
        }

        // Done
        Ok(())
    }
}
impl<'w, W: Write> Write for Formatter<'w, W> {
    #[inline]
    fn write_str(&mut self, s: &str) -> FResult {
        /// Returns the position of the first character _after_ a linebreak.
        ///
        /// Does either LF (`\n`) or CRLF (`\r\n`).
        ///
        /// # Returns
        /// Either `linebreak_pos + linebreak_len`, or else [`None`] if no linebreak was found.
        #[inline]
        fn find_linebreak_end(s: &str) -> Option<usize> {
            const CRLF: &str = "\r\n";
            const LF: &str = "\n";
            s.find(CRLF).map(|pos| pos + CRLF.len()).or_else(|| s.find(LF).map(|pos| pos + LF.len()))
        }


        // Write the rest by chunks
        let mut rem = s;
        while let Some(next_line_pos) = find_linebreak_end(rem) {
            // If there's indentation to write, write it first
            if self.write_indent {
                self.write_indent()?;
                self.write_indent = false;
            }

            // Then write everything up (and including) the newline, the newline, and then trim the remaining to
            // the rest after
            self.fmt.write_str(&rem[..next_line_pos])?;
            rem = &rem[next_line_pos..];

            // Now, at this point, we've just written a newline. Remember to write it next time we write
            self.write_indent = true;
        }

        // If there's still some remaining, then write it too but _without_ the indentation
        if !rem.is_empty() {
            // If there's indentation to write, write it first
            if self.write_indent {
                self.write_indent()?;
                self.write_indent = false;
            }
            self.fmt.write_str(rem)?;
            // `write_indent` is definitely false at this point
        }

        // Done
        Ok(())
    }
}
