//  FORMATTER.rs
//    by Lut99
//
//  Description:
//!   Adds the [`Formatter`]-class, which implements [`Write`] but such that it
//!   supports additional, AST-printing-friendly features.
//


#[cfg(feature = "color")]
use std::fmt;
use std::fmt::{Arguments, Result as FResult, Write};
use std::iter::repeat_n;


/***** HELPER MACROS *****/
/// Generates constants necessary to work with the lib.
macro_rules! define_indent {
    ($ident:literal) => {
        /// Defines the thing we write for eight times the indentation level.
        const EIGHT_TIMES_INDENT: &str = concat!($ident, $ident, $ident, $ident, $ident, $ident, $ident, $ident);
        /// The number of spaces we write for every indentation level.
        pub const INDENT_SIZE: usize = $ident.len();
    };
}





/***** CONSTANTS *****/
define_indent!("    ");





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
    /// A little buffer for remembering what we last wrote.
    trail: Vec<u8>,
    /// The current index in the buffer.
    ///
    /// Together with `trail_len`, this will do ring-buffer like reading and writing of `trail`.
    trail_i: usize,
    /// The current number of bytes occupied in the `trail` buffer.
    ///
    /// Together with `trail_i`, this will do ring-buffer like reading and writing of `trail`.
    trail_len: usize,
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
    pub fn new(fmt: &'w mut W) -> Self {
        Self {
            fmt,
            #[cfg(feature = "color")]
            color: false,
            indent: 0,
            write_indent: true,
            trail: vec![b'\0'; 1],
            trail_i: 0,
            trail_len: 0,
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
        self.indent = self.indent.saturating_add(n);
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
        self.indent = self.indent.saturating_sub(n);
        self
    }



    /// Sets the size of the trailing buffer.
    ///
    /// This will change the number of bytes (at most) returned by [`Formatter::trail()`]. By
    /// default, only the last byte is kept.
    ///
    /// Also note that this can never _shrink_ the buffer. Nothing will happen if the new size you
    /// specify is smaller than or equal to the current size.
    ///
    /// If you _do_ extend the size of the buffer, then any "new" slots are still empty. I.e., the
    /// string returned by [`Formatter::trail()`] still has the old length until more is written.
    ///
    /// # Arguments
    /// - `new_len`: The total length to set the internal trail buffer to. If it's smaller than the
    ///   current length, it won't do anything.
    ///
    /// # Returns
    /// Self for chaining.
    #[inline]
    pub fn extend_trail_len(&mut self, new_len: usize) -> &mut Self {
        // Early quit on no-ops
        let act_trail_len: usize = self.trail.len();
        if new_len <= act_trail_len {
            return self;
        }

        // Extend the buffer size
        self.trail.extend(repeat_n(0, new_len - act_trail_len));

        // That's it! Note that the `trail_len` doesn't have to change (the new bytes are simply
        // unallocated), neither does the `trail_i` (we ain't no writing anything).
        self
    }

    /// Returns the last bytes written to the formatter.
    ///
    /// The maximum number returned is determined by the length of the trail buffer, set by
    /// [`Formatter::extend_trail_len()`].
    ///
    /// # Returns
    /// A [`Vec`] with the bytes last read.
    #[inline]
    pub fn trail(&self) -> Vec<u8> {
        // We need to allocate a new buffer bc the internal one is improperly laid out
        let mut res = vec![0; self.trail_len];
        let mut trail_i: usize = self.trail_i;
        for i in 0..self.trail_len {
            // Read the current byte and copy it over
            res[i] = self.trail[trail_i];
            // Then advance our read pointer, wrapping around the buffer size
            trail_i = (trail_i + 1) % self.trail.len();
        }
        res
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
impl<'w, W> Formatter<'w, W> {
    /// Helper function for remembering that we wrote a given string.
    ///
    /// You can just dump the entire string in there, this function will take care that only that
    /// for which we have space is remembered.
    ///
    /// # Arguments
    /// - `to_remember`: The string to remember that we wrote it.
    #[inline]
    fn _remember_that_we_wrote_that(&mut self, to_remember: &str) {
        let act_trail_len: usize = self.trail.len();
        let mut to_remember: &[u8] = to_remember[to_remember.len().saturating_sub(act_trail_len)..].as_bytes();

        // First, populate the buffer's initial space if we can
        if self.trail_len < act_trail_len {
            // We're not yet done with filling buffer initially; do so
            let add_len: usize = std::cmp::min(act_trail_len - self.trail_len, to_remember.len());
            self.trail[self.trail_len..self.trail_len + add_len].copy_from_slice(&to_remember[..add_len]);
            self.trail_len += add_len;
            to_remember = &to_remember[add_len..];
            if to_remember.is_empty() {
                // We're done!
                return;
            }
        }

        // Then, and/or OR, aadd the rest of the to_remember list
        if self.trail_i + to_remember.len() >= act_trail_len {
            // We'll need to split the write in two, because it would otherwise extend beyond
            // the end of the buffer
            let first_half_len: usize = act_trail_len - self.trail_i;
            // NOTE: This indexing is sound because we know that the `to_remember` is larger than
            // the remaining trail space.
            self.trail[self.trail_i..act_trail_len].copy_from_slice(&to_remember[..first_half_len]);
            self.trail[..(to_remember.len() - first_half_len)].copy_from_slice(&to_remember[first_half_len..]);
            self.trail_i = to_remember.len() - first_half_len;
            self.trail_len = std::cmp::max(self.trail_len, to_remember.len());
        } else {
            // We WON'T need to split the write in two; just blurt it down in one go
            self.trail[self.trail_i..self.trail_i + to_remember.len()].copy_from_slice(to_remember);
            self.trail_i = to_remember.len();
            self.trail_len = std::cmp::max(self.trail_len, to_remember.len());
        }
    }
}
impl<'w, W: Write> Formatter<'w, W> {
    /// Writes the internal indentation level to the internal `W`riter.
    ///
    /// # Errors
    /// This function can error if it failed to write to the `W`riter.
    #[inline]
    pub fn write_indent(&mut self) -> FResult {
        // No-op when nothing to do
        if self.indent == 0 {
            return Ok(());
        }

        // Write the indentations but in optimized chunks
        let mut indents_written: usize = 0;
        for (n, indent) in [
            (8, EIGHT_TIMES_INDENT),
            (4, &EIGHT_TIMES_INDENT[..4 * INDENT_SIZE]),
            (2, &EIGHT_TIMES_INDENT[..2 * INDENT_SIZE]),
            (1, &EIGHT_TIMES_INDENT[..INDENT_SIZE]),
        ] {
            while indents_written <= n {
                self.fmt.write_str(indent)?;
                self._remember_that_we_wrote_that(indent);
                indents_written += n;
            }
        }

        // Done
        Ok(())
    }



    /// Writes something to the formatter but such that it doesn't write it if we wrote it
    /// previously.
    ///
    /// If the thing you are writing exceeds the [trail length](Formatter::extend_trail_len())
    /// (i.e., the number of bytes that the formatter remembers it's written), then we ONLY check
    /// the _first bytes_ of that which you've written. If that matches, then **none** of your fmt
    /// is written.
    ///
    /// Usually, you don't call this function yourself, but instead call the
    /// [`write_dedup!()`](crate::write_dedup!()) macro. If you are calling it manually, use
    /// [`std::format_args!()`] to generate the `args`.
    ///
    /// # Arguments
    /// - `args`: The thing to write.
    ///
    /// # Returns
    /// Whether it was written or not.
    ///
    /// # Errors
    /// This function can error if it failed to write to the internal `W`riter.
    pub fn write_dedup(&mut self, args: Arguments<'_>) -> Result<bool, std::fmt::Error> {
        let act_trail_len: usize = self.trail.len();
        let sargs: String = args.to_string();

        // Check if the common length prefix of `sargs` equals the suffix of `trail`
        let check_len: usize = std::cmp::min(sargs.len(), self.trail_len);
        for (i, b) in sargs[..check_len].bytes().enumerate() {
            let trail_i: usize = (self.trail_i + (self.trail_len - check_len) + i) % act_trail_len;
            if b != self.trail[trail_i] {
                // They differ. Write!
                self.write_str(&sargs)?;
                return Ok(true);
            }
        }

        // If we got here, they are equal. So nothing to do
        Ok(false)
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

            // Then write everything up (and including) the newline
            let to_write: &str = &rem[..next_line_pos];
            rem = &rem[next_line_pos..];
            self.fmt.write_str(to_write)?;
            self._remember_that_we_wrote_that(to_write);

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
            self._remember_that_we_wrote_that(rem);
            // `write_indent` is definitely false at this point
        }

        // Done
        Ok(())
    }
}





/***** TESTS *****/
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_formatter_remember_that_we_wrote_that() {
        let mut buf = String::new();
        let mut fmt = Formatter::new(&mut buf);
        assert_eq!(fmt.trail(), b"");
        fmt._remember_that_we_wrote_that("a");
        assert_eq!(fmt.trail(), b"a");
        fmt._remember_that_we_wrote_that("b");
        assert_eq!(fmt.trail(), b"b");
        fmt.extend_trail_len(5);
        assert_eq!(fmt.trail(), b"b");
        fmt._remember_that_we_wrote_that("c");
        assert_eq!(fmt.trail(), b"bc");
        fmt._remember_that_we_wrote_that("def");
        assert_eq!(fmt.trail(), b"bcdef");
        fmt._remember_that_we_wrote_that("g");
        assert_eq!(fmt.trail(), b"cdefg");
        fmt._remember_that_we_wrote_that("Hello, world!");
        assert_eq!(fmt.trail(), b"orld!");
    }
}
