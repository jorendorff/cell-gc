//! Types and functions for reading input interactively from the user.
//!
//! If you build this crate with the `repl-edit` feature enabled (the default),
//! then the read-eval-print loop provides shell-like editing commands and
//! history for your expressions, using the `rustyline` crate. Otherwise, it
//! just reads from the standard input directly. Both versions define a `Prompt`
//! type, with the same interface.

use std::io::{self, Write};

pub struct Prompt;

impl Prompt {
    /// Return a new `Prompt` value.
    pub fn new() -> Prompt {
        Prompt
    }

    /// Read an expression from the user. Return `Ok(Some(input))` if we read
    /// the input successfully, `Ok(None)` on end-of-file, or `Err(e)` otherwise.
    pub fn read_expr(&mut self, prompt: &str) -> io::Result<Option<String>> {
        print!("{}", prompt);
        io::stdout().flush()?;

        let mut source = String::new();
        io::stdin().read_line(&mut source)?;
        if source.is_empty() {
            return Ok(None);
        }
        return Ok(Some(source));
    }
}
