//! Types and functions for reading input interactively from the user.
//!
//! If you build this crate with the `repl-edit` feature enabled (the default),
//! then the read-eval-print loop provides shell-like editing commands and
//! history for your expressions, using the `rustyline` crate. Otherwise, it
//! just reads from the standard input directly. Both versions define a `Prompt`
//! type, with the same interface.

use std::io;
use rustyline;
use rustyline::error::ReadlineError;

pub struct Prompt(rustyline::Editor<()>);

impl Prompt {
    /// Return a new `Prompt` value.
    pub fn new() -> Prompt {
        Prompt(rustyline::Editor::<()>::new())
    }

    /// Read an expression from the user. Return `Ok(Some(input))` if we read
    /// the input successfully, `Ok(None)` on end-of-file, or `Err(e)` otherwise.
    pub fn read_expr(&mut self, prompt: &str) -> io::Result<Option<String>> {
        loop {
            match self.0.readline(prompt) {
                Ok(source) => {
                    self.0.add_history_entry(&source);
                    return Ok(Some(source))
                },
                Err(ReadlineError::Eof) => return Ok(None),
                Err(ReadlineError::Interrupted) => {
                    println!("Interrupted.");
                },
                Err(other) => return Err(io::Error::new(io::ErrorKind::Other, other)),
            }
        }
    }
}
