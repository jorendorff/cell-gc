use errors::*;
use std;
use std::fmt;
use std::io::{self, BufRead, Write};
use std::marker::PhantomData;
use std::sync::{Arc, Mutex};
use cell_gc::GcHeapSession;
use value::Value;


// Traits

pub trait TextualInputPort: Send {
    fn read<'h>(&mut self, hs: &mut GcHeapSession<'h>) -> Result<Value<'h>>;
    fn read_char(&mut self) -> Result<Option<char>>;
    fn peek_char(&mut self) -> Result<Option<char>>;
    fn read_line(&mut self) -> Result<String>;
    fn is_char_ready(&mut self) -> Result<bool>;
    fn read_string(&mut self, k: usize) -> Result<String>;
}

pub trait BinaryInputPort: Send {
    fn read_u8(&mut self) -> Result<Option<u8>>;
    fn peek_u8(&mut self) -> Result<Option<u8>>;
    fn read_into(&mut self, buf: &mut [u8]) -> Result<Option<usize>>;
}

pub trait AbstractPort: Send + 'static {
    fn is_input(&self) -> bool { self.is_textual_input() || self.is_binary_input() }

    fn is_textual_input(&self) -> bool { false }

    fn as_open_textual_input(&mut self) -> Result<&mut TextualInputPort> {
        Err("textual input port required".into())
    }

    fn is_binary_input(&self) -> bool { false }

    fn as_open_binary_input(&mut self) -> Result<&mut BinaryInputPort> {
        Err("binary input port required".into())
    }

    fn is_input_open(&self) -> Result<bool> {
        Err("input-port-open?: not an input port".into())
    }

    fn close_input(&mut self) -> Result<()> {
        Err("close-input-port: not an input port".into())
    }

    fn is_output(&self) -> bool { self.is_textual_output() || self.is_binary_output() }

    fn is_textual_output(&self) -> bool { false }

    fn as_open_textual_output(&mut self) -> Result<&mut Write> {
        Err("textual output port required".into())
    }

    fn get_output_string(&self) -> Result<String> {
        Err("get-output-string: port was not created with open-output-string".into())
    }

    fn is_binary_output(&self) -> bool { false }

    fn as_open_binary_output(&mut self) -> Result<&mut Write> {
        Err("binary output port required".into())
    }

    fn get_output_bytevector(&self) -> Result<Vec<u8>> {
        Err("get-output-bytevector: port was not created with open-output-bytevector".into())
    }

    fn is_output_open(&self) -> Result<bool> {
        Err("output-port-open?: not an input port".into())
    }

    fn close_output(&mut self) -> Result<()> {
        Err("close-output-port: not an output port".into())
    }

    fn close(&mut self) -> Result<()>;
}


// Concrete types

#[derive(Debug, IntoHeap)]
pub struct Port<'h> {
    pub port_arc: Arc<Mutex<AbstractPort + 'static>>,
    pub phantom: PhantomData<&'h u8>,
}


// Port basics

fn port_into_value<'h, P: AbstractPort>(hs: &mut GcHeapSession<'h>, port: P) -> Value<'h> {
    Value::Port(hs.alloc(Port {
        port_arc: Arc::new(Mutex::new(port)),
        phantom: PhantomData,
    }))
}

impl fmt::Debug for AbstractPort + 'static {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "AbstractPort")
    }
}

impl<'h> PortRef<'h> {
    pub fn is_input(&self) -> bool {
        let arc = self.port_arc();
        let guard = arc.lock().expect("port is poisoned");
        guard.is_input()
    }

    pub fn is_output(&self) -> bool {
        let arc = self.port_arc();
        let guard = arc.lock().expect("port is poisoned");
        guard.is_output()
    }

    pub fn is_textual(&self) -> bool {
        let arc = self.port_arc();
        let guard = arc.lock().expect("port is poisoned");
        guard.is_textual_input() || guard.is_textual_output()
    }

    pub fn is_binary(&self) -> bool {
        let arc = self.port_arc();
        let guard = arc.lock().expect("port is poisoned");
        guard.is_binary_input() || guard.is_binary_output()
    }
}


// Textual input ports based on BufRead

struct TextIn<R: BufRead + Send + 'static> {
    reader: Option<R>
}

impl<R: BufRead + Send + 'static> TextualInputPort for TextIn<R> {
    fn read<'h>(&mut self, _hs: &mut GcHeapSession<'h>) -> Result<Value<'h>> {
        match self.reader {
            None => Err("read: port is closed".into()),
            Some(ref mut _reader) => Err("read: unimplemented".into()),
        }
    }

    fn read_char(&mut self) -> Result<Option<char>> {
        match self.reader {
            None => Err("read-char: port is closed".into()),
            Some(ref mut _reader) => Err("read-char: unimplemented".into()),
        }
    }

    fn peek_char(&mut self) -> Result<Option<char>> {
        match self.reader {
            None => Err("peek-char: port is closed".into()),
            Some(ref mut _reader) => Err("peek-char: unimplemented".into()),
        }
    }

    fn read_line(&mut self) -> Result<String> {
        match self.reader {
            None => Err("peek-char: port is closed".into()),
            Some(ref mut reader) => {
                let mut s = String::new();
                reader.read_line(&mut s).chain_err(|| "read-line: error reading from port")?;
                Ok(s)
            }
        }
    }

    fn is_char_ready(&mut self) -> Result<bool> {
        match self.reader {
            None => Err("char-ready?: port is closed".into()),
            Some(ref mut _reader) => Err("char-ready?: unimplemented".into()),
        }
    }

    fn read_string(&mut self, _k: usize) -> Result<String> {
        match self.reader {
            None => Err("read-string: port is closed".into()),
            Some(ref mut _reader) => Err("read-string: unimplemented".into()),
        }
    }
}

impl<R: BufRead + Send + 'static> AbstractPort for TextIn<R> {
    fn is_textual_input(&self) -> bool { true }

    fn as_open_textual_input(&mut self) -> Result<&mut TextualInputPort> {
        if self.reader.is_some() {
            Ok(self)
        } else {
            Err("input port is closed".into())
        }
    }

    fn is_input_open(&self) -> Result<bool> { Ok(self.reader.is_some()) }

    fn close_input(&mut self) -> Result<()> { self.reader = None; Ok(()) }

    fn close(&mut self) -> Result<()> { self.close_input() }
}

pub fn buf_read_into_textual_input_port<'h, R: BufRead + Send + 'static>(
    hs: &mut GcHeapSession<'h>,
    reader: R
) -> Value<'h> {
    port_into_value(hs, TextIn {
        reader: Some(reader)
    })
}


// Binary input ports

struct BytesIn<R: BufRead> {
    reader: Option<R>,
}

impl<R: BufRead> BytesIn<R> {
    fn borrow_reader<'s>(&'s mut self, proc_name: &str) -> Result<&'s mut R> {
        match self.reader {
            None => Err(format!("{}: input port is closed", proc_name).into()),
            Some(ref mut reader) => Ok(reader),
        }
    }
}

fn read_u8<R: BufRead>(reader: &mut R) -> Result<Option<u8>> {
    let mut buf: [u8; 1] = [0];
    let bytes_read = reader.read(&mut buf).chain_err(|| "read-u8: error reading from port")?;
    if bytes_read == 0 {
        Ok(None)
    } else {
        debug_assert!(bytes_read == 1);
        Ok(Some(buf[0]))
    }
}

fn peek_u8<R: BufRead>(reader: &mut R) -> Result<Option<u8>> {
    let buf = reader.fill_buf().chain_err(|| "peek-u8: error reading from port")?;
    if buf.is_empty() {
        Ok(None)
    } else {
        Ok(Some(buf[0]))
    }
}

fn read_into<R: BufRead>(reader: &mut R, buf: &mut [u8]) -> Result<Option<usize>> {
    let mut total_read = 0;
    while total_read < buf.len() {
        let bytes_read = reader.read(&mut buf[total_read..])
            .chain_err(|| "read-bytevector!: error reading from port")?;
        if bytes_read == 0 {
            if total_read == 0 {
                return Ok(None);
            }
            break;
        }
        total_read += bytes_read;
    }
    Ok(Some(total_read))
}

impl<R: BufRead + Send> BinaryInputPort for BytesIn<R> {
    fn read_u8(&mut self) -> Result<Option<u8>> {
        read_u8(self.borrow_reader("read-u8")?)
    }

    fn peek_u8(&mut self) -> Result<Option<u8>> {
        peek_u8(self.borrow_reader("peek-u8")?)
    }

    fn read_into(&mut self, buf: &mut [u8]) -> Result<Option<usize>> {
        read_into(self.borrow_reader("read-bytevector")?, buf)
    }
}

pub fn buf_read_into_binary_input_port<'h, R: BufRead + Send + 'static>(
    hs: &mut GcHeapSession<'h>,
    reader: R
) -> Value<'h> {
    port_into_value(hs, BytesIn {
        reader: Some(reader)
    })
}

impl<R: BufRead + Send + 'static> AbstractPort for BytesIn<R> {
    fn is_binary_input(&self) -> bool { true }

    fn as_open_binary_input(&mut self) -> Result<&mut BinaryInputPort> {
        if self.reader.is_some() {
            Ok(self)
        } else {
            Err("input port is closed".into())
        }
    }

    fn is_input_open(&self) -> Result<bool> { Ok(self.reader.is_some()) }

    fn close_input(&mut self) -> Result<()> { self.reader = None; Ok(()) }

    fn close(&mut self) -> Result<()> { self.close_input() }
}


// Stdin

pub struct StdinPort {
    is_open: bool,
}

impl StdinPort {
    pub fn new() -> StdinPort {
        StdinPort {
            is_open: true,
        }
    }

    fn check_open(&self, proc_name: &str) -> Result<()> {
        if self.is_open {
            Ok(())
        } else {
            Err(format!("{}: stdin is closed", proc_name).into())
        }
    }
}

impl TextualInputPort for StdinPort {
    fn read<'h>(&mut self, _hs: &mut GcHeapSession<'h>) -> Result<Value<'h>> {
        self.check_open("read")?;
        Err("read: unimplemented".into())
    }

    fn read_char(&mut self) -> Result<Option<char>> {
        self.check_open("read-char")?;
        Err("read-char: unimplemented".into())
    }

    fn peek_char(&mut self) -> Result<Option<char>> {
        self.check_open("peek-char")?;
        Err("peek-char: unimplemented".into())
    }

    fn read_line(&mut self) -> Result<String> {
        self.check_open("read-line")?;

        io::stdout().flush().chain_err(|| "read-line: error flushing stdout")?;
        let stdin = io::stdin();
        let mut guard = stdin.lock();
        let mut buffer = String::new();
        guard.read_line(&mut buffer).chain_err(|| "read-line: error reading stdin")?;
        Ok(buffer)
    }

    fn is_char_ready(&mut self) -> Result<bool> {
        self.check_open("char-ready?")?;
        Err("char-ready?: unimplemented".into())
    }

    fn read_string(&mut self, _k: usize) -> Result<String> {
        self.check_open("read-string")?;
        Err("read-string: unimplemented".into())
    }
}

impl BinaryInputPort for StdinPort {
    fn read_u8(&mut self) -> Result<Option<u8>> {
        self.check_open("read-u8")?;
        let stdin = io::stdin();
        let mut guard = stdin.lock();
        read_u8(&mut guard)
    }

    fn peek_u8(&mut self) -> Result<Option<u8>> {
        self.check_open("peek-u8")?;
        let stdin = io::stdin();
        let mut guard = stdin.lock();
        peek_u8(&mut guard)
    }

    fn read_into(&mut self, buf: &mut [u8]) -> Result<Option<usize>> {
        self.check_open("read-bytevector")?;
        let stdin = io::stdin();
        let mut guard = stdin.lock();
        read_into(&mut guard, buf)
    }
}

impl AbstractPort for StdinPort {
    fn is_textual_input(&self) -> bool { true }

    fn as_open_textual_input(&mut self) -> Result<&mut TextualInputPort> {
        if self.is_open {
            Ok(self)
        } else {
            Err("stdin is closed".into())
        }
    }

    fn is_input_open(&self) -> Result<bool> { Ok(self.is_open) }

    fn close_input(&mut self) -> Result<()> {
        self.is_open = false;
        Ok(())
    }

    fn close(&mut self) -> Result<()> { self.close_input() }
}

pub fn stdin<'h>(hs: &mut GcHeapSession<'h>) -> Value<'h> {
    port_into_value(hs, StdinPort::new())
}


// Textual output ports

struct TextOut<W: Write + Send + 'static> {
    writer: Option<W>
}

impl<W: Write + Send + 'static> AbstractPort for TextOut<W> {
    fn is_textual_output(&self) -> bool { true }

    fn as_open_textual_output(&mut self) -> Result<&mut Write> {
        match self.writer {
            None => Err("output port is closed".into()),
            Some(ref mut w) => Ok(w),
        }
    }

    fn is_output_open(&self) -> Result<bool> { Ok(self.writer.is_some()) }

    fn close_output(&mut self) -> Result<()> { self.writer = None; Ok(())}

    fn close(&mut self) -> Result<()> { self.close_output() }
}

pub fn writer_into_textual_output_port<'h, W: Write + Send + 'static>(
    hs: &mut GcHeapSession<'h>,
    writer: W
) -> Value<'h> {
    port_into_value(hs, TextOut {
        writer: Some(writer),
    })
}

pub fn stdout<'h>(hs: &mut GcHeapSession<'h>) -> Value<'h> {
    writer_into_textual_output_port(hs, io::stdout())
}

pub fn stderr<'h>(hs: &mut GcHeapSession<'h>) -> Value<'h> {
    writer_into_textual_output_port(hs, io::stderr())
}


// open-output-string

struct StringOut {
    buffer: Option<String>,
}

impl Write for StringOut {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let s = match std::str::from_utf8(buf) {
            Ok(s) => s,
            Err(_) => return Err(io::Error::new(io::ErrorKind::Other, "output must be UTF-8")),
        };
        match self.buffer {
            None => return Err(io::Error::new(io::ErrorKind::Other, "port is closed")),
            Some(ref mut string) => *string += s,
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.write(buf)?;
        Ok(())
    }
}

impl AbstractPort for StringOut {
    fn is_textual_output(&self) -> bool { true }

    fn as_open_textual_output(&mut self) -> Result<&mut Write> {
        if self.buffer.is_some() {
            Ok(self)
        } else {
            Err("output port is closed".into())
        }
    }

    fn get_output_string(&self) -> Result<String> {
        match self.buffer {
            None => Err("output port is closed".into()),
            Some(ref s) => Ok(s.clone()),
        }
    }

    fn is_output_open(&self) -> Result<bool> { Ok(self.buffer.is_some()) }

    fn close_output(&mut self) -> Result<()> { self.buffer = None; Ok(())}

    fn close(&mut self) -> Result<()> { self.close_output() }
}

pub fn new_output_string_port<'h>(hs: &mut GcHeapSession<'h>) -> Value<'h> {
    port_into_value(hs, StringOut {
        buffer: Some(String::new()),
    })
}


// Binary output ports

struct BinOut<W: Write + Send + 'static> {
    writer: Option<W>
}

impl<W: Write + Send + 'static> AbstractPort for BinOut<W> {
    fn is_binary_output(&self) -> bool { true }

    fn as_open_binary_output(&mut self) -> Result<&mut Write> {
        match self.writer {
            Some(ref mut w) => Ok(w),
            None => Err("port is closed".into()),
        }
    }

    fn is_output_open(&self) -> Result<bool> { Ok(self.writer.is_some()) }

    fn close_output(&mut self) -> Result<()> { self.writer = None; Ok(())}

    fn close(&mut self) -> Result<()> { self.close_output() }
}

pub fn writer_into_binary_output_port<'h, W: Write + Send + 'static>(
    hs: &mut GcHeapSession<'h>,
    writer: W
) -> Value<'h> {
    port_into_value(hs, BinOut {
        writer: Some(writer),
    })
}

// open-output-bytevector

struct BytesOut {
    buffer: Option<Vec<u8>>,
}

impl Write for BytesOut {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self.buffer {
            None => return Err(io::Error::new(io::ErrorKind::Other, "port is closed")),
            Some(ref mut v) => v.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.write(buf)?;
        Ok(())
    }
}

impl AbstractPort for BytesOut {
    fn is_binary_output(&self) -> bool { true }

    fn as_open_binary_output(&mut self) -> Result<&mut Write> {
        if self.buffer.is_some() {
            Ok(self)
        } else {
            Err("output port is closed".into())
        }
    }

    fn get_output_bytevector(&self) -> Result<Vec<u8>> {
        match self.buffer {
            None => Err("output port is closed".into()),
            Some(ref v) => Ok(v.clone()),
        }
    }

    fn is_output_open(&self) -> Result<bool> { Ok(self.buffer.is_some()) }

    fn close_output(&mut self) -> Result<()> { self.buffer = None; Ok(())}

    fn close(&mut self) -> Result<()> { self.close_output() }
}

pub fn new_output_bytevector_port<'h>(hs: &mut GcHeapSession<'h>) -> Value<'h> {
    port_into_value(hs, BytesOut {
        buffer: Some(Vec::new()),
    })
}
