//! Parsing of s-expressions.

use cell_gc::{GcHeapSession, GcLeaf};
use cell_gc::collections::VecRef;
use errors::Result;
use nom::{IResult, Needed};
use std::str::FromStr;
use value::{InternedString, Pair, PairRef, Value};


// Generic Scheme parser ///////////////////////////////////////////////////////

/// A value that can build Rust values representing Scheme data.
///
/// As the parser runs left to right over the code, it calls the methods of a
/// builder to report what it finds. The builder is responsible for buliding
/// out an actual Rust data structure (or whatever).
///
/// Currently there's no location information in here.
///
pub trait Builder {
    /// The type of Rust value that represents the Scheme production `datum`.
    type Datum;

    /// A partially-constructed list. This is used during parsing, as a list is
    /// being parsed; when the close parenthesis is parsed, the `List` is
    /// turned into a finished `Datum` (see `list_close()`).
    type List;

    /// A partially-constructed vector.
    type Vector;

    /// A partially-constructed bytevector.
    type Bytevector;

    fn identifier(&mut self, s: &str) -> Self::Datum;
    fn boolean(&mut self, b: bool) -> Self::Datum;
    fn integer(&mut self, i: i32) -> Self::Datum;
    fn character(&mut self, c: char) -> Self::Datum;
    fn string(&mut self, s: String) -> Self::Datum;

    fn toplevel_push(&mut self, d: Self::Datum);
    fn new_list(&mut self) -> Self::List;
    fn list_push(&mut self, list: &mut Self::List, d: Self::Datum);
    fn list_close(&mut self, list: Self::List) -> Self::Datum;
    fn list_improper_close(&mut self, list: Self::List, d: Self::Datum) -> Self::Datum;
    fn new_vector(&mut self) -> Self::Vector;
    fn vector_push(&mut self, v: &mut Self::Vector, d: Self::Datum);
    fn vector_close(&mut self, v: Self::Vector) -> Self::Datum;
    fn new_bytevector(&mut self) -> Self::Bytevector;
    fn bytevector_push(&mut self, v: &mut Self::Bytevector, byte: u8);
    fn bytevector_close(&mut self, v: Self::Bytevector) -> Self::Datum;
}

/// A `Builder` that ignores and discards all input.
/// This could be used for parsing `#;(...)` comments.
#[allow(dead_code)]
struct Ignore;

impl Builder for Ignore {
    type Datum = ();
    type List = ();
    type Vector = ();
    type Bytevector = ();

    fn identifier(&mut self, _s: &str) {}
    fn boolean(&mut self, _b: bool) {}
    fn integer(&mut self, _i: i32) {}
    fn character(&mut self, _c: char) {}
    fn string(&mut self, _s: String) {}

    fn toplevel_push(&mut self, _d: ()) {}
    fn new_list(&mut self) {}
    fn list_push(&mut self, _list: &mut (), _d: ()) {}
    fn list_close(&mut self, _list: ()) {}
    fn list_improper_close(&mut self, _list: (), _d: ()) {}
    fn new_vector(&mut self) {}
    fn vector_push(&mut self, _v: &mut (), _d: ()) {}
    fn vector_close(&mut self, _v: ()) {}
    fn new_bytevector(&mut self) {}
    fn bytevector_push(&mut self, _v: &mut (), _byte: u8) {}
    fn bytevector_close(&mut self, _v: ()) {}
}

/// A mut reference to a Builder is also a builder.
/// (There's a similar generic `impl` for `Iterator`.
/// We're not actually using this anywhere, but might as well have it.)
impl<'b, B: Builder> Builder for &'b mut B {
    type Datum = B::Datum;
    type List = B::List;
    type Vector = B::Vector;
    type Bytevector = B::Bytevector;

    fn identifier(&mut self, s: &str) -> B::Datum { B::identifier(*self, s) }
    fn boolean(&mut self, b: bool) -> B::Datum { B::boolean(*self, b) }
    fn integer(&mut self, i: i32) -> B::Datum { B::integer(*self, i) }
    fn character(&mut self, c: char) -> B::Datum { B::character(*self, c) }
    fn string(&mut self, s: String) -> B::Datum { B::string(*self, s) }

    fn toplevel_push(&mut self, d: B::Datum) { B::toplevel_push(*self, d); }
    fn new_list(&mut self) -> B::List { B::new_list(*self) }
    fn list_push(&mut self, list: &mut B::List, d: B::Datum) { B::list_push(*self, list, d); }
    fn list_close(&mut self, list: B::List) -> B::Datum { B::list_close(*self, list) }
    fn list_improper_close(&mut self, list: B::List, d: B::Datum) -> B::Datum {
        B::list_improper_close(*self, list, d)
    }
    fn new_vector(&mut self) -> B::Vector { B::new_vector(*self) }
    fn vector_push(&mut self, v: &mut B::Vector, d: B::Datum) { B::vector_push(*self, v, d); }
    fn vector_close(&mut self, v: B::Vector) -> B::Datum { B::vector_close(*self, v) }
    fn new_bytevector(&mut self) -> B::Bytevector { B::new_bytevector(*self) }
    fn bytevector_push(&mut self, v: &mut B::Bytevector, byte: u8) { B::bytevector_push(*self, v, byte); }
    fn bytevector_close(&mut self, v: B::Bytevector) -> B::Datum { B::bytevector_close(*self, v) }
}

/// Partial continuations for parsing, so that the parsing algorithm is
/// iterative and not recursive. A vector of these serves as "the stack" for
/// the parser, telling where the next value we parse needs to be stored, and
/// what happens after that.
enum ReaderCtn<B: Builder> {
    List(B::List),
    ImproperList(B::List),
    Vector(B::Vector),
    Quote(B::List)
}

/// A Scheme parser.
pub struct Parser<'s, B: Builder> {
    /// The builder receives the output from parsing and is responsible for
    /// building the tree of Scheme objects in memory.
    builder: B,

    /// This represents partially-built values, partially-parsed forms.
    stack: Vec<ReaderCtn<B>>,

    /// The string being parsed.
    source: &'s str,

    /// The current read point in the source code, as a byte offset from the
    /// start.
    point: usize,
}

impl<'s, B: Builder> Parser<'s, B> {
    fn new(source: &'s str, builder: B) -> Parser<'s, B> {
        Parser {
            source,
            point: 0,
            builder: builder,
            stack: vec![],
        }
    }

    fn eat_close_paren(&mut self) -> Result<()> {
        self.skip_space();
        if self.source[self.point..].starts_with(')') {
            self.point += 1;
            Ok(())
        } else {
            Err(format!("')' expected at offset {}", self.point).into())
        }
    }

    fn open(&mut self) {
        self.stack.push(ReaderCtn::List(self.builder.new_list()));
    }

    fn dot(&mut self) -> Result<()> {
        match self.stack.pop().unwrap() {
            ReaderCtn::List(list) => {
                self.stack.push(ReaderCtn::ImproperList(list));
                Ok(())
            }
            _ => Err("dot allowed only in a list".into()),
        }
    }

    fn push_quote(&mut self, tag: &'static str) {
        let mut list = self.builder.new_list();
        let symbol = self.builder.identifier(tag);
        self.builder.list_push(&mut list, symbol);
        self.stack.push(ReaderCtn::Quote(list));
    }

    fn skip_space(&mut self) {
        match intertoken_space(&self.source[self.point..]) {
            IResult::Error(e) => panic!("bug: error in intertoken_space: {:?}", e),
            IResult::Incomplete(_) => panic!("bug: incomplete intertoken_space"),
            IResult::Done(rest, ()) => self.point = self.source.len() - rest.len(),
        }
    }

    pub fn parse_bytevector(&mut self) -> Result<B::Datum> {
        let mut vec = self.builder.new_bytevector();
        loop {
            self.skip_space();
            let start = self.point;
            let t = token(&self.source[start..]);
            match t {
                IResult::Done(rest, token) => {
                    self.point = self.source.len() - rest.len();
                    match token {
                        Token::Close => {
                            break;
                        }
                        Token::Number(s) => {
                            if let Ok(u) = u8::from_str(&s) {
                                self.builder.bytevector_push(&mut vec, u);
                            } else {
                                return Err(
                                    format!("parse error: expected byte or ')' in bytevector, got {}",
                                            &self.source[start..self.point]).into()
                                );
                            }
                        }
                        _ => {
                            return Err(
                                format!("in bytevector, expected byte or ')' in bytevector, got {}",
                                        &self.source[start..self.point]).into()
                            );
                        }
                    }
                }
                IResult::Error(e) => return Err(e.description().into()),
                IResult::Incomplete(_) => break,
            }
        }
        Ok(self.builder.bytevector_close(vec))
    }

    pub fn parse_datum(&mut self) -> Result<Option<B::Datum>> {
        loop {
            self.skip_space();
            let start = self.point;
            let t = token(&self.source[start..]);
            let mut datum = match t {
                IResult::Error(e) => {
                    return Err(e.description().into());
                }
                IResult::Incomplete(_) => {
                    break;
                }
                IResult::Done(rest, token) => {
                    self.point = self.source.len() - rest.len();
                    match token {
                        Token::Identifier => {
                            let s = &self.source[start..self.point];
                            self.builder.identifier(&s)
                        }
                        Token::Boolean(b) => self.builder.boolean(b),
                        Token::Number(s) => {
                            if let Ok(i) = i32::from_str(&s) {
                                self.builder.integer(i)
                            } else {
                                return Err(format!("unsupported number token {}", s).into())
                            }
                        }
                        Token::Char(c) => self.builder.character(c),
                        Token::String(s) => self.builder.string(s),
                        Token::Open => {
                            self.open();
                            continue;
                        }
                        Token::Close => {
                            match self.stack.pop().unwrap() {
                                ReaderCtn::List(list) => self.builder.list_close(list),
                                ReaderCtn::Vector(vector) => self.builder.vector_close(vector),
                                _ => return Err("unexpected ')'".into()),
                            }
                        }

                        Token::OpenVector => {
                            self.stack.push(ReaderCtn::Vector(self.builder.new_vector()));
                            continue;
                        }
                        Token::OpenBytevector => self.parse_bytevector()?,
                        Token::Quote => {
                            self.push_quote("quote");
                            continue;
                        }
                        Token::Quasiquote => {
                            self.push_quote("quasiquote");
                            continue;
                        }
                        Token::UnquoteSplicing => {
                            self.push_quote("unquote-splicing");
                            continue;
                        }
                        Token::Unquote => {
                            self.push_quote("unquote");
                            continue;
                        }
                        Token::Dot => {
                            self.dot()?;
                            continue;
                        }
                    }
                }
            };

            loop {
                match self.stack.last_mut() {
                    None => {
                        return Ok(Some(datum));
                    }
                    Some(&mut ReaderCtn::List(ref mut list)) => {
                        self.builder.list_push(list, datum);
                        break;
                    }
                    Some(&mut ReaderCtn::Vector(ref mut vector)) => {
                        self.builder.vector_push(vector, datum);
                        break;
                    }
                    _ => {}
                }
                match self.stack.pop().unwrap() {
                    ReaderCtn::ImproperList(list) => {
                        self.eat_close_paren()?;
                        datum = self.builder.list_improper_close(list, datum);
                    }
                    ReaderCtn::Quote(mut list) => {
                        self.builder.list_push(&mut list, datum);
                        datum = self.builder.list_close(list);
                    }
                    _ => panic!("missing case in Parser::parse()"),
                }
            }
        }

        // Incomplete token. Assuming the input was a complete program/file, we
        // either have EOF or an error.
        self.skip_space();
        if self.point < self.source.len() {
            Err(format!("parse error at offset {}", self.point).into())
        } else if !self.stack.is_empty() {
            Err("parse error: unexpected end of input".into())
        } else {
            Ok(None)
        }
    }

    pub fn parse_all(mut self) -> Result<B> {
        while let Some(datum) = self.parse_datum()? {
            self.builder.toplevel_push(datum);
        }
        Ok(self.builder)
    }
}


// === Tokens

enum Token {
    Identifier,
    Boolean(bool),
    Number(String),
    Char(char),
    String(String),
    Open,
    Close,
    OpenVector,
    OpenBytevector,
    Quote,
    Quasiquote,
    UnquoteSplicing,
    Unquote,
    Dot,
}

fn token(s: &str) -> IResult<&str, Token> {
    let mut it = s.chars();
    match it.next() {
        None => IResult::Incomplete(Needed::Unknown),
        Some(c) => match c {
            '(' => IResult::Done(it.as_str(), Token::Open),
            ')' => IResult::Done(it.as_str(), Token::Close),
            '\'' => IResult::Done(it.as_str(), Token::Quote),
            '`' => IResult::Done(it.as_str(), Token::Quasiquote),
            '"' => string(s).map(Token::String),

            'A' ... 'Z' | 'a' ... 'z' |
            '!' | '$' | '%' | '&' | '*' | '/' | ':' |
            '<' | '=' | '>' | '?' | '~' | '_' | '^' =>
                ordinary_identifier(s),

            _ => other_token(s)
        }
    }
}

// A token that is not one of ``( ) ' ` "`` or a symbol starting with an ASCII
// letter.
named!(
    other_token(&str) -> Token,
    alt!(
        // Achtung! 'number' must precede 'identifier' in this alt!()
        // So that `-1` is parsed as a single number token, not `- 1`.
        map!(number, Token::Number)
      | map!(other_identifier, |_| Token::Identifier)
      | map!(boolean, Token::Boolean)
      | map!(character, Token::Char)
      | value!(Token::OpenVector, tag!("#("))
      | value!(Token::OpenBytevector, tag!("#u8("))
      | value!(Token::UnquoteSplicing, tag!(",@"))
      | value!(Token::Unquote, char!(','))
      | value!(Token::Dot, terminated!(char!('.'), peek!(delimiter)))
    )
);

fn push_char(mut s: String, c: char) -> String {
    s.push(c);
    s
}


named!(delimiter(&str) -> (), alt!(value!((), one_of!(" \n\r\t()\";")) | value!((), eof!())));
named!(whitespace(&str) -> (), value!((), one_of!(" \n\r\t")));
named!(comment(&str) -> (), value!((), preceded!(char!(';'), take_until_and_consume!("\n"))));
named!(atmosphere(&str) -> (), alt!(whitespace | comment));
named!(intertoken_space(&str) -> (), value!((), many0!(atmosphere)));

#[test]
fn test_whitespace() {
    assert_eq!(whitespace(" \n\r\t"), IResult::Done("\n\r\t", ()));
    assert_eq!(
        comment("; hello\nnext line"),
        IResult::Done("next line", ())
    );
    assert_eq!(
        intertoken_space(";; nothing to see here\n;;\n;; just a program\n\n\t\n\n"),
        IResult::Done("", ())
    );
}

/// An identifier starting with a character that matches `<letter>` or
/// `<special initial>`. The first character of s must already have matched!
fn ordinary_identifier(s: &str) -> IResult<&str, Token> {
    let mut ci = s.chars();
    ci.next();
    loop {
        let rest = ci.as_str();
        match ci.next() {
            None => return IResult::Incomplete(Needed::Unknown),
            Some(c) => match c {
                // <subsequent>
                'A' ... 'Z' | 'a' ... 'z' |
                '!' | '$' | '%' | '&' | '*' | '/' | ':' |
                '<' | '=' | '>' | '?' | '~' | '_' | '^' |
                '0' ... '9' | '.' | '+' | '-' => {}
                _ => return IResult::Done(
                    rest,
                    // &s[..s.len() - rest.len()]
                    Token::Identifier
                ),
            }
        }
    }
}

named!(
    other_identifier(&str) -> &str,
    alt!(
        tag!("+")
      | tag!("-")
      | tag!("...")
    )
);

named!(digit(&str) -> char, one_of!("0123456789"));
named!(xdigit(&str) -> u32,
       map!(one_of!("0123456789abcdefABCDEF"), |ch| ch.to_digit(16).unwrap()));

named!(
    boolean(&str) -> bool,
    alt!(
        value!(true, terminated!(tag!("#t"), peek!(delimiter)))
      | value!(false, terminated!(tag!("#f"), peek!(delimiter)))
    )
);

named!(
    character(&str) -> char,
    terminated!(
        alt!(
            value!(' ', tag!("#\\space"))
          | value!('\n', tag!("#\\newline"))
          | preceded!(tag!("#\\"), none_of!(""))
        ),
        peek!(delimiter)
    )
);

#[test]
fn test_character() {
    assert_eq!(character(r"#\space "), IResult::Done(" ", ' '));
    assert_eq!(character(r"#\s "), IResult::Done(" ", 's'));
    assert!(character(r"#\sp ").is_err());
    assert_eq!(character(r"#\λ "), IResult::Done(" ", 'λ'));
    assert!(character(r"#\λx").is_err());
}

named!(
    string(&str) -> String,
    delimited!(
        tag!("\""),
        fold_many0!(
            string_element,
            String::new(),
            push_char
        ),
        tag!("\"")
    )
);

named!(
    string_element(&str) -> char,
    alt!(
        none_of!("\"\\")
      | preceded!(char!('\\'), one_of!("\\\""))
      | value!('\n', tag!("\\n"))
      | value!('\r', tag!("\\r"))
      | preceded!(tag!("\\x"), do_parse!(
            d1: xdigit >>
            d2: xdigit >>
            (((d1 * 16) + d2) as u8 as char)
        ))
    )
);

named!(
    number(&str) -> String,
    do_parse!(
        sign: sign >>
        num_str: fold_many1!(
            digit,
            sign.to_string(),
            push_char
        ) >>
        (num_str)
    )
);

named!(
    sign(&str) -> &'static str,
    alt!(
        value!("-", char!('-'))
      | value!("", opt!(char!('+')))
    )
);


// Parsing

#[test]
fn test_parse() {
    struct TestBuilder {
        top: Vec<String>
    }

    impl Builder for TestBuilder {
        type Datum = String;
        type List = String;
        type Vector = String;
        type Bytevector = String;

        fn identifier(&mut self, s: &str) -> String { s.to_string() }
        fn boolean(&mut self, b: bool) -> String { format!("{}", b) }
        fn integer(&mut self, i: i32) -> String { format!("{}", i) }
        fn character(&mut self, c: char) -> String { format!("{:?}", c) }
        fn string(&mut self, s: String) -> String { format!("{:?}", s) }

        fn toplevel_push(&mut self, d: String) { self.top.push(d); }
        fn new_list(&mut self) -> String { "(".to_string() }
        fn list_push(&mut self, list: &mut String, d: String) {
            if list.len() > 1 {
                list.push(' ');
            }
            *list += &d;
        }
        fn list_close(&mut self, list: String) -> String { list + ")" }
        fn list_improper_close(&mut self, mut list: String, d: String) -> String {
            list += " .";
            self.list_push(&mut list, d);
            list + ")"
        }
        fn new_vector(&mut self) -> String { "[".to_string() }
        fn vector_push(&mut self, v: &mut String, d: String) {
            if v.len() > 1 {  // already contains at least one element; 1 == "[".len()
                v.push(' ');
            }
            *v += &d;
        }
        fn vector_close(&mut self, v: String) -> String { v + "]" }
        fn new_bytevector(&mut self) -> String { "b[".to_string() }
        fn bytevector_push(&mut self, v: &mut String, byte: u8) {
            if v.len() > 2 {  // already contains at least one element; 2 == "b[".len()
                v.push(' ');
            }
            *v += &format!("{}", byte);
        }
        fn bytevector_close(&mut self, v: String) -> String { v + "]" }
    }

    fn parse(s: &str) -> Result<Vec<String>> {
        let p = Parser::new(s, TestBuilder { top: vec![] });
        Ok(p.parse()?.top)
    }

    assert_eq!(
        parse("376    ").unwrap(),
        vec!["376"]
    );

    assert_eq!(parse("").unwrap(), Vec::<String>::new());
    assert_eq!(parse("\t").unwrap(), Vec::<String>::new());
    assert_eq!(parse("    376").unwrap(), vec!["376"]);
    assert_eq!(
        parse("    376\n    212\n").unwrap(),
        vec!["376", "212"]
    );
    assert_eq!(
        parse("    376\n    ;212\n").unwrap(),
        vec!["376"]
    );
    assert_eq!(
        parse(";; nothing to see here\n\n").unwrap(),
        Vec::<String>::new()
    );
    assert_eq!(
        parse("(1 2)").unwrap(),
        vec!["(1 2)"]
    );
    assert_eq!(
        parse("#()").unwrap(),
        vec!["[]"]
    );
    assert_eq!(
        parse("#(a)").unwrap(),
        vec!["[a]"]
    );
    assert!(parse("#(0 . 1)").is_err());

    assert_eq!(parse("#u8()").unwrap(),
               vec!["b[]"]);
    assert_eq!(parse("#u8(  ;comments allowed here\n)").unwrap(),
               vec!["b[]"]);
    assert_eq!(parse("#u8(0 255)").unwrap(),
               vec!["b[0 255]"]);
    assert!(parse("#u8(a)").is_err());
    assert!(parse("#u8(254 255 256)").is_err());
    assert!(parse("#u8(-1)").is_err());
    assert!(parse("#u8(-0)").is_err());

    assert_eq!(
        parse(
            ";; I stole these lines of code from <https://www.bluishcoder.co.nz/jsscheme/>.
             ;; Original by Alex Yakovlev. Adapted by Chris Double.

             (define (list . x) x)
             (define (not x) (if x #f #t))").unwrap(),
        vec!["(define (list . x) x)",
             "(define (not x) (if x false true))"]
    );
}

struct ValueBuilder<'v, 'h: 'v> {
    hs: &'v mut GcHeapSession<'h>,
    toplevel: Vec<Value<'h>>,
}

impl<'v, 'h: 'v> ValueBuilder<'v, 'h> {
    fn new(hs: &'v mut GcHeapSession<'h>) -> ValueBuilder<'v, 'h> {
        ValueBuilder { hs, toplevel: vec![] }
    }
}

impl<'v, 'h: 'v> Builder for ValueBuilder<'v, 'h> {
    type Datum = Value<'h>;
    type List = Option<(PairRef<'h>, PairRef<'h>)>;
    type Vector = VecRef<'h, Value<'h>>;
    type Bytevector = Vec<u8>;

    fn identifier(&mut self, s: &str) -> Value<'h> {
        Value::Symbol(GcLeaf::new(InternedString::get(s)))
    }

    fn boolean(&mut self, b: bool) -> Value<'h> {
        Value::Bool(b)
    }

    fn integer(&mut self, i: i32) -> Value<'h> {
        Value::Int(i)
    }

    fn character(&mut self, c: char) -> Value<'h> {
        Value::Char(c)
    }

    fn string(&mut self, s: String) -> Value<'h> {
        Value::ImmString(GcLeaf::new(InternedString::get(s)))
    }

    fn toplevel_push(&mut self, v: Value<'h>) {
        self.toplevel.push(v);
    }

    fn new_list(&mut self) -> Self::List {
        None
    }

    fn list_push(&mut self, list: &mut Self::List, value: Value<'h>) {
        let new_tail = self.hs.alloc(Pair {
            car: value,
            cdr: Value::Nil
        });
        match *list {
            Some((_, ref mut last)) => {
                last.set_cdr(Value::Cons(new_tail.clone()));
                *last = new_tail;
            }
            ref mut other => *other = Some((new_tail.clone(), new_tail)),
        }
    }

    fn list_close(&mut self, list: Self::List) -> Value<'h> {
        match list {
            None => Value::Nil,
            Some((full, _)) => Value::Cons(full)
        }
    }

    fn list_improper_close(&mut self, list: Self::List, tail: Value<'h>) -> Value<'h> {
        match list {
            None => panic!("bug: improper list with no head"),
            Some((full, last)) => {
                last.set_cdr(tail);
                Value::Cons(full)
            }
        }
    }

    fn new_vector(&mut self) -> VecRef<'h, Value<'h>> {
        self.hs.alloc(vec![])
    }

    fn vector_push(&mut self, vector: &mut VecRef<'h, Value<'h>>, value: Value<'h>) {
        vector.push(value);
    }

    fn vector_close(&mut self, vector: VecRef<'h, Value<'h>>) -> Value<'h> {
        Value::Vector(vector)
    }

    fn new_bytevector(&mut self) -> Vec<u8> {
        vec![]
    }

    fn bytevector_push(&mut self, bytevector: &mut Vec<u8>, byte: u8) {
        bytevector.push(byte);
    }

    fn bytevector_close(&mut self, bytevector: Vec<u8>) -> Value<'h> {
        Value::ImmBytevector(self.hs.alloc(bytevector))
    }
}

/// Given a source string, read one datum and return the `Value` for the object
/// represented by the datum. On success, returns `Ok(Some((value,
/// remainder_of_string)))`. On end of input, returns `Ok(None)`.
pub fn parse<'h, 's>(
    hs: &mut GcHeapSession<'h>,
    source: &'s str
) -> Result<Option<(Value<'h>, &'s str)>> {
    let mut parser = Parser::new(source, ValueBuilder::new(hs));
    let opt_datum = parser.parse_datum()?;
    Ok(opt_datum.map(|d| (d, &source[parser.point..])))
}

/// Top level entry point to s-expression parsing. Takes a source string and
/// returns a vector of `Value`s.
pub fn parse_all<'h>(hs: &mut GcHeapSession<'h>, source: &str) -> Result<Vec<Value<'h>>> {
    let parser = Parser::new(source, ValueBuilder::new(hs));
    Ok(parser.parse_all()?.toplevel)
}
