//! Parsing of s-expressions.

use cell_gc::GcHeapSession;
use nom::IResult;
use std::str::FromStr;
use std::sync::Arc;
use vm::{Pair, Value};


// === Tokens

// #[derive(Clone)]
// enum Token {
//     Identifier(Arc<String>),
//     Boolean(bool),
//     Number(String),
//     Character(char),
//     String(Arc<String>),
//     Open,
//     Close,
//     OpenVector,
//     Quote,
//     Quasiquote,
//     UnquoteSplicing,
//     Unquote,
//     Dot,
// }

fn push_char(mut s: String, c: char) -> String {
    s.push(c);
    s
}

// named!(
//     token(&str) -> Token,
//     preceded!(
//         intertoken_space,
//         alt!(
//             map!(identifier, |s| Token::Identifier(Arc::new(s)))
//           | map!(boolean, Token::Boolean)
//           | map!(number, Token::Number)
//           | map!(character, Token::Character)
//           | map!(string, |s| Token::String(Arc::new(s)))
//           | value!(Token::Open, open)
//           | value!(Token::Close, char!(')'))
//           | value!(Token::OpenVector, open_vector)
//           | value!(Token::Quote, char!('\''))
//           | value!(Token::Quasiquote, char!('`'))
//           | value!(Token::UnquoteSplicing, tag!(",@"))
//           | value!(Token::Unquote, char!(','))
//           | value!(Token::Dot, char!('.'))
//         )
//     )
// );

named!(open(&str) -> (), value!((), char!('(')));
named!(close(&str) -> (), value!((), char!(')')));
named!(open_vector(&str) -> (), value!((), tag!("#(")));
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

named!(
    identifier(&str) -> String,
    alt!(
        do_parse!(
            first: initial >>
            s: fold_many0!(
                subsequent,
                first.to_string(),
                push_char
            ) >>
            (s)
        )
      | map!(
            alt!(
                tag!("+")
                    | tag!("-")
                    | tag!("...")
            ),
            str::to_string
        )
    )
);

named!(initial(&str) -> char, alt!(letter | special_initial));

named!(letter(&str) -> char, one_of!("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));

named!(special_initial(&str) -> char, one_of!("!$%&*/:<=>?~_^"));

named!(subsequent(&str) -> char, alt!(initial | digit | one_of!(".+-")));

named!(digit(&str) -> char, one_of!("0123456789"));

named!(
    boolean(&str) -> bool,
    alt!(
        value!(true, terminated!(tag!("#t"), peek!(delimiter)))
      | value!(false, terminated!(tag!("#f"), peek!(delimiter)))
    )
);

named!(
    character(&str) -> char,
    alt!(
        preceded!(tag!("#\\"), none_of!(""))
      | value!(' ', tag!("#\\space"))
      | value!('\n', tag!("#\\newline"))
    )
);

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


// === Parsing

#[derive(PartialEq, Debug)]
enum Datum {
    Boolean(bool),
    Number(String),
    Character(char),
    String(String),
    Identifier(String),
    List(Vec<Datum>),
    ImproperList(Vec<Datum>),
    Vector(Vec<Datum>),
}

named!(
    file_to_data(&str) -> Vec<Datum>,
    delimited!(
        intertoken_space,
        many0!(terminated!(datum, intertoken_space)),
        eof!()
    )
);

#[test]
fn test_parser() {
    use nom::ErrorKind;

    assert_eq!(
        datum("376    "),
        IResult::Done("    ", Datum::Number("376".to_string()))
    );
    assert_eq!(datum("    376"), IResult::Error(ErrorKind::Alt));

    assert_eq!(file_to_data(""), IResult::Done("", vec![]));
    assert_eq!(file_to_data("\t"), IResult::Done("", vec![]));
    assert_eq!(
        file_to_data("    376"),
        IResult::Done("", vec![Datum::Number("376".to_string())])
    );
    assert_eq!(
        file_to_data("    376\n    212\n"),
        IResult::Done(
            "",
            vec![
                Datum::Number("376".to_string()),
                Datum::Number("212".to_string()),
            ]
        )
    );
    assert_eq!(
        file_to_data("    376\n    ;212\n"),
        IResult::Done("", vec![Datum::Number("376".to_string())])
    );
    assert_eq!(
        file_to_data(";; nothing to see here\n\n"),
        IResult::Done("", vec![])
    );
    assert_eq!(
        file_to_data("(1 2)"),
        IResult::Done(
            "",
            vec![
                Datum::List(vec![
                    Datum::Number("1".to_string()),
                    Datum::Number("2".to_string()),
                ]),
            ]
        )
    );
}

named!(
    datum(&str) -> Datum,
    alt!(simple_datum | compound_datum)
);

named!(
    simple_datum(&str) -> Datum,
    alt!(
        map!(boolean, Datum::Boolean)
      | map!(number, Datum::Number)
      | map!(character, Datum::Character)
      | map!(string, Datum::String)
      | map!(identifier, Datum::Identifier)
    )
);

named!(
    compound_datum(&str) -> Datum,
    alt!(list | vector)
);

named!(
    list(&str) -> Datum,
    alt!(
        preceded!(
            terminated!(open, intertoken_space),
            alt!(
                // Nonempty list or improper list.
                map!(
                    terminated!(
                        pair!(
                            // Normal list elements.
                            many1!(terminated!(datum, intertoken_space)),
                            // Optional improper ending.
                            opt!(
                                do_parse!(
                                    value!((), char!('.')) >>
                                    intertoken_space >>
                                    d: datum >>
                                    intertoken_space >>
                                    (d)
                                )
                            )
                        ),
                        close
                    ),
                    |(mut data, opt_improper)| {
                        match opt_improper {
                            Some(improper) => {
                                data.push(improper);
                                Datum::ImproperList(data)
                            }
                            None =>
                                Datum::List(data),
                        }
                    }
                )
              | map!(
                    close,
                    |()| Datum::List(vec![])
                )
            )
        )
      | do_parse!(
            prefix: alt!(
                value!("quote", char!('\''))
              | value!("quasiquote", char!('`'))
              | value!("unquote", char!(','))
              | value!("unquote-splicing", tag!(",@"))
            ) >>
            intertoken_space >>
            arg: datum >>
            (
                Datum::List(vec![
                    // allocation here is dumb; symbols should be reused, of course
                    Datum::Identifier(prefix.to_string()),
                    arg,
                ])
            )
        )
    )
);

named!(
    vector(&str) -> Datum,
    map!(
        delimited!(
            terminated!(open_vector, intertoken_space),
            many0!(terminated!(datum, intertoken_space)),
            close
        ),
        Datum::Vector
    )
);

#[test]
fn test_parse_vector() {
    use nom::ErrorKind;

    assert_eq!(vector("#()"), IResult::Done("", Datum::Vector(vec![])));
    assert_eq!(
        vector("#(a)"),
        IResult::Done("", Datum::Vector(vec![Datum::Identifier("a".to_string())]))
    );
    assert_eq!(vector("#(0 . 1)"), IResult::Error(ErrorKind::Char));
}

fn into_list<'h>(
    hs: &mut GcHeapSession<'h>,
    elements: Vec<Datum>,
    tail: Value<'h>,
) -> Result<Value<'h>, &'static str> {
    let mut list = tail;
    for d in elements.into_iter().rev() {
        let value = datum_to_value(hs, d)?;
        list = Value::Cons(hs.alloc(Pair {
            car: value,
            cdr: list,
        }));
    }
    Ok(list)
}

fn datum_to_value<'h>(hs: &mut GcHeapSession<'h>, datum: Datum) -> Result<Value<'h>, &'static str> {
    match datum {
        Datum::Boolean(b) => Ok(Value::Bool(b)),
        Datum::Number(s) => {
            if let Ok(i) = i32::from_str(&s) {
                Ok(Value::Int(i))
            } else {
                Err("unsupported number token")
            }
        }
        Datum::Character(_) => Err("character tokens not supported"),
        Datum::String(_) => Err("strings not supported"),
        Datum::Identifier(s) => Ok(Value::Symbol(Arc::new(s))),
        Datum::List(data) => into_list(hs, data, Value::Nil),
        Datum::ImproperList(mut data) => {
            let tail = data.pop()
                .expect("internal error: improper lists require a last value");
            let tail_val = datum_to_value(hs, tail)?;
            into_list(hs, data, tail_val)
        }
        Datum::Vector(data) => {
            let values: Result<Vec<Value<'h>>, &'static str> =
                data.into_iter().map(|d| datum_to_value(hs, d)).collect();
            Ok(Value::Vector(hs.alloc(values?)))
        }
    }
}

/// Top level entry point to s-expression parsing. Takes a source string and
/// returns a `Value` which can contain `Pair`s allocated in the GC heap.
pub fn parse<'h>(hs: &mut GcHeapSession<'h>, source: &str) -> Result<Vec<Value<'h>>, &'static str> {
    match file_to_data(source) {
        IResult::Error(_e) => Err("syntax error"),
        IResult::Incomplete(_e) => Err("incomplete input"),
        IResult::Done(leftovers, data) => {
            // The parser uses `eof!()` to ensure that on success, all input has been consumed.
            assert!(leftovers.is_empty(), "parser did not consume all input");

            Ok(data.into_iter()
                .map(|d| datum_to_value(hs, d))
                .collect::<Result<Vec<_>, _>>()?)
        }
    }
}
