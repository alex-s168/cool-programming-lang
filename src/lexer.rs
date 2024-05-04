use core::fmt;
use std::hash::Hash;
use std::ops::Range;
use chumsky::error::Cheap;
use chumsky::prelude::*;
use chumsky::text;

#[derive(Debug, Clone, Eq, PartialOrd, PartialEq, Hash)]
pub enum Token {
    Error(/* source that errored */ char),
    MultiComment(String),
    SingleComment(String),
    Padding(/* source */ char),

    Number(String, Base),
    String(String),
    Ident(String),
    
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
    AngleOpen,
    AngleClose,
    Comma,
    UnrollDots,
    Dot,
    NamespaceColons,
    Colon,

    Ref,
    Minus,
    Plus,
    Mul,
    Div,

    KWType,
}

impl Token {
    pub fn count_lines_off(self) -> usize {
        match self {
            Token::MultiComment(s) => s.chars().into_iter().filter(|c| *c == '\n').count(),
            Token::Padding(s) => if s == '\n' { 1 } else { 0 },
            Token::String(s) => s.chars().into_iter().filter(|c| *c == '\n').count(),
            _ => 0
        }
    }

    pub fn get_source_str(self, dest: &mut String) {
        match self {
            Token::Error(c) => dest.push(c),
            Token::MultiComment(s) => {
                dest.push_str("/*");
                dest.push_str(s.as_str());
                dest.push_str("*/");
            }
            Token::SingleComment(s) => {
                dest.push_str("//");
                dest.push_str(s.as_str());
            }
            Token::Padding(c) => {
                dest.push(c);
            }
            
            Token::Number(s, base) => {
                match base {
                    Base::DEC => {}
                    Base::HEX => dest.push_str("0x"),
                    Base::OCT => dest.push_str("0o"),
                    Base::BIN => dest.push_str("0b"),
                    _ => panic!()
                }
                dest.push_str(s.as_str());
            }
            Token::String(s) => {
                dest.push('"');
                dest.push_str(s.as_str());
                dest.push('"')
            }
            Token::Ident(s) => dest.push_str(s.as_str()),

            Token::ParenOpen => dest.push('('),
            Token::ParenClose => dest.push(')'),
            Token::CurlyOpen => dest.push('{'),
            Token::CurlyClose => dest.push('}'),
            Token::SquareOpen => dest.push('['),
            Token::SquareClose => dest.push(']'),
            Token::AngleOpen => dest.push('<'),
            Token::AngleClose => dest.push('>'),
            Token::Comma => dest.push(','),
            Token::UnrollDots => dest.push_str(".."),
            Token::Dot => dest.push('.'),
            Token::NamespaceColons => dest.push_str("::"),
            Token::Colon => dest.push(':'),

            Token::Ref => dest.push('&'),
            Token::Minus => dest.push('-'),
            Token::Plus => dest.push('+'),
            Token::Mul => dest.push('*'),
            Token::Div => dest.push('/'),

            Token::KWType => dest.push_str("type"),
        }
    }
}

pub struct NthLine {
    pub off_up_from_wanted: usize,
    pub tokens: Vec<Token>
}

pub fn nth_line(tokens: &[Token], want: usize) -> NthLine {
    let mut res = Vec::new();

    let mut curr = 0;

    for token in tokens {
        let off = token.clone().count_lines_off();
        if curr == want && curr + off == want {
            res.push(token.clone())
        } else if curr == want && curr + off > want {
            res.push(token.clone());
            return NthLine {
                off_up_from_wanted: curr + off - want,
                tokens: res,
            };
        }
        curr += off;
    }

    NthLine {
        off_up_from_wanted: 0,
        tokens: res,
    }
}

pub fn line_at(tokens: &[Token], index: usize) -> Range<usize> {
    let mut start = index;
    loop {
        if start == 0 {
            break
        }
        let off = tokens[start - 1].clone().count_lines_off();
        if off > 0 {
            break
        }
        start -= 1;
    }

    let mut end = index;
    loop {
        if end >= tokens.len() {
            break
        }
        let off = tokens[end].clone().count_lines_off();
        if off > 0 {
            end += 1;
            break
        }
        end += 1;
    }

    Range { start, end }
}

pub fn line_index(tokens: &[Token], index: usize) -> usize {
    let mut sum = 0;
    let mut index = index;
    loop {
        sum += tokens[index].clone().count_lines_off();
        if index == 0 {
            break
        }
        index -= 1;
    }
    
    sum
}

pub fn get_source_str(tokens: &[Token]) -> String {
    let mut buf = String::new();
    for tok in tokens {
        tok.clone().get_source_str(&mut buf)
    }
    buf
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialOrd, PartialEq, Hash)]
pub struct Base {
    pub radix: u8
}

impl Base {
    pub const HEX: Base = Base { radix: 16 };
    pub const OCT: Base = Base { radix: 8 };
    pub const BIN: Base = Base { radix: 2 };
    pub const DEC: Base = Base { radix: 10 };
}

pub const SIMPLE_TOKENS: [char; 16] = [
    '(', ')',
    '{', '}',
    '[', ']',
    '<', '>',
    ',',
    '.',
    ':',
    '&',
    '+',
    '-',
    '*',
    '/',
];

pub fn parser() -> impl Parser<char, Vec<Token>, Error = Cheap<char>> {
    let int =
        choice::<_, Cheap<char>>([
            just("0x").to(Base::HEX),
            just("0b").to(Base::BIN),
            just("0o").to(Base::OCT),
        ])
        .then_with(move |base: Base| text::digits(base.radix as u32)
            .map(move |s: String| Token::Number(s, base))
        )
        .labelled("int");

    let float = text::digits(10)
        .separated_by(just('.'))
        .at_least(1)
        .at_most(2)
        .map(|s: Vec<String>| s.join("."))
        .map(|s: String| Token::Number(s, Base::DEC))
        .labelled("float");

    let number = int.or(float)
        .labelled("number");

    let char = just('\\')
        .then(choice([
                just('n').to('\n'),
                just('"').to('"'),
                just('t').to('\t'),
                just('0').to('\0'),
                just('\\').to('\\'),
            ])
            .recover_with(skip_parser(any())))
        .map(|(_,b)| b)
        .labelled("escape sequence")
        .or(filter(|c| !['"', '\\'].contains(c)));

    let string = char
        .repeated()
        .collect()
        .map(|str: String| Token::String(str))
        .delimited_by(just('"'), just('"'))
        .recover_with(skip_parser(
            just('"')
                .then(none_of(SIMPLE_TOKENS)
                    .repeated()
                    .collect()
                    .then_ignore(one_of(SIMPLE_TOKENS)
                        .rewind())
                    .map(|str: String| Token::String(str.trim_end().parse().unwrap())))
                .map(|(_, v)| v)))
        .labelled("string");

    let single_comment = text::newline()
        .not()
        .repeated()
        .collect()
        .delimited_by(just("//"), text::newline().or(end().rewind()))
        .map(|txt: String| Token::SingleComment(txt))
        .labelled("comment");
    
    let multi_comment = none_of("*/")
        .repeated()
        .collect()
        .delimited_by(just("/*"), just("*/"))
        .map(|txt: String| Token::MultiComment(txt))
        .labelled("comment");
    
    choice((
        single_comment,
        multi_comment,
        text::keyword("type").to(Token::KWType),
        string,
        number,
        text::ident().map(|v| Token::Ident(v)),
        just('(').to(Token::ParenOpen),
        just(')').to(Token::ParenClose),
        just('{').to(Token::CurlyOpen),
        just('}').to(Token::CurlyClose),
        just('[').to(Token::SquareOpen),
        just(']').to(Token::SquareClose),
        just('<').to(Token::AngleOpen),
        just('>').to(Token::AngleClose),
        just(',').to(Token::Comma),
        just("..").to(Token::UnrollDots),
        just('.').to(Token::Dot),
        just("::").to(Token::NamespaceColons),
        just(':').to(Token::Colon),

        just('&').to(Token::Ref),
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Mul),
        just('/').to(Token::Div),
        
        one_of(['\n','\t','\r',' ']).map(|c| Token::Padding(c)),
    )).recover_with(skip_parser(any().map(|c| Token::Error(c))))
        .repeated()
        .then_ignore(end())
}