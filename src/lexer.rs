use core::fmt;
use std::hash::Hash;
use bigdecimal::{BigDecimal, FromPrimitive};
use chumsky::prelude::*;
use chumsky::text;

#[derive(Debug, Clone, Eq, PartialOrd, PartialEq, Hash)]
pub enum Token {
    Error,
    Comment,

    Number(BigDecimal),
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


impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Copy,Clone)]
struct Base {
    radix: u8
}

impl Base {
    const HEX: Base = Base { radix: 16 };
    const OCT: Base = Base { radix: 8 };
    const BIN: Base = Base { radix: 2 };
    const DEC: Base = Base { radix: 10 };
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

pub fn parser() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let int_semi = |radix| text::digits(radix)
        .separated_by(just("_"))
        .at_least(1)
        .map(|s: Vec<String>| s.join(""));

    let int =
        choice::<_, Simple<char>>([
            just("0x").to(Base::HEX),
            just("0b").to(Base::BIN),
            just("0o").to(Base::OCT),])
            .then_with(move |base: Base| int_semi(base.radix as u32)
                .map(move |s: String| u64::from_str_radix(s.as_str(), base.radix as u32).unwrap() as f64)
            )
            .labelled("int");

    let float = int_semi(10)
        .separated_by(just('.'))
        .at_least(1)
        .at_most(2)
        .map(|s: Vec<String>| s.join("."))
        .map(|s: String| s.parse().unwrap())
        .labelled("float");

    let number = int.or(float)
        .padded()
        .map(|v| Token::Number(BigDecimal::from_f64(v).unwrap()))
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
        .delimited_by(just("//"), text::newline().or(end().rewind()))
        .map(|_| Token::Comment)
        .labelled("comment");
    
    let multi_comment = none_of("*/")
        .repeated()
        .delimited_by(just("/*"), just("*/"))
        .map(|_| Token::Comment)
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
    )).recover_with(skip_parser(any().map(|_| Token::Error)))
        .padded()
        .repeated()
        .then_ignore(end())
}