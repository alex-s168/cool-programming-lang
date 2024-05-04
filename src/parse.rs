use bigdecimal::Num;
use chumsky::error::Simple;
use chumsky::prelude::*;

use crate::lexer::Token;

#[derive(Debug)]
pub struct IdentSegment {
    value: String,
    type_args: TypeArgs,
}

pub type Ident = Vec<IdentSegment>;

// DIFFERENT FROM TYPE CONSTRAINTS!!!
#[derive(Debug)]
pub struct TypeArgSegment {
    unroll: bool,
    value: Ident,
}

pub type TypeArgs = Vec<TypeArgSegment>;

#[derive(Debug)]
pub struct ArrSegment {
    unroll: bool,
    source: Box<Expr>,
}

pub type Arr = Vec<ArrSegment>;

#[derive(Debug)]
pub struct InitializerSegment {
    name: Option<String>,
    value: Box<Expr>,
}

pub type Initializer = Vec<InitializerSegment>;

#[derive(Debug)]
pub enum Expr {
    Err,

    Num(f64),
    Var(Ident),
    Array(Arr),
    Struct(Ident, Initializer),
    String(String),

    MemberRef(Box<Expr>, String),
    ArrIndex {
        arr: Box<Expr>,
        index: Box<Expr>,
    },
    Call(Box<Expr>, Arr),

    TypeOf(Ident),

    Neg(Box<Expr>),
    Ref(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
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

pub fn parser() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    macro_rules! annoying0 {
        ($ty:path) => {
            filter(|tk: &Token| matches!(tk, $ty(_)))
                .map(|tk| {
                    match tk { $ty(v) => v, _ => panic!() }
                })
        };
    }

    macro_rules! annoying2 {
        ($ty:path) => {
            filter(|tk: &Token| matches!(tk, $ty(_,_)))
                .map(|tk| {
                    match tk { $ty(a, b) => (a,b), _ => panic!() }
                })
        };
    }

    macro_rules! annoying1 {
        ($ty:path) => {
            filter(|tk: &Token| match tk { $ty => true, _ => false })
                .ignored()
        };
    }

    let tok_ident = annoying0!(Token::Ident);
    let tok_num = annoying2!(Token::Number);
    let tok_str = annoying0!(Token::String);

    fn padded<'a, O: 'a, P: 'a>(parser: P) -> BoxedParser<'a, Token, O, Simple<Token>>
        where P: Parser<Token, O, Error = Simple<Token>> + Clone
    {
        let tok_err = annoying0!(Token::Error);
        let tok_multi = annoying0!(Token::MultiComment);
        let tok_single = annoying0!(Token::SingleComment);
        let padding = annoying0!(Token::Padding);

        let errs = choice([
                tok_err.ignored().boxed(),
                tok_multi.ignored().boxed(),
                tok_single.ignored().boxed(),
                padding.ignored().boxed(),
        ]).repeated();

        errs.clone()
            .then_with::<_, P, _>(move |_| parser.clone())
            .then_ignore(errs.clone())
            .boxed()
    }

    let just = |tk: Token| padded(just(tk));

    let ident: Recursive<Token, Ident, _> = recursive(|ident|
        tok_ident
            .then(ident
                .then(just(Token::NamespaceColons)
                    .ignored()
                    .or_not())
                .map(|(elem,unroll)| TypeArgSegment {
                    value: elem,
                    unroll: unroll.map(|_|true).unwrap_or(false),
                })
                .separated_by(just(Token::Comma))
                .allow_leading()
                .delimited_by(just(Token::AngleOpen), just(Token::AngleClose))
                .or_not()
                .map(|v| v.unwrap_or(vec!()) as TypeArgs))
            .map(|(v,a)| IdentSegment {
                value: v,
                type_args: a
            })

            .separated_by(just(Token::NamespaceColons))
            .allow_leading()
            .at_least(1)
            .map(|elems| elems as Ident)
    );

    let expr: Recursive<Token, Expr, _> = recursive(|expr| {
        let var = ident.clone()
            .map(|v| Expr::Var(v));

        let initializer_list = tok_ident
            .then_ignore(just(Token::Colon))
            .then(expr.clone())
            .map(|(member,value)| InitializerSegment { name: Some(member), value: Box::new(value) })
            .or(expr.clone().map(|value| InitializerSegment { name: None, value: Box::new(value) }))
            .separated_by(just(Token::Comma));

        let inner_list = expr.clone()
            .then(just(Token::UnrollDots).ignored().or_not())
            .map(|(elem, unroll)| ArrSegment {
                unroll: unroll.map(|_|true).unwrap_or(false),
                source: Box::new(elem),
            })
            .separated_by(just(Token::Comma));

        let atom = choice::<_, Simple<Token>>((
            tok_num.map(|(s,b)| Expr::Num(
                f64::from_str_radix(s.as_str(), b.radix as u32).unwrap()
            )),

            tok_str.map(|v| Expr::String(v)),

            padded(annoying1!(Token::KWType))
                .then(ident.clone()
                    .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
                    .labelled("type() builtin"))
                .map(|(_, ident)| Expr::TypeOf(ident))
                .labelled("type() builtin")
                .recover_with(nested_delimiters(Token::ParenOpen, Token::ParenClose, [], |_| Expr::Err)),

            expr.clone()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),

            inner_list.clone()
                .delimited_by(just(Token::SquareOpen), just(Token::SquareClose))
                .map(|v| Expr::Array(v))
                .labelled("array initialization")
                .recover_with(nested_delimiters(Token::SquareOpen, Token::SquareClose, [], |_| Expr::Err)),

            ident.clone()
                .then(initializer_list
                    .delimited_by(just(Token::CurlyOpen), just(Token::CurlyClose)))
                .map(|(name,init)| Expr::Struct(name, init))
                .labelled("struct initialization")
                .recover_with(nested_delimiters(Token::CurlyOpen, Token::CurlyClose, [], |_| Expr::Err)),

            var,
        ));

        let member_ref = atom
            .then(padded(annoying1!(Token::Dot))
                .then(tok_ident)
                .labelled("member access")
                .repeated())
            .foldl(|a, (_, b)| Expr::MemberRef(Box::new(a), b));

        let arr_index = member_ref
            .then(expr.clone()
                .delimited_by(just(Token::SquareOpen), just(Token::SquareClose))
                .labelled("array index")
                .repeated())
            .foldl(|a, index| Expr::ArrIndex { arr: Box::new(a), index: Box::new(index) });

        let call = arr_index
            .then(inner_list
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
                .labelled("function call")
                .recover_with(nested_delimiters(Token::ParenOpen, Token::ParenClose, [], |_| vec!()))
                .repeated())
            .foldl(|a, args| Expr::Call(Box::new(a), args));

        let unary = padded(annoying1!(Token::Minus)).to(Expr::Neg as fn(_) -> _)
            .or(padded(annoying1!(Token::Ref)).to(Expr::Ref as fn(_) -> _))
            .repeated()
            .then(call)
            .foldr(|op, old| op(Box::new(old)));

        let product = unary.clone()
            .then(padded(annoying1!(Token::Mul)).to(Expr::Mul as fn(_, _) -> _)
                .or(padded(annoying1!(Token::Div)).to(Expr::Div as fn(_, _) -> _))
                .then(unary)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product.clone()
            .then(padded(annoying1!(Token::Plus)).to(Expr::Add as fn(_, _) -> _)
                .or(padded(annoying1!(Token::Minus)).to(Expr::Sub as fn(_, _) -> _))
                .then(product)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        sum
    });

    padded(expr).then_ignore(end())
}