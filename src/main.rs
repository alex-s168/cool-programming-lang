use chumsky::prelude::*;
use chumsky::text;

#[derive(Debug)]
struct IdentSegment {
    value: String,
    type_args: TypeArgs,
}

type Ident = Vec<IdentSegment>;

// DIFFERENT FROM TYPE CONSTRAINTS!!!
#[derive(Debug)]
struct TypeArgSegment {
    unroll: bool,
    value: Ident,
}

type TypeArgs = Vec<TypeArgSegment>;

#[derive(Debug)]
struct ArrSegment {
    unroll: bool,
    source: Box<Expr>,
}

type Arr = Vec<ArrSegment>;

#[derive(Debug)]
struct InitializerSegment {
    name: Option<String>,
    value: Box<Expr>,
}

type Initializer = Vec<InitializerSegment>;

#[derive(Debug)]
enum Expr {
    Num(f64),
    Var(Ident),
    Array(Arr),
    Struct(Ident, Initializer),

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

fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = recursive(|ident|
        text::ident::<_, Simple<char>>()
            .then(ident
                .then(just("..")
                    .ignored()
                    .or_not())
                .map(|(elem,unroll)| TypeArgSegment {
                    value: elem,
                    unroll: unroll.map(|_|true).unwrap_or(false),
                })
                .padded()
                .separated_by(just(','))
                .allow_leading()
                .delimited_by(just('<'), just('>'))
                .or_not()
                .map(|v| v.unwrap_or(vec!()) as TypeArgs))
            .map(|(v,a)| IdentSegment {
                value: v,
                type_args: a
            })
            .separated_by(just("::"))
            .allow_leading()
            .at_least(1)
            .map(|elems| elems as Ident)
    );

    let expr: Recursive<char, Expr, _> = recursive(|expr| {
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
            );

        let float = int_semi(10)
            .separated_by(just('.'))
            .at_least(1)
            .at_most(2)
            .map(|s: Vec<String>| s.join("."))
            .map(|s: String| s.parse().unwrap());

        let number = int.or(float).padded()
            .map(|v| Expr::Num(v));

        let var = ident.clone()
            .map(|v| Expr::Var(v));

        let initializer_list = text::ident::<_, Simple<char>>()
            .padded()
            .then_ignore(just(':').padded())
            .then(expr.clone())
            .map(|(member,value)| InitializerSegment { name: Some(member), value: Box::new(value) })
            .or(expr.clone().map(|value| InitializerSegment { name: None, value: Box::new(value) }))
            .padded()
            .separated_by(just(','));

        let inner_list = expr.clone()
            .then(just("..").ignored().or_not())
            .map(|(elem, unroll)| ArrSegment {
                unroll: unroll.map(|_|true).unwrap_or(false),
                source: Box::new(elem),
            })
            .padded()
            .separated_by(just(','));

        let atom = number
            .or(text::keyword("type")
                .then(ident.clone()
                    .delimited_by(just('('), just(')')))
                .map(|(_, ident)| Expr::TypeOf(ident))
                .labelled("type() builtin"))
            .or(expr.clone()
                .delimited_by(just('('), just(')')))
            .or(inner_list.clone()
                .delimited_by(just('['), just(']'))
                .map(|v| Expr::Array(v)))
            .or(ident.clone()
                .padded()
                .then(initializer_list
                    .delimited_by(just('{'), just('}')))
                .map(|(name,init)| Expr::Struct(name, init)))
            .or(var)
            .padded();

        let op = |c| just(c).padded();

        let member_ref = atom
            .then(op('.')
                .then(text::ident::<_, Simple<char>>())
                .repeated())
            .foldl(|a, (_, b)| Expr::MemberRef(Box::new(a), b));

        let arr_index = member_ref
            .then(expr.clone()
                .delimited_by(just('['), just(']'))
                .repeated())
            .foldl(|a, index| Expr::ArrIndex { arr: Box::new(a), index: Box::new(index) });
        
        let call = arr_index
            .then(inner_list
                .delimited_by(just('('), just(')'))
                .repeated())
            .foldl(|a, args| Expr::Call(Box::new(a), args));

        let unary = op('-').to(Expr::Neg as fn(_) -> _)
            .or(op('&').to(Expr::Ref as fn(_) -> _))
            .repeated()
            .then(call)
            .foldr(|op, old| op(Box::new(old)));

        let product = unary.clone()
            .then(op('*').to(Expr::Mul as fn(_, _) -> _)
                .or(op('/').to(Expr::Div as fn(_, _) -> _))
                .then(unary)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product.clone()
            .then(op('+').to(Expr::Add as fn(_, _) -> _)
                .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                .then(product)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        sum
    });

    expr.then_ignore(end())
}

fn main() {
    let src = std::env::args().nth(1)
        .and_then(|it| std::fs::read_to_string(it).ok())
        .unwrap_or_else(|| {
            eprintln!("Specify a valid file as first argument!");
            std::process::exit(1);
        });

    println!("{:?}", parser().parse(src));
}