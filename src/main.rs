mod parse;
mod lexer;

use chumsky::prelude::*;
use std::io::{self, Write};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

fn print_tokens(tokens: &[lexer::Token]) -> io::Result<()> {
    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    for tok in tokens {
        let color = match tok {
            lexer::Token::String(_) => Color::Rgb(60, 138, 58),
            lexer::Token::Number(_,_) => Color::Rgb(0, 105, 204),
            lexer::Token::Ident(_) => Color::Rgb(152, 177, 184),
            lexer::Token::MultiComment(_) => Color::Rgb(97, 97, 97),
            lexer::Token::SingleComment(_) => Color::Rgb(97, 97, 97),
            lexer::Token::Error(_) => Color::Rgb(255, 0, 0),
            lexer::Token::KWType => Color::Rgb(179, 62, 0),
            _ => Color::White
        };

        stdout.set_color(ColorSpec::new().set_fg(Some(color)))?;

        let mut str = String::new();
        tok.clone().get_source_str(&mut str);
        write!(&mut stdout, "{}", str.as_str())?;
    }

    stdout.flush()
}

fn main() {
    let src = std::env::args().nth(1)
        .and_then(|it| std::fs::read_to_string(it).ok())
        .unwrap_or_else(|| {
            eprintln!("Specify a valid file as first argument!");
            std::process::exit(1);
        });

    let (tokens, errs0) = lexer::parser().parse_recovery(src.as_str());

    let tokens = tokens.unwrap_or_else(|| {
        std::process::exit(1);
    });

    print_tokens(tokens.as_slice()).unwrap();
/*
    println!("{}", lexer::get_source_str(tokens.as_slice()));

    let (ast, errs1) = parse::parser().parse_recovery(tokens);

    println!("{:#?}", ast);

 */
}