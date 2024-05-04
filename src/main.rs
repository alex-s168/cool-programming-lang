mod parse;
mod lexer;

use chumsky::prelude::*;
use std::io::{self, Write};
use chumsky::error::Cheap;
use line_span::LineSpanExt;
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

fn print_err_lexer(err: Cheap<char>, src: &String, tokens: &[lexer::Token]) {
    src.line_spans()
        .enumerate()
        .find(|(_,span)| err.span().start >= span.range().start && err.span().start <= span.range().end)
        .map(|(id,_)| {
            let tk = lexer::nth_line(tokens, id).tokens;
            let msg = err.label().map_or_else(
                || format!("In line {}:", id + 1),
                |label| format!("In line {}: {}", id + 1, label),
            );
            let mut stdout = StandardStream::stdout(ColorChoice::Always);
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
            writeln!(&mut stdout, "{}", msg.as_str())?;
            print_tokens(tk.as_slice())?;
            writeln!(&mut stdout)
        });
}

fn print_err_parser(err: Cheap<lexer::Token>, tokens: &[lexer::Token]) {
    
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
        eprintln!("Fatal lexer error!");
        std::process::exit(1);
    });
    for err in errs0 {
        print_err_lexer(err, &src, tokens.as_slice());
    }
    
    let (ast, errs1) = parse::parser().parse_recovery(tokens.as_slice());
    for err in errs1 {
        print_err_parser(err, tokens.as_slice());
    }

    println!("{:#?}", ast);
}