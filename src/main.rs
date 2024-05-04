use std::error::Error;
use std::io::Write;

use chumsky::error::Cheap;
use chumsky::prelude::*;
use line_span::LineSpanExt;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use crate::lexer::{print_tokens, Token, TokenPrintConfig};

mod parse;
mod lexer;

macro_rules! errdef {
    ($name:ident,$msg:expr) => {
        #[derive(Debug, Clone)]
        struct $name;

        impl Error for $name {}

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, $msg)
            }
        }
    };
}

errdef!(BrokenParserError, "Returned parser index range broken");

fn print_err_lexer(err: Cheap<char>, src: &String, tokens: &[Token]) -> Result<(), Box<dyn Error>> {
    let (id, _) = src.line_spans()
        .enumerate()
        .find(|(_,span)| err.span().start >= span.range().start && err.span().start < span.range().end)
        .ok_or(BrokenParserError {})?;

    let tk = lexer::nth_line(tokens, id).tokens;
    let msg = err.label().map_or_else(
        || format!("In line {}: Unexpected", id + 1),
        |label| format!("In line {}: {}", id + 1, label),
    );
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
    writeln!(&mut stdout, "{}", msg.as_str())?;
    write!(&mut stdout, "  ")?;
    print_tokens(tk.as_slice(), TokenPrintConfig::new()
        .with_line_limit(1)
        .with_highlight_fn(|f| match f {
            Token::Error(_) => Some(Color::Red),
            _ => None
        }))?;
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
    writeln!(&mut stdout, "\n")?;
    stdout.reset()?;
    Ok(())
}

fn print_err_parser(err: Simple<Token>, tokens: &[Token]) -> Result<(), Box<dyn Error>> {
    let line = lexer::line_at(tokens, err.span().start);
    let tk = tokens.get(line).ok_or(BrokenParserError {})?;
    let line_id = lexer::line_index(tokens, err.span().start);

    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
    let msg = err.label().map_or_else(
        || format!("In line {}: {:?}", line_id + 1, err.reason()),
        |label| format!("In line {}: {}: {:?}", line_id + 1, label, err.reason()),
    );
    writeln!(&mut stdout, "{}", msg.as_str())?;
    if err.expected().count() > 0 {
        write!(&mut stdout, "Expected one of: ")?;
        for x in err.expected() {
            if x.is_none() {
                continue
            }
            let x = x.as_ref().unwrap();
            print_tokens([x.clone()].as_slice(), TokenPrintConfig::new()
                .without_breaks())?;
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
            write!(&mut stdout, " ")?;
        }
        writeln!(&mut stdout)?;
    }
    if err.found().is_some() {
        write!(&mut stdout, "Found: ")?;
        let found = err.found().unwrap();
        print_tokens([found.clone()].as_slice(), TokenPrintConfig::new()
            .without_breaks())?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
        writeln!(&mut stdout)?;
    }
    write!(&mut stdout, "  ")?;
    print_tokens(tk, TokenPrintConfig::new()
        .with_line_limit(1)
        .with_highlight(err.span(), Color::Red))?;
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
    writeln!(&mut stdout, "\n")?;
    stdout.reset()?;
    Ok(())
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
        print_err_lexer(err, &src, tokens.as_slice()).unwrap();
    }

    let (ast, errs1) = parse::parser().parse_recovery(tokens.as_slice());
    for err in errs1 {
        print_err_parser(err, tokens.as_slice()).unwrap();
    }

    println!("{:#?}", ast);
}