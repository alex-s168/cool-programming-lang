mod parse;
mod lexer;

use std::error::Error;
use chumsky::prelude::*;
use std::io::{self, Write};
use std::ops::{Index, Range};
use std::ptr::write;
use chumsky::error::Cheap;
use line_span::LineSpanExt;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

fn simple_color_lut(tok: &lexer::Token) -> Color {
    match tok {
        lexer::Token::String(_) => Color::Rgb(60, 138, 58),
        lexer::Token::Number(_, _) => Color::Rgb(0, 105, 204),
        lexer::Token::Ident(_) => Color::Rgb(152, 177, 184),
        lexer::Token::MultiComment(_) => Color::Rgb(97, 97, 97),
        lexer::Token::SingleComment(_) => Color::Rgb(97, 97, 97),
        lexer::Token::Error(_) => Color::Rgb(255, 0, 0),
        lexer::Token::KWType => Color::Rgb(179, 62, 0),
        _ => Color::White
    }
}

fn print_tokens<F>(tokens: &[lexer::Token], lut: F) -> io::Result<()>
    where F: Fn(&lexer::Token) -> Color
{
    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    for tok in tokens {
        let color = lut(tok);
        stdout.set_color(ColorSpec::new().set_fg(Some(color)))?;

        let mut str = String::new();
        tok.clone().get_source_str(&mut str);
        write!(&mut stdout, "{}", str.as_str())?;
    }

    stdout.flush()?;
    stdout.reset()
}

fn print_tokens_at_most_lines<F>(tokens: &[lexer::Token], lut: F, lines: usize) -> io::Result<()>
    where F: Fn(&lexer::Token) -> Color
{
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    let mut line: usize =  0;

    for tok in tokens {
        let off = tok.clone().count_lines_off();

        let color = lut(tok);
        stdout.set_color(ColorSpec::new().set_fg(Some(color)))?;

        let mut str = String::new();
        tok.clone().get_source_str(&mut str);
        let mut strstr = str.as_str();
        if line + off >= lines {
            let nl = str.find('\n').unwrap();
            strstr = str.index(Range { start: 0, end: nl });
        }
        write!(&mut stdout, "{}", strstr)?;

        line += off;
        if line >= lines {
            break
        }
    }

    stdout.flush()?;
    stdout.reset()
}

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

fn print_err_lexer(err: Cheap<char>, src: &String, tokens: &[lexer::Token]) -> Result<(), Box<dyn Error>> {
    let (id, _) = src.line_spans()
        .enumerate()
        .find(|(_,span)| err.span().start >= span.range().start && err.span().start <= span.range().end)
        .ok_or(BrokenParserError {})?;

    let tk = lexer::nth_line(tokens, id).tokens;
    let msg = err.label().map_or_else(
        || format!("In line {}:", id + 1),
        |label| format!("In line {}: {}", id + 1, label),
    );
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
    writeln!(&mut stdout, "{}", msg.as_str())?;
    write!(&mut stdout, "  ")?;
    print_tokens_at_most_lines(tk.as_slice(), simple_color_lut, 1)?;
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
    writeln!(&mut stdout, "\n")?;
    stdout.reset()?;
    Ok(())
}

fn print_err_parser(err: Simple<lexer::Token>, tokens: &[lexer::Token]) -> Result<(), Box<dyn Error>> {
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
            print_tokens([x.clone()].as_slice(), simple_color_lut)?;
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
            write!(&mut stdout, " ")?;
        }
        writeln!(&mut stdout)?;
    }
    if err.found().is_some() {
        write!(&mut stdout, "Found: ")?;
        let found = err.found().unwrap();
        print_tokens([found.clone()].as_slice(), simple_color_lut)?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
        writeln!(&mut stdout)?;
    }
    write!(&mut stdout, "  ")?;
    print_tokens_at_most_lines(tk, simple_color_lut, 1)?;
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