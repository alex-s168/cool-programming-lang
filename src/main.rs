mod parse;
mod lexer;

use std::error::Error;
use chumsky::prelude::*;
use std::io::{self, Write};
use std::ops::{Index, Range};
use chumsky::error::Cheap;
use line_span::LineSpanExt;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use crate::lexer::Token;

fn simple_color_lut(tok: &Token) -> Color {
    match tok {
        Token::String(_) => Color::Rgb(60, 138, 58),
        Token::Number(_, _) => Color::Rgb(0, 105, 204),
        Token::Ident(_) => Color::Rgb(152, 177, 184),
        Token::MultiComment(_) => Color::Rgb(97, 97, 97),
        Token::SingleComment(_) => Color::Rgb(97, 97, 97),
        Token::Error(_) => Color::Rgb(255, 0, 0),
        Token::KWType => Color::Rgb(179, 62, 0),
        _ => Color::White
    }
}

#[derive(Clone)]
struct TokenPrintConfig {
    colorscheme: fn(&Token) -> Color,
    at_most_lines: Option<usize>,
    highlight: Option<(Range<usize>, Color)>,
    highlight_auto: Option<fn(&Token) -> Option<Color>>,
    no_break: bool,
}

impl TokenPrintConfig {
    fn new() -> TokenPrintConfig {
        TokenPrintConfig {
            colorscheme: simple_color_lut,
            at_most_lines: None,
            highlight: None,
            no_break: false,
            highlight_auto: None,
        }
    }

    fn with_colors(&mut self, scheme: fn(&Token) -> Color) -> &mut TokenPrintConfig {
        self.colorscheme = scheme;
        self
    }

    fn with_line_limit(&mut self, limit: usize) -> &mut TokenPrintConfig {
        self.at_most_lines = Some(limit);
        self
    }

    fn with_highlight(&mut self, range: Range<usize>, color: Color) -> &mut TokenPrintConfig {
        self.highlight = Some((range, color));
        self
    }
    
    fn with_highlight_fn(&mut self, f: fn(&Token) -> Option<Color>) -> &mut TokenPrintConfig {
        self.highlight_auto = Some(f);
        self
    }

    fn without_breaks(&mut self) -> &mut TokenPrintConfig {
        self.no_break = true;
        self
    }
}

fn print_tokens(tokens: &[lexer::Token], config: &TokenPrintConfig) -> io::Result<()>
{
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    let mut line: usize =  0;

    for (id, tok) in tokens.iter().enumerate() {
        let off = tok.clone().count_lines_off();

        let color = (config.colorscheme)(tok);

        let mut str = String::new();
        tok.clone().get_source_str(&mut str);
        if config.no_break {
            str.retain(|v| v != '\n');
        }
        let mut strstr = str.as_str();
        if config.at_most_lines.is_some() {
            if line + off >= config.at_most_lines.unwrap() {
                let nl = str.find('\n').unwrap();
                strstr = str.index(Range { start: 0, end: nl });
            }
        }
        if config.highlight.is_some() {
            let (hl,col) = config.highlight.as_ref().unwrap();
            if id >= hl.start && id < hl.end {
                stdout.set_color(ColorSpec::new()
                    .set_fg(Some(color))
                    .set_bg(Some(*col)))?;
            } else {
                stdout.set_color(ColorSpec::new()
                    .set_fg(Some(color)))?;
            }
        }
        else {
            stdout.set_color(ColorSpec::new()
                .set_fg(Some(color))
                .set_bg(config.highlight_auto
                    .and_then(|f| f(tok))))?;
        }
        write!(&mut stdout, "{}", strstr)?;

        if config.at_most_lines.is_some() {
            line += off;
            if line >= config.at_most_lines.unwrap() {
                break
            }
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