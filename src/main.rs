mod parse;
mod lexer;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;

fn main() {
    let src = std::env::args().nth(1)
        .and_then(|it| std::fs::read_to_string(it).ok())
        .unwrap_or_else(|| {
            eprintln!("Specify a valid file as first argument!");
            std::process::exit(1);
        });

    let (tokens, errs0) = lexer::parser().parse_recovery(src.as_str());
    for e in errs0 {
        Report::build(ReportKind::Error, (), e.span().start)
            .with_message(e.to_string())
            .with_label(Label::new(e.span())
                            .with_message(e.label().unwrap_or("Lexing error occurred!"))
                            .with_color(Color::Red),
            )
            .finish()
            .print(Source::from(&src))
            .unwrap();
    };

    println!("{:?}", tokens);

    let (ast, errs1) = parse::parser().parse_recovery(tokens.unwrap_or_else(|| {
        std::process::exit(1);
    }));

    println!("{:#?}", ast);

    errs1.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, (), e.span().start)
            .with_message(e.to_string())
            .with_label(Label::new(e.span())
                            .with_message(e.label().unwrap_or("Parsing error occurred!"))
                            .with_color(Color::Red),
            )
            .finish()
            .print(Source::from(&src))
            .unwrap();
    });
}