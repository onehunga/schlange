use clap::Parser;

use schlange::{lexer::Lexer, token::Token};

#[derive(Parser)]
struct Args {
	pub file: String
}

fn main() {
	let args = Args::parse();
	let src = std::fs::read_to_string(args.file).unwrap();

	let mut lex = Lexer::new(&src);
	loop {
		let tok = lex.next();

		println!("{tok:?}");

		if tok == Token::Eof {
			break;
		}
	}
}
