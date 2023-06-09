use clap::Parser;
use schlange::print::{print_statement, TAB};

#[derive(Parser)]
struct Args {
	pub file: String,

	#[arg(default_value_t = 2)]
	pub print_tab_size: usize,
}

fn main() {
	let args = Args::parse();
	let src = std::fs::read_to_string(args.file).unwrap();
	unsafe { TAB = args.print_tab_size };

	let mut parser = schlange::parser::Parser::new(&src);

	match parser.parse() {
		Ok(stmts) => stmts.into_iter().for_each(|stmt| print_statement(&stmt, 0)),
		Err(err) => eprintln!("{err:?}"),
	}
}