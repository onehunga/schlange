use clap::Parser;
use schlange::ast;

#[derive(Parser)]
struct Args {
	pub file: String,
	pub print_tab_size: Option<usize>,
}

fn main() {
	let args = Args::parse();
	let src = std::fs::read_to_string(args.file).unwrap();
	let print_tab_size = match args.print_tab_size {
		Some(size) => size,
		None => 2,
	};

	let mut parser = schlange::parser::Parser::new(&src);
	let stmts = parser.parse().unwrap();

	stmts.iter().for_each(|stmt| match stmt {
		ast::Statement::Expression(expr) => ast::print_expr(expr.as_ref(), print_tab_size, 0),
		_ => {}
	});
}