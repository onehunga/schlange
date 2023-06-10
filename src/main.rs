use clap::Parser;
use schlange::ast;

#[derive(Parser)]
struct Args {
	pub file: String
}

fn main() {
	let args = Args::parse();
	let src = std::fs::read_to_string(args.file).unwrap();

	let mut parser = schlange::parser::Parser::new(&src);
	let stmts = parser.parse().unwrap();

	stmts.iter().for_each(|stmt| match stmt {
		ast::Statement::Expression(expr) => ast::print_expr(expr.as_ref(), 0),
		_ => {}
	});
}
