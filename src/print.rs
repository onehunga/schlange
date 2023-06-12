use crate::ast::{Statement, Expression, Scope};

pub static mut TAB: usize = 2;

fn tabs(mut depth: usize) {
	if depth == 0 { return; }
	unsafe { depth *= TAB };
	print!("{:>depth$}", ' ');
}

pub fn print_statement(stmt: &Statement, depth: usize) {
	match stmt {
		Statement::Function(name, parameters, scope) => print_function(name, parameters, scope, depth),
		Statement::Expression(expr) => print_expr(expr, depth),
		Statement::Return(expr) => {
			tabs(depth);
			println!("return:");
			if let Some(expr) = expr {	print_expr(expr, depth + 1)	}
		}
		Statement::Pass => { tabs(depth); println!("pass") },
		Statement::Eof => {},
	}
}

fn print_expr(expr: &Expression, depth: usize) {
	tabs(depth);
	match expr {
		Expression::Int(int) => println!("int: {int}"),
		Expression::Float(float) => println!("float: {float}"),
		Expression::Ident(ident) => println!("ident: {ident}"),
		Expression::String(string) => println!("string: {string}"),
		Expression::BinOp(lhs, rhs, op) => {
			println!("binary:");
				
			tabs(depth + 1);
			println!("operator: {op:?}");

			print_expr(lhs, depth + 1);
			print_expr(rhs, depth + 1);
		}
	};
}

fn print_function(name: &String, parameters: &[String], scope: &Scope, depth: usize) {
	tabs(depth);
	println!("function:");
	tabs(depth + 1);
	println!("name: {}", name);
	tabs(depth + 1);
	println!("parameters:");

	parameters.iter().for_each(|p| {
		tabs(depth + 2);
		println!("{}", p);
	});
	print_scope(scope, depth + 1);
}

fn print_scope(scope: &Scope, depth: usize) {
	tabs(depth);
	println!("scope: ");
	scope.statements.iter().for_each(|s| {
		print_statement(s, depth + 1)
	})
}