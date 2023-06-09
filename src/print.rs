use crate::{ast::{Statement, Expression}, scope::Scope};

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
			println!("Return:");
			if let Some(expr) = expr {	print_expr(expr, depth + 1)	}
		}
		Statement::Pass => { tabs(depth); println!("pass") },
		Statement::Eof => {},
	}
}

fn print_expr(expr: &Expression, depth: usize) {
	tabs(depth);
	match expr {
		Expression::Int(int) => println!("Int: {int}"),
		Expression::Float(float) => println!("Float: {float}"),
		Expression::Ident(ident) => println!("Ident: {ident}"),
		Expression::String(string) => println!("String: {string}"),
		Expression::Unary(lhs, uny) => {
			println!("Unary:");
			tabs(depth + 1);
			println!("kind: {uny:?}");
			print_expr(lhs, depth + 1);
		}
		Expression::Bitwise(lhs, rhs, btw) => {
			println!("Bitwise:");
			tabs(depth + 1);
			println!("kind: {btw:?}");
			print_expr(lhs, depth + 1);
			print_expr(rhs, depth + 1)
		}
		Expression::Comparison(lhs, rhs, cmp) => {
			println!("Comparison:");
			tabs(depth + 1);
			println!("kind: {cmp:?}");
			print_expr(lhs, depth + 1);
			print_expr(rhs, depth + 1)
		},
		Expression::BinOp(lhs, rhs, op) => {
			println!("Binary:");
			tabs(depth + 1);
			println!("operator: {op:?}");
			print_expr(lhs, depth + 1);
			print_expr(rhs, depth + 1);
		}
	};
}

fn print_function(name: &String, parameters: &[String], scope: &Scope, depth: usize) {
	tabs(depth);
	println!("Function:");
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