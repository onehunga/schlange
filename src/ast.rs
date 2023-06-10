/// Binary operations
#[derive(Debug)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	Div,
	Pow,
	Mod
}

#[derive(Debug)]
pub enum Statement {
	/// args:
	///  - name
	///  - function params
	Def(String, Vec<String>),
	Expression(Box<Expression>),

	/// really stupid fix for a stupid problem
	Eof
}

#[derive(Debug)]
pub enum Expression {
	Int(u64),
	Float(f64),

	/// args:
	///  - lhs
	///  - rhs
	///  - op
	BinOp(Box<Expression>, Box<Expression>, BinOp),
}

fn tabs(mut depth: usize) {
	if depth == 0 { return; }
	depth *= 4;
	print!("{:>depth$}", ' ');
}

/// TODO: write full ast printer
pub fn print_expr(expr: &Expression, depth: usize) {
	tabs(depth);
	match expr {
		Expression::Int(i) => println!("int: {i}"),
		Expression::Float(f) => println!("float: {f}"),
		Expression::BinOp(lhs, rhs, op) => {
			println!("binary:");
				
			tabs(depth + 1);
			println!("operator: {op:?}");

			print_expr(lhs, depth + 1);
			print_expr(rhs, depth + 1);
		}
	};
}
