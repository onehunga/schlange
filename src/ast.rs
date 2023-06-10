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

fn tabs(tab: usize, mut depth: usize) {
	if depth == 0 { return; }
	depth *= tab;
	print!("{:>depth$}", ' ');
}

/// TODO: write full ast printer
pub fn print_expr(expr: &Expression, tab: usize, depth: usize) {
	tabs(tab, depth);
	match expr {
		Expression::Int(i) => println!("int: {i}"),
		Expression::Float(f) => println!("float: {f}"),
		Expression::BinOp(lhs, rhs, op) => {
			println!("binary:");
				
			tabs(tab, depth + 1);
			println!("operator: {op:?}");

			print_expr(lhs, tab, depth + 1);
			print_expr(rhs, tab, depth + 1);
		}
	};
}
