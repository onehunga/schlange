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
	Function(String, Vec<String>, Scope),
	Expression(Box<Expression>),
	Return(Option<Box<Expression>>),
	Pass,

	/// really stupid fix for a stupid problem
	Eof
}

#[derive(Debug)]
pub struct Scope {
	pub statements: Vec<Statement>,
	pub parent: *const Scope,
}

impl Scope {
	pub fn new(parent: *const Scope) -> Self {
		Self {
			statements: vec![],
			parent,
		}
	}
}

#[derive(Debug)]
pub enum Expression {
	Int(u64),
	Float(f64),
	Ident(String),
	String(String),

	/// args:
	///  - lhs
	///  - rhs
	///  - op
	BinOp(Box<Expression>, Box<Expression>, BinOp),
}
