use crate::scope::Scope;

/// Binary operations
#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	Div,
	Pow,
	Mod
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
	/// args:
	///  - name
	///  - function params
	Function(String, Vec<String>, Box<Scope>),
	Expression(Box<Expression>),
	Return(Option<Box<Expression>>),
	Pass,

	/// really stupid fix for a stupid problem
	Eof
}

#[derive(Debug, PartialEq, Clone)]
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
