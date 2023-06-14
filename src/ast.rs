use crate::scope::Scope;

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
	None,
	LogicalOr,       // or
	LogicalAnd,      // and
	LogicalNot,      // not
	Comparison,      // ==, !=, >, >=, <, <=, is, is not, in, not in
	BitwiseOr,       // |
	BitwiseXor,      // ^
	BitwiseAnd,      // &
	BitwiseShift,    // <<, >>
	Sum,             // +, -
	Prod,            // *, /, //, %
	Unary,           // +x, -x, ~x
	Exponent,        // **
	Parentheses,     // ()
}

/// Binary operations
#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	Div,
	Pow,
	Mod,
	Floor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
	/// args:
	///  - name
	///  - function params
	Function(String, Vec<String>, Box<Scope>),
	Expression(Box<Expression>),
	Comparison(Box<Expression>, Box<Comparison>, Box<Expression>),
	Return(Option<Box<Expression>>),
	Pass,

	/// really stupid fix for a stupid problem
	Eof
}

#[derive(Debug, PartialEq, Clone)]
pub enum Comparison {
	Equal,
	NotEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
	Is,
	IsNot,
	In,
	NotIn,
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
