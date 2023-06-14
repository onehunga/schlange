use crate::scope::Scope;

type Expr = Box<Expression>;

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
	Addition,
	Subtraction,
	Multiplication,
	Divition,
	Power,
	Modulo,
	Floor,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
	/// args:
	///  - name
	///  - function params
	Function(String, Vec<String>, Box<Scope>),
	Expression(Expr),
	Return(Option<Expr>),
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
pub enum Logical {
	Not,
	And,
	Or,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Bitwise {
	ShiftLeft,
	ShiftRight,
	And,
	Xor,
	Or
}

#[derive(Debug, PartialEq, Clone)]
pub enum Unary {
	Plus,
	Minus,
	Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
	Int(u64),
	Float(f64),
	Ident(String),
	String(String),
	Logical(Expr, Expr, Logical),
	BinOp(Expr, Expr, BinOp),
	Comparison(Expr, Expr, Comparison),
	Bitwise(Expr, Expr, Bitwise),
	Unary(Expr, Unary),
}
