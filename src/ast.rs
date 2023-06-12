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

#[derive(Debug, Clone)]
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

impl PartialEq for Statement {
	fn eq(&self, other: &Statement) -> bool {
		match (self, other) {
			(Statement::Function(sname, sparams, sscope), Statement::Function(oname, oparams, oscope)) => { 
				sname == oname && sparams == oparams && sscope.statements == oscope.statements
			},
			(_, _) => self == other
		}
	}	
}

#[derive(Debug, PartialEq, Clone)]
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
