#[derive(Debug, PartialEq)]
pub enum Token {
	LParen,
	RParen,
	LBracket,
	RBracket,

	Plus,
	Minus,
	Asterisk,
	Slash,

	Dot,
	Comma,
	Colon,

	Equal,
	EqualEqual,
	Bang,
	BangEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,

	String(String),
	Ident(String),
	Int(u64),
	Float(f64),

	// keywords
	And,
	As,
	Assert,
	Break,
	Class,
	Continue,
	Def,
	Del,
	Elif,
	Else,
	Except,
	False,
	Finally,
	For,
	From,
	Global,
	If,
	Import,
	In,
	Is,
	Lambda,
	None,
	Nonlocal,
	Not,
	Or,
	Pass,
	Raise,
	Return,
	True,
	Try,
	While,
	With,
	Yield,

	Indent,
	NewLine,

	Illegal,
	Error(String),
	Eof
}
