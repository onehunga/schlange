#[derive(Debug, PartialEq, Clone)]
pub enum Token {
	LParen,          // (
	RParen,          // )
	LBracket,        // [
	RBracket,        // ]
	LSquirly,        // {
	RSquirly,        // }

	Plus,            // +
	Minus,           // -
	Asterisk,        // *
	Exponent,        // **
	Slash,           // /
	Floor,           // //
	Percent,         // %

	UnaryPlus,       // +...
	UnaryMinus,      // -...
	UnaryNot,        // ~...

	Dot,             // . 
	Comma,           // ,
	Colon,           // :

	Assign,          // =
	Equal,           // ==
	Bang,            // !
	NotEqual,        // !=
	Greater,         // >
	GreaterEqual,    // >=
	Less,            // <
	LessEqual,       // <=

	ShiftLeft,       // <<
	ShiftRight,      // >>

	Ampersand,       // &
	Caret,           // ^
	Line,            // |


	// Values
	String(String),  // "..."
	Ident(String),   // ...
	Int(u64),        // 1
	Float(f64),      // 1.0

	// keywords
	False,
	None,
	True,
	And,
	As,
	Assert,
	Async,
	Await,
	Break,
	Class,
	Continue,
	Def,
	Del,
	Elif,
	Else,
	Except,
	Finally,
	For,
	From,
	Global,
	If,
	Import,
	In,
	Is,
	IsNot,
	Lambda,
	Nonlocal,
	Not,
	NotIn,
	Or,
	Pass,
	Raise,
	Return,
	Try,
	While,
	With,
	Yield,

	// Indentation
	Indent,          // \t or '    '
	NewLine,         // \n

	Illegal,
	Eof,
}
