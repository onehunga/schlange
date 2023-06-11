use crate::token::Token;

const SPACE4: u32 = b' ' as u32 | (b' ' as u32) << 8 | (b' ' as u32) << 16 | (b' ' as u32) << 24;

struct Keyword {
	pub name: &'static [u8],
	pub tok: Token
}

const KEYWORDS: [Keyword; 33] = [
	Keyword {
		name: "and".as_bytes(),
		tok: Token::And
	},
	Keyword {
		name: "as".as_bytes(),
		tok: Token::As
	},
	Keyword {
		name: "assert".as_bytes(),
		tok: Token::Assert
	},
	Keyword {
		name: "break".as_bytes(),
		tok: Token::Break
	},
	Keyword {
		name: "class".as_bytes(),
		tok: Token::Class,
	},
	Keyword {
		name: "continue".as_bytes(),
		tok: Token::Continue,
	},
	Keyword {
		name: "def".as_bytes(),
		tok: Token::Def
	},
	Keyword {
		name: "del".as_bytes(),
		tok: Token::Del,
	},
	Keyword {
		name: "elif".as_bytes(),
		tok: Token::Elif,
	},
	Keyword {
		name: "else".as_bytes(),
		tok: Token::Else
	},
	Keyword {
		name: "except".as_bytes(),
		tok: Token::Except
	},
	Keyword {
		name: "False".as_bytes(),
		tok: Token::False
	},
	Keyword {
		name: "finally".as_bytes(),
		tok: Token::Finally
	},
	Keyword {
		name: "for".as_bytes(),
		tok: Token::For
	},
	Keyword {
		name: "from".as_bytes(),
		tok: Token::From
	},
	Keyword {
		name: "global".as_bytes(),
		tok: Token::Global
	},
	Keyword {
		name: "if".as_bytes(),
		tok: Token::If
	},
	Keyword {
		name: "import".as_bytes(),
		tok: Token::Import
	},
	Keyword {
		name: "in".as_bytes(),
		tok: Token::In
	},
	Keyword {
		name: "is".as_bytes(),
		tok: Token::Is
	},
	Keyword {
		name: "lambda".as_bytes(),
		tok: Token::Lambda
	},
	Keyword {
		name: "None".as_bytes(),
		tok: Token::None
	},
	Keyword {
		name: "nonlocal".as_bytes(),
		tok: Token::Nonlocal
	},
	Keyword {
		name: "not".as_bytes(),
		tok: Token::Not
	},
	Keyword {
		name: "or".as_bytes(),
		tok: Token::Or
	},
	Keyword {
		name: "pass".as_bytes(),
		tok: Token::Pass
	},
	Keyword {
		name: "raise".as_bytes(),
		tok: Token::Raise
	},
	Keyword {
		name: "return".as_bytes(),
		tok: Token::Return
	},
	Keyword {
		name: "True".as_bytes(),
		tok: Token::True
	},
	Keyword {
		name: "try".as_bytes(),
		tok: Token::Try
	},
	Keyword {
		name: "while".as_bytes(),
		tok: Token::While
	},
	Keyword {
		name: "with".as_bytes(),
		tok: Token::With
	},
	Keyword {
		name: "yield".as_bytes(),
		tok: Token::Yield
	},
];

pub struct Lexer<'a> {
	code: &'a [u8],
	read: usize,
	ch: u8
}

impl<'a> Lexer<'a> {

	pub fn new(code: &'a str) -> Self {
		Self {
			code: code.as_bytes(),
			read: 0,
			ch: 0
		}
	}

	pub fn next(&mut self) -> Token {
		self.advance();

		match self.ch {
			b'(' => Token::LParen,
			b')' => Token::RParen,
			b'[' => Token::LBracket,
			b']' => Token::RBracket,

			b'+' => Token::Plus,
			b'-' => Token::Minus,
			b'*' => Token::Asterisk,
			b'/' => Token::Slash,

			b'=' => self.double_tok('=', Token::Equal, Token::EqualEqual),
			b'!' => self.double_tok('=', Token::Bang, Token::BangEqual),
			b'>' => self.double_tok('=', Token::Greater, Token::GreaterEqual),
			b'<' => self.double_tok('=', Token::Less, Token::LessEqual),

			b'.' => Token::Dot,
			b',' => Token::Comma,
			b':' => Token::Colon,

			b'\n' => Token::NewLine,
			b' ' => {
				unsafe {
					if self.code.as_ptr().add(self.read - 1).cast::<u32>().read() == SPACE4 {
						return Token::Indent;
					}
				}
				self.next()
			},
			b'\t' => Token::Indent,
			b'\r' => self.next(),

			b'"' => {
				let start = self.read;
				self.advance();

				while self.peek() != b'"' {
					self.advance();
				}

				let ident = &self.code[start..self.read];
				self.advance();

				unsafe {
					Token::String(String::from_utf8_unchecked(ident.to_vec()))
				}
			}

			0 => Token::Eof,

			_ => if is_digit(self.ch) {
				let start = self.read - 1;

				while is_digit(self.peek()) {
					self.advance();
				}
				if self.peek() == b'.' {
					self.advance();
					while is_digit(self.peek()) {
						self.advance();
					}

					let ident = &self.code[start..self.read];
					let str = String::from_utf8_lossy(ident);
					return match str.parse::<f64>() {
						Ok(f) => Token::Float(f),
						Err(e) => Token::Error(e.to_string())
					}
				}

				let ident = &self.code[start..self.read];
				let str = String::from_utf8_lossy(ident);
				match str.parse::<u64>() {
					Ok(i) => Token::Int(i),
					Err(e) => Token::Error(e.to_string())
				}
			}
			else if is_alpha(self.ch) {
				let start = self.read - 1;

				while is_alpha(self.peek()) || is_digit(self.peek()) {
					self.advance();
				}

				let ident = &self.code[start..self.read];
				
				for kw in KEYWORDS {
					if kw.name == ident {
						return kw.tok
					}
				}

				unsafe {
					Token::Ident(String::from_utf8_unchecked(ident.to_vec()))
				}
			}
			else {
				Token::Illegal
			}
		}
	}

	#[inline(always)]
	fn double_tok(&mut self, cond: char, single: Token, double: Token) -> Token {
		if self.peek() == cond as u8 {
			self.advance();
			return double;
		}
		single
	}

	fn advance(&mut self) {
		if self.read >= self.code.len() {
			self.ch = 0;
			return;
		}
		self.ch = self.code[self.read];
		self.read += 1;
	}

	fn peek(&self) -> u8 {
		if self.read >= self.code.len() {
			return 0;
		}
		self.code[self.read]
	}
}

#[inline(always)]
fn is_alpha(ch: u8) -> bool {
	let ch = ch as char;
	ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_'
}

#[inline(always)]
fn is_digit(ch: u8) -> bool {
	let ch = ch as char;
	ch.is_ascii_digit()
}
