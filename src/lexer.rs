use crate::token::Token;
use crate::keyword;

const SPACE4: u32 = b' ' as u32 | (b' ' as u32) << 8 | (b' ' as u32) << 16 | (b' ' as u32) << 24;

pub struct Lexer<'a> {
	source: &'a [u8],
	read: usize,
	ch: u8
}

impl<'a> Lexer<'a> {

	pub fn new(source: &'a str) -> Self {
		Self {
			source: source.as_bytes(),
			read: 0,
			ch: 0
		}
	}

	pub fn next_token(&mut self) -> Token {
		
		self.advance();

		match self.ch {
			b'(' => Token::LParen,	
			b')' => Token::RParen,
			b'[' => Token::LBracket,
			b']' => Token::RBracket,
			b'{' => Token::LSquirly,
			b'}' => Token::RSquirly,

			b'+' => { if is_alpha(self.peek()) { Token::UnaryPlus } else { Token::Plus }}
			b'-' => { if is_alpha(self.peek()) { Token::UnaryMinus } else { Token::Minus }}
			b'*' => { if self.peek() == b'*' { Token::Exponent } else { Token::Asterisk }}
			b'/' => { if self.peek() == b'/' { Token::Floor } else { Token::Slash }}
			b'%' => Token::Percent,
			b'~' => Token::UnaryNot,

			b'.' => Token::Dot,
			b',' => Token::Comma,
			b':' => Token::Colon,

			b'=' => { if self.peek() == b'=' { Token::Equal } else { Token::Assign }}
			b'!' => { if self.peek() == b'=' { Token::NotEqual } else { Token::Bang }}
			b'<' => { match self.peek() {
				b'<' => Token::ShiftLeft,
				b'=' => Token::LessEqual,
				_ => Token::Less,
			}}
			b'>' => { match self.peek() {
				b'>' => Token::ShiftLeft,
				b'=' => Token::GreaterEqual,
				_ => Token::Greater,
			}}

			b'&' => Token::Ampersand,
			b'^' => Token::Caret,
			b'|' => Token::Line,

			b'\n' => Token::NewLine,
			b'\t' => Token::Indent,
			b'\r' => self.next_token(),
			b' ' => { unsafe {
				if self.source.as_ptr().add(self.read - 1).cast::<u32>().read() == SPACE4 {
					self.read += 3;
					Token::Indent
				} else { self.next_token() }
			}}

			b'#' => { while self.ch != b'\n' && self.ch != b'\0' { self.advance() } self.next_token()}
			b'\0' => { Token::Eof }

			b'"' => {
				let start = self.read;

				while self.peek() != b'"' {
					self.advance();
				}

				let ident = &self.source[start..self.read];
				self.advance();

				unsafe { Token::String(String::from_utf8_unchecked(ident.to_vec())) }
			}

			b'0'..=b'9' => {
				let start = self.read - 1;

				while is_digit(self.peek()) { self.advance() }

				if self.peek() == b'.' {
					self.advance();
					while is_digit(self.peek()) { self.advance() }

					let ident = &self.source[start..self.read];
					let str = String::from_utf8_lossy(ident);
					match str.parse::<f64>() {
						Ok(float) => return Token::Float(float),
						Err(_) => panic!("Failed to parse float"),
					}
				}

				let ident = &self.source[start..self.read];
				let str = String::from_utf8_lossy(ident);
				match str.parse::<u64>() {
					Ok(int) => Token::Int(int),
					Err(_) => panic!("Failed to parse int")
				}				
			}

			b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
				let start = self.read - 1;

				while is_alpha(self.peek()) || is_digit(self.peek()) { self.advance(); }

				let ident = &self.source[start..self.read];

				unsafe { keyword::lookup(ident).unwrap_or(
					Token::Ident(String::from_utf8_unchecked(ident.to_vec()))
				)}
			}

			_ => Token::Illegal,
		}
	}

	fn advance(&mut self) {
		if self.read >= self.source.len() {
			self.ch = 0;
			return;
		}
		self.ch = self.source[self.read];
		self.read += 1;
	}

	fn peek(&self) -> u8 {
		if self.read >= self.source.len() {
			return 0;
		}
		self.source[self.read]
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

impl Iterator for Lexer<'_> {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		let token = self.next_token();
		match token {
			Token::Eof => None,
			_ => Some(token),
		}
	}
}
