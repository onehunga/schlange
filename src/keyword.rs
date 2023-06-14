use crate::token::Token;

pub fn lookup(ident: &[u8]) -> Option<Token> {
	for (word, token) in KEYWORDS {
		if word == ident { return Some(token); }
	}

	None
}

const KEYWORDS: [(&[u8], Token); 37] = [
	(b"False", Token::False),
	(b"None", Token::None),
	(b"True", Token::True),
	(b"and", Token::And),
	(b"as", Token::As),
	(b"assert", Token::Assert),
	(b"async", Token::Async),
	(b"await", Token::Await),
	(b"break", Token::Break),
	(b"class", Token::Class),
	(b"continue", Token::Continue),
	(b"def", Token::Def),
	(b"del", Token::Del),
	(b"elif", Token::Elif),
	(b"else", Token::Else),
	(b"except", Token::Except),
	(b"finally", Token::Finally),
	(b"for", Token::For),
	(b"from", Token::From),
	(b"global", Token::Global),
	(b"if", Token::If),
	(b"import", Token::Import),
	(b"in", Token::In),
	(b"is", Token::Is),
	(b"is not", Token::IsNot),
	(b"lambda", Token::Lambda),
	(b"nonlocal", Token::Nonlocal),
	(b"not", Token::Not),
	(b"not in", Token::NotIn),
	(b"or", Token::Or),
	(b"pass", Token::Pass),
	(b"raise", Token::Raise),
	(b"return", Token::Return),
	(b"try", Token::Try),
	(b"while", Token::While),
	(b"with", Token::With),
	(b"yield", Token::Yield)
];