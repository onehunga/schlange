use schlange::lexer::Lexer;
use schlange::token::Token;

fn run(src: &str, tokens: &[Token]) {
	let mut lex = Lexer::new(src);

	for tok in tokens {
		assert_eq!(*tok, lex.next());
	}
}

#[test]
fn simple_token() {
	run("()[]+-*/.,:",&[
		Token::LParen,
		Token::RParen,
		Token::LBracket,
		Token::RBracket,
		Token::Plus,
		Token::Minus,
		Token::Asterisk,
		Token::Slash,
		Token::Dot,
		Token::Comma,
		Token::Colon,
		Token::Eof
	]);
}

#[test]
fn double_tokens() {
	run("= == ! != > >= < <=", &[
		Token::Equal,
		Token::EqualEqual,
		Token::Bang,
		Token::BangEqual,
		Token::Greater,
		Token::GreaterEqual,
		Token::Less,
		Token::LessEqual
	]);
}

#[test]
fn numbers() {
	run(&format!("69 {} 420 2.71", std::f64::consts::PI), &[
		Token::Int(69),
		Token::Float(std::f64::consts::PI),
		Token::Int(420),
		Token::Float(2.71),
	]);
}

#[test]
fn identifiers() {
	run(r#"
and def class True None
False lambda return
"#, &[
		Token::NewLine,
		Token::And,
		Token::Def,
		Token::Class,
		Token::True,
		Token::None,
		Token::NewLine,
		Token::False,
		Token::Lambda,
		Token::Return,
		Token::NewLine,
		Token::Eof
	]);
}
