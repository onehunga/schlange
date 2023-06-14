use schlange::lexer::Lexer;
use schlange::token::Token;

fn run(src: &str, tokens: &[Token]) {
	let lexer = Lexer::new(src);

	lexer.enumerate().for_each(|(i, t)| {
		assert_eq!(t, tokens[i])
	})
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
		Token::Assign,
		Token::Equal,
		Token::Bang,
		Token::NotEqual,
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
