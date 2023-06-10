use std::mem::swap;

use crate::{token::Token, lexer::Lexer, ast::{Expression, BinOp, Statement}};

#[derive(Debug)]
pub enum ParseError {
	Expression,
	InvalidOperator,
	InvalidExpressionKind(Token),
	InvalidPrefixExpression(Token),
	IllegalToken,
}

pub type ParserResult<T> = Result<T, ParseError>;
pub type Stmt = ParserResult<Statement>;
pub type Expr = ParserResult<Box<Expression>>;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
	None,
	Sum,
	Prod,
}

pub struct Parser<'a> {
	lexer: Lexer<'a>,
	current: Token,
	peek: Token,
}

impl<'a> Parser<'a> {

	pub fn new(source: &'a str) -> Self {
		let mut lexer = Lexer::new(source);
		let current = lexer.next();
		let peek = lexer.next();

		Self {
			lexer,
			current,
			peek
		}
	}

	pub fn parse(&mut self) -> ParserResult<Vec<Statement>> {
		let mut out = Vec::new();

		while self.current != Token::Eof {
			out.push(self.statement()?);
			self.advance();
		}

		Ok(out)
	}

	fn statement(&mut self) -> Stmt {
		match self.current {
			Token::NewLine => {
				self.advance();
				self.statement()
			}
			Token::Eof => Ok(Statement::Eof),
			Token::Illegal => Err(ParseError::IllegalToken),
			_ => Ok(Statement::Expression(self.expression(Precedence::None)?))
		}
	}

	fn expression(&mut self, prec: Precedence) -> Expr {
		let mut left = self.prefix()?;

		while prec <= self.peek_prec() && self.peek != Token::NewLine {
			self.advance();
			left = self.infix(left)?;
		}

		Ok(left)
	}

	fn prefix(&mut self) -> Expr {
		match self.current {
			Token::Int(i) => Ok(Box::new(Expression::Int(i))),
			Token::Float(f) => Ok(Box::new(Expression::Float(f))),
			_ => Err(ParseError::InvalidPrefixExpression(self.current.clone()))
		}
	}

	fn infix(&mut self, lhs: Box<Expression>) -> Expr {
		match self.current {
			Token::Plus |
			Token::Minus |
			Token::Asterisk |
			Token::Slash => self.binary(lhs),
			_ => Err(ParseError::InvalidExpressionKind(self.current.clone()))
		}
	}

	fn binary(&mut self, lhs: Box<Expression>) -> Expr {
		let prec = self.current_prec();
		let op = self.get_op()?;
		self.advance();

		let rhs = self.expression(prec)?;

		Ok(Box::new(Expression::BinOp(lhs, rhs, op)))
	}

	fn advance(&mut self) {
		swap(&mut self.current, &mut self.peek);
		self.peek = self.lexer.next();
	}

	fn current_prec(&mut self) -> Precedence {
		match self.current {
			Token::Plus |
			Token::Minus => Precedence::Sum,
			Token::Asterisk |
			Token::Slash => Precedence::Prod,
			_ => Precedence::None
		}
	}

	fn peek_prec(&mut self) -> Precedence {
		match self.peek {
			Token::Plus |
			Token::Minus => Precedence::Sum,
			Token::Asterisk |
			Token::Slash => Precedence::Prod,
			_ => Precedence::None
		}
	}

	fn get_op(&mut self) -> ParserResult<BinOp> {
		match self.current {
			Token::Plus => Ok(BinOp::Add),
			Token::Minus => Ok(BinOp::Sub),
			Token::Asterisk => Ok(BinOp::Mul),
			Token::Slash => Ok(BinOp::Div),
			_ => Err(ParseError::InvalidOperator)
		}
	}
}
