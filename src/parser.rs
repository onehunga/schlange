use std::mem::swap;

use crate::{token::Token, lexer::Lexer, ast::{Expression, BinOp, Statement, Scope}};

#[derive(Debug)]
pub enum ParseError {
	Expression,
	InvalidOperator,
	InvalidExpressionKind(Token),
	InvalidPrefixExpression(Token, Token),
	UnexpectedToken { found: Token, expected: Vec<Token> },
	ExpectedIndentation(Token),
	IllegalToken,
}

pub type ParserResult<T> = Result<T, ParseError>;
pub type Stmt = ParserResult<Statement>;
pub type Expr = ParserResult<Box<Expression>>;
pub type Ast = ParserResult<Box<Scope>>;

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
	main_scope: Box<Scope>,
	current_scope: *const Scope,
	depth: usize,
}

impl<'a> Parser<'a> {

	pub fn new(source: &'a str) -> Self {
		let mut lexer = Lexer::new(source);
		let current = lexer.next();
		let peek = lexer.next();
		let main_scope = Box::new(Scope {
			parent: std::ptr::null(),
			statements: vec![],
		});
		let current_scope = Box::into_raw(main_scope.clone());

		Self {
			lexer,
			current,
			peek,
			current_scope,
			main_scope,
			depth: 0,
		}
	}

	pub fn parse(&mut self) -> Ast {
		while self.current != Token::Eof {
			let stmt = self.statement()?;
			self.main_scope.statements.push(stmt);
			self.advance();
		}

		Ok(self.main_scope.clone())
	}

	fn statement(&mut self) -> Stmt {
		match self.current {
			Token::NewLine => {
				self.advance();
				self.statement()
			},
			Token::Eof => Ok(Statement::Eof),
			Token::Illegal => Err(ParseError::IllegalToken),
			Token::Def => self.function(),
			Token::Pass => Ok(Statement::Pass),
			Token::Return => {
				let expr = match self.peek {
					Token::Eof | Token::NewLine => None,
					_ => {
						self.advance();
						Some(self.expression(Precedence::None)?)
					}
				};
				Ok(Statement::Return(expr))
			},
			_ => Ok(Statement::Expression(self.expression(Precedence::None)?))
		}
	}

	fn function(&mut self) -> Stmt {
		self.advance();
		let name = match self.current.clone() {
			Token::Ident(name) => name,
			_ => return Err(ParseError::UnexpectedToken {
				found: self.current.clone(),
				expected: vec![Token::Ident("...".to_string())] }
			),
		};
		self.advance(); 

		match self.current {
			Token::LParen => { self.advance() }
			_ => return Err(ParseError::UnexpectedToken {
				found: self.current.clone(),
				expected: vec![Token::LParen]}
			)
		}

		let mut parameters = vec![];

		loop {
			match self.current.clone() {
				Token::RParen => { self.advance(); break},
				Token::NewLine | Token::Eof => return Err(ParseError::UnexpectedToken {
					found: self.current.clone(),
					expected: vec![Token::RParen]}
				),
				Token::Ident(ident) => parameters.push(ident.clone()),
				Token::Comma => {
					match &self.peek {
						Token::Ident(_) => {},
						_ => return Err(ParseError::UnexpectedToken {
							found: self.peek.clone(),
							expected: vec![Token::Ident("...".to_string())],
						})
					}
				},
				Token::Illegal => return Err(ParseError::IllegalToken),
				_ => return Err(ParseError::UnexpectedToken {
					found: self.current.clone(),
					expected: vec![Token::Ident("...".to_string()), Token::Comma, Token::RParen]}
				)
			}

			self.advance();
		}

		match self.current {
			Token::Colon => Ok(Statement::Function(name, parameters, self.scope()?)),
			_ => Err(ParseError::UnexpectedToken {
				found: self.current.clone(),
				expected: vec![Token::Colon]
			})
		}
	}

	fn depth(&mut self) -> usize {
		let mut depth = 0;

		loop {
			match self.current {
				Token::NewLine => depth = 0,
				Token::Indent => depth += 1,
				_ => break,
			}

			self.advance();
		}
		depth
	}

	fn scope(&mut self) -> Ast {
		self.advance(); // skip the colon
		let depth = self.depth();

		if self.depth >= depth {
			return Err(ParseError::ExpectedIndentation(self.current.clone()))
		}

		let old_depth = self.depth;
		self.depth = depth;
		
		let mut scope = Box::new(Scope::new(self.current_scope));
		self.current_scope = Box::into_raw(scope.clone());

		loop {
			let stmt = self.statement()?;
			scope.statements.push(stmt);
			self.advance();
			if self.depth() != depth { break; }
		}

		// Restore
		self.current_scope = Box::into_raw(scope.clone());
		self.depth = old_depth;
		Ok(scope)
	}

	fn expression(&mut self, prec: Precedence) -> Expr {
		let mut left = self.prefix()?;

		while prec <= self.peek_prec() && self.peek != Token::NewLine && self.peek != Token::Eof {
			self.advance();
			left = self.infix(left)?;
		}

		Ok(left)
	}

	fn prefix(&mut self) -> Expr {
		match &self.current {
			Token::Int(int) => Ok(Box::new(Expression::Int(*int))),
			Token::Float(float) => Ok(Box::new(Expression::Float(*float))),
			Token::Ident(ident) => Ok(Box::new(Expression::Ident(ident.clone()))),
			Token::String(string) => Ok(Box::new(Expression::String(string.clone()))),
			_ => Err(ParseError::InvalidPrefixExpression(self.current.clone(), self.peek.clone()))
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
