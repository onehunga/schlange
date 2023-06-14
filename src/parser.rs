use std::mem::swap;

use crate::{token::Token, lexer::Lexer, ast::{Expression, BinOp, Statement, Precedence, Comparison, Logical}, scope::Scope};

#[derive(Debug)]
pub enum ParseError {
	Expression,
	InvalidOperator(Token),
	InvalidComparison(Token),
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
		let current = lexer.next_token();
		let peek = lexer.next_token();
		let main_scope = Box::<Scope>::default();
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
			self.main_scope.push(stmt);
		}

		Ok(self.main_scope.clone())
	}

	fn statement(&mut self) -> Stmt {
		match self.current {
			Token::NewLine | Token::Indent => {
				self.depth = self.depth();
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
			_ => { let expr = self.expression(Precedence::None)?; self.advance(); Ok(Statement::Expression(expr)) }
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
			Token::Colon => { self.advance(); Ok(Statement::Function(name, parameters, self.scope()?)) },
			_ => Err(ParseError::UnexpectedToken {
				found: self.current.clone(),
				expected: vec![Token::Colon]
			})
		}
	}

	fn scope(&mut self) -> Ast {
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
			scope.push(stmt);
			if self.depth() < depth { break; }
		}

		self.advance();

		// Restore
		self.current_scope = Box::into_raw(scope.clone());
		self.depth = old_depth;
		Ok(scope)
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
			Token::Plus | Token::Minus | Token::Asterisk | Token::Percent |
			Token::Slash | Token::Exponent | Token::Floor => self.binary(lhs),
			Token::Equal | Token::NotEqual | Token::Greater | Token::GreaterEqual |
			Token::Less | Token::LessEqual | Token::Is | Token::IsNot | Token::In | 
			Token::NotIn => self.comparison(lhs),
			Token::Not | Token::And | Token::Or => self.logical(lhs),
			_ => Err(ParseError::InvalidExpressionKind(self.current.clone()))
		}
	}

	fn logical(&mut self, lhs: Box<Expression>) -> Expr {
		let prec = self.current_prec();
		let log = self.get_log()?;
		self.advance();

		let rhs = self.expression(prec)?;
		Ok(Box::new(Expression::Logical(lhs, rhs, log)))
	}

	fn comparison(&mut self, lhs: Box<Expression>) -> Expr {
		let prec = self.current_prec();
		let cmp = self.get_cmp()?;
		self.advance();

		let rhs = self.expression(prec)?;
		Ok(Box::new(Expression::Comparison(lhs, rhs, cmp)))
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
		self.peek = self.lexer.next_token();
	}

	fn current_prec(&mut self) -> Precedence {
		match self.current {
			Token::Exponent => Precedence::Exponent,
			Token::UnaryPlus | Token::UnaryMinus | Token::UnaryNot => Precedence::Unary,
			Token::Asterisk | Token::Slash | Token::Floor | Token::Percent => Precedence::Prod,
			Token::Plus | Token::Minus => Precedence::Sum,
			Token::ShiftLeft | Token::ShiftRight => Precedence::BitwiseShift,
			Token::Ampersand => Precedence::BitwiseAnd,
			Token::Caret => Precedence::BitwiseXor,
			Token::Line => Precedence::BitwiseOr,
			Token::Equal | Token::NotEqual | Token::Greater | Token::GreaterEqual | 
			Token::Less | Token::LessEqual | Token::Is | Token::In |
			Token::IsNot | Token::NotIn => Precedence::Comparison, 
			Token::Not => Precedence::LogicalNot,
			Token::And => Precedence::LogicalAnd,
			Token::Or => Precedence::LogicalOr,
			
			_ => Precedence::None
		}
	}

	fn peek_prec(&mut self) -> Precedence {
		match self.peek {
			Token::Exponent => Precedence::Exponent,
			Token::UnaryPlus | Token::UnaryMinus | Token::UnaryNot => Precedence::Unary,
			Token::Asterisk | Token::Slash | Token::Floor | Token::Percent => Precedence::Prod,
			Token::Plus | Token::Minus => Precedence::Sum,
			Token::ShiftLeft | Token::ShiftRight => Precedence::BitwiseShift,
			Token::Ampersand => Precedence::BitwiseAnd,
			Token::Caret => Precedence::BitwiseXor,
			Token::Line => Precedence::BitwiseOr,
			Token::Equal | Token::NotEqual | Token::Greater | Token::GreaterEqual | 
			Token::Less | Token::LessEqual | Token::Is | Token::In |
			Token::IsNot | Token::NotIn => Precedence::Comparison, 
			Token::Not => Precedence::LogicalNot,
			Token::And => Precedence::LogicalAnd,
			Token::Or => Precedence::LogicalOr,
			
			_ => Precedence::None
		}
	}

	fn get_op(&mut self) -> ParserResult<BinOp> {
		Ok(match self.current {
			Token::Plus => BinOp::Add,
			Token::Minus => BinOp::Sub,
			Token::Asterisk => BinOp::Mul,
			Token::Slash => BinOp::Div,
			Token::Floor => BinOp::Floor,
			Token::Exponent => BinOp::Pow,
			Token::Percent => BinOp::Mod,
			_ => return Err(ParseError::UnexpectedToken {
				found: self.current.clone(),
				expected: vec![
					Token::Plus, Token::Minus, Token::Asterisk,
					Token::Slash, Token::Floor, Token::Exponent, Token::Percent
				]
			})
		})
	}

	fn get_cmp(&mut self) -> ParserResult<Comparison> {
		Ok(match self.current {
			Token::Equal => Comparison::Equal,
			Token::NotEqual => Comparison::NotEqual,
			Token::Greater => Comparison::Greater,
			Token::GreaterEqual => Comparison::GreaterEqual,
			Token::Less => Comparison::Less,
			Token::LessEqual => Comparison::LessEqual,
			Token::Is => Comparison::Is,
			Token::IsNot => Comparison::IsNot,
			Token::In => Comparison::In,
			Token::NotIn => Comparison::NotIn,
			_ => return Err(ParseError::UnexpectedToken {
				found: self.current.clone(),
				expected: vec![
					Token::Equal, Token::NotEqual, Token::Greater,
					Token::GreaterEqual, Token::Less, Token::LessEqual,
					Token::Is, Token::IsNot, Token::In, Token::NotIn
				]
			})
		})
	}

	fn get_log(&mut self) -> ParserResult<Logical> {
		Ok(match self.current {
			Token::Not => Logical::Not,
			Token::And => Logical::And,
			Token::Or => Logical::Or,
			_ => return Err(ParseError::UnexpectedToken {
				found: self.current.clone(),
				expected: vec![Token::Not, Token::And, Token::Or]
			})
		})
	}
}
