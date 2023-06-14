use crate::{
	token::Token,
	lexer::Lexer,
	ast::{
		Bitwise,
		Expression,
		BinOp,
		Statement,
		Comparison,
		Unary
	},
	scope::Scope
};

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
	None,
	LogicalOr,       // or
	LogicalAnd,      // and
	LogicalNot,      // not
	Comparison,      // ==, !=, >, >=, <, <=, is, is not, in, not in
	BitwiseOr,       // |
	BitwiseXor,      // ^
	BitwiseAnd,      // &
	BitwiseShift,    // <<, >>
	Sum,             // +, -
	Prod,            // *, /, //, %
	Unary,           // +x, -x, ~x
	Exponent,        // **
	Parentheses,     // ()
}

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
				self.advance();
				let expr = match self.current {
					Token::Eof | Token::NewLine => None,
					_ => {
						let expr = self.expression(Precedence::None)?;
						self.advance();
						Some(expr)
					}
				};
				Ok(Statement::Return(expr))
			},
			_ => { let expr = self.expression(Precedence::None)?; self.advance(); Ok(Statement::Expression(expr)) }
		}
	}

	fn function(&mut self) -> Stmt {
		let name = match self.peek.clone() {
			Token::Ident(name) => name,
			_ => return Err(ParseError::UnexpectedToken {
				found: self.current.clone(),
				expected: vec![Token::Ident("...".to_string())] }
			),
		};
		self.advance(); 

		match self.peek {
			Token::LParen => { self.advance(); self.advance() }
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

		while prec <= self.prec(&self.peek) && self.peek != Token::NewLine && self.peek != Token::Eof {
			self.advance();
			left = self.infix(left)?;
		}

		Ok(left)
	}

	fn prefix(&mut self) -> Expr {
		Ok(match &self.current {
			Token::Int(int) => Box::new(Expression::Int(*int)),
			Token::Float(float) => Box::new(Expression::Float(*float)),
			Token::Ident(ident) => Box::new(Expression::Ident(ident.clone())),
			Token::String(string) => Box::new(Expression::String(string.clone())),
			Token::LParen => {
				self.advance();
				let expr = self.expression(Precedence::None)?;
				println!("hi");
				if self.peek != Token::RParen { return Err(ParseError::UnexpectedToken {
					found: self.peek.clone(),
					expected: vec![Token::RParen]
				})} else { expr }
			}
			Token::UnaryMinus => {
				self.advance();
				Box::new(Expression::Unary(self.prefix()?, Unary::Minus))
			},
			Token::UnaryPlus => {
				self.advance();
				Box::new(Expression::Unary(self.prefix()?, Unary::Plus))
			},
			Token::UnaryNot => {
				self.advance();
				Box::new(Expression::Unary(self.prefix()?, Unary::Not))
			},
			_ => return Err(ParseError::InvalidPrefixExpression(
				self.current.clone(),
				self.peek.clone())
			)
		})
	}

	fn infix(&mut self, lhs: Box<Expression>) -> Expr {
		let prec = self.prec(&self.current);
		let current = self.current.clone();
		self.advance();
		let rhs = self.expression(prec)?;

		get_infix!(lhs, rhs, current, BinOp,
			(Plus, Addition),
			(Minus, Subtraction),
			(Asterisk, Multiplication),
			(Slash, Divition),
			(Floor, Floor),
			(Exponent, Power),
			(Percent, Modulo)
		);

		get_infix!(lhs, rhs, current, Comparison, 
			Equal, NotEqual, Greater, GreaterEqual,	Less,
			LessEqual, Is, IsNot, In, NotIn, Not, And, Or
		);
		
		get_infix!(lhs, rhs, current, Bitwise,
			(ShiftLeft, ShiftLeft),
			(ShiftRight, ShiftRight),
			(Ampersand, And),
			(Caret, Xor),
			(Line, Or)
		);
		
		Err(ParseError::InvalidExpressionKind(current))
	}

	fn advance(&mut self) {
		std::mem::swap(&mut self.current, &mut self.peek);
		self.peek = self.lexer.next_token();
	}

	fn prec(&self, token: &Token) -> Precedence {
		match token {
			Token::LParen | Token::RParen => Precedence::Parentheses,
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
}

macro_rules! get_infix {
	( $lhs:ident , $rhs:ident, $found:ident, $expr_kind:ident, $(($token:ident , $kind:ident)), *) => {
		{$(if $found == Token::$token {
			return Ok(Box::new(Expression::$expr_kind(
				$lhs, $rhs, $expr_kind::$kind
			)));
		})*}
	};
	
	( $lhs:ident, $rhs:ident, $found:ident, $expr_kind:ident, $($token:ident), *) => {
		{$(if $found == Token::$token {
			return Ok(Box::new(Expression::$expr_kind(
				$lhs, $rhs, $expr_kind::$token
			)));
		})*}
	}
}

pub(crate) use get_infix;
