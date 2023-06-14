use schlange::parser::Parser;
use schlange::scope::Scope;
use schlange::ast::{ Statement, BinOp, Expression};

fn run(source: &str, statements: &[Statement]) {
	let mut parser = Parser::new(source);
	let res = parser.parse().unwrap();

	assert_eq!(res.len(), statements.len());

	res.into_iter().enumerate().for_each(|(i, s)| {
		assert_eq!(s, statements[i])
	});
}

#[test]
fn expressions() {
	run("1.0\n2309\n\"Ducks\"\nBanana", &[
		Statement::Expression(Box::new(Expression::Float(1.0))),
		Statement::Expression(Box::new(Expression::Int(2309))),
		Statement::Expression(Box::new(Expression::String("Ducks".to_string()))),
		Statement::Expression(Box::new(Expression::Ident("Banana".to_string())))
	])
}

#[test]
fn functions() {
	run("def main(duck, banana):\n\t1 + 2\n\treturn pizza\n1", &[
		Statement::Function(
			"main".to_string(),
			vec![
				"duck".to_string(),
				"banana".to_string()
			],
			Box::new(Scope {
				parent: std::ptr::null(),
				statements: vec![
					Statement::Expression(Box::new(Expression::BinOp(
						Box::new(Expression::Int(1)),
						Box::new(Expression::Int(2)),
						BinOp::Addition		
					))),
					Statement::Return(Some(Box::new(
							Expression::Ident("pizza".to_string())
					)))
				]
			})
		),
		Statement::Expression(Box::new(Expression::Int(1)))
	])
}

#[test]
fn binary_expressions() {
	run("1 + 2 * 3 - 4\n90 - 3\n17.3 / 2.0 - 3.2", &[
		Statement::Expression(Box::new(Expression::BinOp(
			Box::new(Expression::Int(1)),
			Box::new(Expression::BinOp(
				Box::new(Expression::BinOp(
					Box::new(Expression::Int(2)),
					Box::new(Expression::Int(3)),
					BinOp::Multiplication
				)),
				Box::new(Expression::Int(4)),
				BinOp::Subtraction
			)),
			BinOp::Addition
		))),
		Statement::Expression(Box::new(Expression::BinOp(
			Box::new(Expression::Int(90)),
			Box::new(Expression::Int(3)),
			BinOp::Subtraction
		))),
		Statement::Expression(Box::new(Expression::BinOp(
			Box::new(Expression::BinOp(
				Box::new(Expression::Float(17.3)),
				Box::new(Expression::Float(2.0)),
				BinOp::Divition
			)),
			Box::new(Expression::Float(3.2)),
			BinOp::Subtraction
		)))
	])
}