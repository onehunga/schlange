use crate::ast::Statement;
use std::ops::{Index, IndexMut};

#[derive(Debug, Clone)]
pub struct Scope {
	pub statements: Vec<Statement>,
	pub parent: *const Scope,
}

impl Scope {
	pub fn new(parent: *const Scope) -> Self {
		Self {
			statements: vec![],
			parent,
		}
	}

	pub fn len(&self) -> usize {
		self.statements.len()
	}

	pub fn is_empty(&self) -> bool {
		self.statements.is_empty()
	}

	pub fn push(&mut self, stmt: Statement) {
		self.statements.push(stmt)
	}
}

impl PartialEq for Scope {
	fn eq(&self, other: &Scope) -> bool {
		self.statements == other.statements
	}
}

impl Default for Scope {
	fn default() -> Self {
		Self {
			parent: std::ptr::null(),
			statements: vec![],
		}
	}
}

impl Index<usize> for Scope {
	type Output = Statement;
	
	fn index(&self, idx: usize) -> &Self::Output {
		&self.statements[idx]
	}
}

impl IndexMut<usize> for Scope {
	fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
		&mut self.statements[idx]
	}
}

impl IntoIterator for Scope {
	type Item = Statement;
	type IntoIter = std::vec::IntoIter<Self::Item>;
	
	fn into_iter(self) -> Self::IntoIter {
		self.statements.into_iter()
	}
}
