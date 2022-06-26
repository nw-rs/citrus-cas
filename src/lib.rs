#![no_std]

use heapless::LinearMap;

mod expression;
mod parser;
mod token;

pub use parser::approx as parse_approximation;
pub use parser::math_expr as parse_math_expression;
pub use expression::{Expression, Approx};

/// A map of characters associated with
/// the expressions that they should evaluate to.
pub struct ValueMap<const E: usize, const N: usize> {
    pub map: LinearMap<char, Expression<E>, N>,
}

impl<const E: usize, const N: usize> ValueMap<E, N> {
    pub fn new() -> Self {
        ValueMap {
            map: LinearMap::new(),
        }
    }

    pub fn insert(&mut self, var: char, expr: Expression<E>) {
        self.map
            .insert(var, expr)
            .expect("Could not add variable to map.");
    }

    pub fn get(&self, var: char) -> Option<&Expression<E>> {
        self.map.get(&var)
    }

    pub fn get_mut(&mut self, var: char) -> Option<&mut Expression<E>> {
        self.map.get_mut(&var)
    }

    pub fn remove(&mut self, var: char) -> Option<Expression<E>> {
        self.map.remove(&var)
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }
}
