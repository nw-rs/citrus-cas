use heapless::{Vec, LinearMap,};

use crate::{token::Token, Expression, Error, Approx};

/// parses tokens which need modifiable definition
pub trait ExpressionMap<const E: usize> {
    fn stack_contains(&self, stack: &Expression<E>) -> bool;
    fn evaluate(&self, stack: &Expression<E>) -> Expression<E>;
    fn approximate(&self, stack: &Expression<E>) -> Expression<E>;
}

pub struct VariableMap<const E: usize, const L: usize> {
    map: LinearMap<char, Expression<E>, L>,
}

impl<const E: usize, const L: usize> ExpressionMap<E> for VariableMap<E, L> {
    fn stack_contains(&self, stack: &Expression<E>) -> bool {
        for token in stack.tokens.iter() {
            match token {
                Token::Var(_) => return true,
                _ => continue,
            }
        }
        return false;
    }

    fn evaluate(&self, stack: &Expression<E>) -> Expression<E> {
        Expression { tokens: stack.tokens.clone() }
    }

    fn approximate(&self, stack: &Expression<E>) -> Expression<E> {
        Expression { tokens: stack.tokens
        .iter()
        .map(|token| {
            match token {
                Token::Var(var) => {
                    if let Some(expr) = self.map.get(&var) {
                        match expr.approximate(&Vec::<&dyn ExpressionMap<E>, 1>::from_slice(&[self]).unwrap()) {
                            Ok(Approx::Num(n)) => Ok(Some(Token::Number(n))),
                            Ok(Approx::Undef) => Ok(None),
                            Err(err) => Err(err),
                        }
                    } else {
                        return Ok(None);
                    }
                }
                _ => Ok(Some(token.clone())),
            }
        })
        .filter_map(Result::transpose)
        .collect::<Result<Vec<Token, E>, Error>>()
        .unwrap() }
    }
}

impl<const E: usize, const L: usize> VariableMap<E, L> {
    pub fn new() -> Self {
        VariableMap {
            map: LinearMap::<char, Expression<E>, L>::new(),
        }
    }

    pub fn insert(&mut self, var: char, expr: Expression<E>) -> Result<Option<Expression<E>>, Error> {
        self.map.insert(var, expr).map_err(|_| Error::NotEnoughMemory)
    }

    pub fn get(&self, var: char) -> Option<&Expression<E>> {
        self.map.get(&var)
    }

    pub fn remove(&mut self, var: char) -> Option<Expression<E>> {
        self.map.remove(&var)
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }
}
