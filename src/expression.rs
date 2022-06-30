use core::{
    fmt::{Display, Formatter},
    str::FromStr,
};

use heapless::LinearMap;
use heapless::Vec;

use crate::{
    parser::math_expr,
    token::{Operation, Token},
    Error,
};

/// the shunting yard algorithm converts an infix
/// notation expression, for example 5 + 2 * 7, into
/// a postfix notation expression, for example 5 2 7 * +
fn shunting_yard<const E: usize>(tokens: Vec<Token, E>) -> Result<Vec<Token, E>, Error> {
    let mut output = Vec::new();
    let mut stack: Vec<Token, E> = Vec::new();

    for token in tokens {
        match token {
            Token::Op(op) => {
                while let Some(&Token::Op(top)) = stack.last() {
                    if top.precedence() >= op.precedence() {
                        output
                            .push(stack.pop().unwrap())
                            .map_err(|_| Error::NotEnoughMemory)?;
                    } else {
                        break;
                    }
                }
                stack.push(token).map_err(|_| Error::NotEnoughMemory)?;
            }
            Token::Paren(true) => {
                stack.push(token).map_err(|_| Error::NotEnoughMemory)?;
            }
            Token::Paren(false) => {
                while let Some(stack_token) = stack.last() {                 
                    if stack_token == &Token::Paren(true) {
                        stack.pop().unwrap();
                        break;
                    } else {
                        output
                            .push(stack.pop().unwrap())
                            .map_err(|_| Error::NotEnoughMemory)?;
                    }
                }
            }
            _ => {
                output.push(token).map_err(|_| Error::NotEnoughMemory)?;
            }
        }
    }

    while let Some(token) = stack.pop() {
        output.push(token).map_err(|_| Error::NotEnoughMemory)?;
    }

    Ok(output)
}

/// A sequence of tokens which make up
/// an expression in postfix notation
#[derive(Debug)]
pub struct Expression<const E: usize> {
    //TODO: implement postfix to infix fmt conversion
    pub tokens: Vec<Token, E>,
}

impl<const E: usize> FromStr for Expression<E> {
    type Err = Error; //TODO: error type

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let infix = math_expr(s).unwrap().1;
        let tokens = shunting_yard(infix)?;
        Ok(Expression { tokens })
    }
}

impl<const E: usize> Expression<E> {
    pub fn new(tokens: Vec<Token, E>) -> Self {
        Expression { tokens }
    }

    pub fn evaluate(&self) -> Result<Expression<E>, Error> {
        let mut stack: Vec<Token, E> = Vec::new();

        for token in self.tokens.iter() {
            match token {
                Token::Number(f) => {
                    stack
                        .push(Token::Number(*f))
                        .map_err(|_| Error::NotEnoughMemory)?;
                }
                Token::Op(op) => {
                    let rhs: f32 = match stack.pop().unwrap() {
                        //TODO: convert into a confined array to support non-binary operations
                        Token::Number(n) => n,
                        _ => return Ok(Expression::new(Vec::new())),
                    };
                    let lhs: f32 = match stack.pop().unwrap() {
                        Token::Number(n) => n,
                        _ => return Ok(Expression::new(Vec::new())),
                    };

                    let result = match op {
                        Operation::Add => lhs + rhs,
                        Operation::Subtract => lhs - rhs,
                        Operation::Multiply => lhs * rhs,
                        Operation::Divide => lhs / rhs,
                        Operation::Power => libm::powf(lhs, rhs),
                    };

                    stack
                        .push(Token::Number(result))
                        .map_err(|_| Error::NotEnoughMemory)?;
                }
                _ => unimplemented!(), //TODO: create value enum and implement value precedence to sort and combine by num, then var, then func
            }
        }

        Ok(Expression::new(stack))
    }

    pub fn approximate<const N: usize>(
        &self,
        vars: &LinearMap<char, Expression<E>, N>,
    ) -> Result<Approx, Error> {
        let tokens = self
            .tokens
            .iter()
            .map(|token| {
                match token {
                    Token::Var(var) => {
                        if let Some(expr) = vars.get(&var) {
                            //this feels like it's wrong somehow
                            match expr.approximate(vars) {
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
            .filter_map(Result::transpose);

        let mut result = Expression::<E>::new(tokens.collect::<Result<Vec<Token, E>, Error>>()?)
            .evaluate()?
            .tokens;

        if result.len() != 1 {
            //this should not occur, because variables and functions should be replaced with numbers, or return undefined before this
            return Ok(Approx::Undef);
        }
        Ok(match result.pop().unwrap() {
            Token::Number(n) => Approx::Num(n),
            _ => Approx::Undef,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Approx {
    Num(f32),
    Undef,
}

impl Display for Approx {
    fn fmt(&self, f: &mut Formatter) -> core::fmt::Result {
        match self {
            Approx::Num(n) => write!(f, "{}", n),
            Approx::Undef => write!(f, "undefined"),
        }
    }
}
