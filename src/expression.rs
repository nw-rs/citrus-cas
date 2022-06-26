use core::{str::FromStr, fmt::{Formatter, Display}};

use arrayvec::ArrayVec;
use micromath::F32;

use crate::{token::{Token, Operation}, parser::math_expr, ValueMap};

/// the shunting-yard algorithm converts an infix
/// notation expression, for example 5 + 2 * 7, into
/// a postfix notation expression, for example 5 2 7 * +
fn shunting_yard<const E: usize>(tokens: ArrayVec<Token, E>) -> ArrayVec<Token, E> {
    let mut output = ArrayVec::new();
    let mut stack: ArrayVec<Token, E> = ArrayVec::new();

    for token in tokens {
        match token {
            Token::Op(op) => {
                while let Some(&Token::Op(top)) = stack.last() {
                    if top.precedence() >= op.precedence() {
                        output.push(stack.pop().unwrap());
                    } else {
                        break;
                    }
                }
                stack.push(token);
            }
            Token::Paren(true) => stack.push(token),
            Token::Paren(false) => {
                while let Some(&Token::Paren(true)) = stack.last() {
                    output.push(stack.pop().unwrap()); //I literally cannot remeber if this is correct
                }
                stack.pop().unwrap();
            }
            _ => output.push(token),
        }
    }

    while let Some(token) = stack.pop() {
        output.push(token);
    }

    output
}

/// A sequence of tokens which make up
/// an expression in postfix notation
#[derive(Debug)]
pub struct Expression<const E: usize> {
    //TODO: implement postfix to infix fmt conversion
    pub tokens: ArrayVec<Token, E>,
}

impl<const E: usize> FromStr for Expression<E> {
    type Err = (); //TODO: error type

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let infix = math_expr(s).unwrap().1;
        let tokens = shunting_yard(infix);
        Ok(Expression { tokens })
    }
}

impl<const E: usize> Expression<E> {
    pub fn new(tokens: ArrayVec<Token, E>) -> Self {
        Expression { tokens }
    }

    pub fn evaluate(&self) -> Expression<E> {
        let mut stack: ArrayVec<Token, E> = ArrayVec::new();

        for token in self.tokens.iter() {
            match token {
                Token::Number(f) => stack.push(Token::Number(*f)),
                Token::Op(op) => {
                    let rhs: F32 = match stack.pop().unwrap() {
                        //TODO: convert into a confined array to support non-binary operations
                        Token::Number(n) => n,
                        _ => return Expression::new(ArrayVec::new()),
                    };
                    let lhs: F32 = match stack.pop().unwrap() {
                        Token::Number(n) => n,
                        _ => return Expression::new(ArrayVec::new()),
                    };

                    let result = match op {
                        Operation::Add => lhs + rhs,
                        Operation::Subtract => lhs - rhs,
                        Operation::Multiply => lhs * rhs,
                        Operation::Divide => lhs / rhs,
                        Operation::Power => lhs.powf(rhs),
                    };

                    stack.push(Token::Number(result));
                }
                _ => unimplemented!(), //TODO: create value enum and implement value precedence to sort and combine by num, then var, then func
            }
        }

        Expression::new(stack)
    }

    pub fn approximate<const N: usize>(&self, vars: &ValueMap<E, N>) -> Approx {
        let mut replace = self.tokens.clone();

        for token in replace.iter_mut() {
            match token {
                Token::Var(var) => {
                    if let Some(expr) = vars.get(*var) {
                        //this feels like it's wrong somehow
                        *token = match expr.approximate(vars) {
                            Approx::Num(n) => Token::Number(n),
                            Approx::Undef => return Approx::Undef,
                        };
                    } else {
                        return Approx::Undef;
                    }
                }
                _ => {}
            }
        }

        let mut result = Expression::new(replace).evaluate().tokens;
        if result.len() != 1 {
            //this should not occur, because variables and functions should be replaced with numbers, or return undefined before this
            return Approx::Undef;
        }
        match result.pop().unwrap() {
            Token::Number(n) => Approx::Num(n),
            _ => Approx::Undef,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Approx {
    Num(F32),
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