use core::{
    fmt::{Display, Formatter},
    str::FromStr,
};

use heapless::{Vec, String};

use crate::{
    parser::math_expr,
    token::{Operation, Token},
    Error,
    expression_map::ExpressionMap,
};

fn function_constructor<const E: usize>(tokens: Vec<Token, E>) -> Result<Vec<Token, E>, Error> {
    let mut output = Vec::new();

    let mut function_staging = String::<8>::new();
    for token in tokens {
        match token {
            Token::Var(var) => function_staging.push(var).map_err(|_| Error::NotEnoughMemory)?,
            _ => {
                match function_staging.len() {
                    0 => {}
                    1 => output.push(Token::Var(function_staging.pop().unwrap())).unwrap(),
                    _ => {
                        output.push(Token::Func(function_staging.clone()).into()).unwrap();
                        function_staging.clear();
                    }
                }
                output.push(token).map_err(|_| Error::NotEnoughMemory)?;
            }
        }
    }

    Ok(output)
}

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
                //TODO: this is incorrect, because it doesn't check whether or not it's in a function
                stack.push(Token::Terminator).map_err(|_| Error::NotEnoughMemory)?;
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
#[derive(Debug, Clone)]
pub struct Expression<const E: usize> {
    //TODO: implement postfix to infix fmt conversion
    pub tokens: Vec<Token, E>,
}

impl<const E: usize> FromStr for Expression<E> {
    type Err = Error; //TODO: error type

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let infix = math_expr(s).unwrap().1;
        let function_added = function_constructor(infix)?;
        let tokens = shunting_yard(function_added)?;
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
                    //TODO: convert into a confined array to support non-binary operations
                    let rhs: f32 = match stack.pop().unwrap() {
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
        maps: &Vec<&dyn ExpressionMap<E>, N>, //forgive me father for I have sinned
    ) -> Result<Approx, Error> {
        let mut exprs = Expression::new(self.tokens.clone());

        //this checks whether the maps need to continue operating on the expression
        while maps.iter().fold(false, |t, map| {
            t || map.stack_contains(&exprs)
        }) {
            //every ExpressionMap is applied to the expression
            for map in maps.iter() {
                exprs = map.approximate(&exprs);
            }
        }

        let mut ensure_implimentation: Expression<E> = Expression::new(Vec::new());
        for token in exprs.tokens.iter() {
            match token {
                &Token::Terminator => {},
                _ => {
                    ensure_implimentation.tokens.push(token.clone())
                        .map_err(|_| Error::NotEnoughMemory)?;
                }
            }
        }

        let mut result = ensure_implimentation
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
