/*use core::{
    fmt::{Display, Formatter, self},
    str::FromStr,
};

use heapless::{Vec, String};

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

//TODO: write as infix instead of postfix
impl<const E: usize> fmt::Display for Expression<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.tokens.iter().try_for_each(|token| {
            write!(f, "{}", token)
        })?;
        Ok(())
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
                _ => return Err(Error::InvalidSyntax), //TODO: create value enum and implement value precedence to sort and combine by num, then var, then func
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
*/