#![no_std]

extern crate alloc;

#[macro_use]
extern crate nom;

use alloc::vec::Vec;
use nom::{error::Error, number::complete::float as float_par, Err};

fn float(input: &str) -> Result<(&str, f32), Err<Error<&str>>> {
    float_par::<&str, Error<&str>>(input)
}

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Number(f32),
    Op(Operation),
}

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Add,
    Subtract,
    Divide,
    Multiply,
}

named!(math_token<&str, Token>,
    alt!(
        float => { |f| Token::Number(f) } |
        tag!("*") => { |_| Token::Op(Operation::Multiply) } |
        tag!("+") => { |_| Token::Op(Operation::Add)} |
        tag!("-") => { |_| Token::Op(Operation::Subtract)} |
        tag!("/") => { |_| Token::Op(Operation::Divide)}
    )
);

named!(math_expr<&str, Vec<Token>>, many0!(complete!(math_token)));

pub fn eval_math_expr(expr: Vec<Token>) -> f32 {
    let mut next_op = Operation::Add;
    let mut num: f32 = 0.0;
    for token in expr.iter() {
        match token {
            Token::Number(n) => {
                match next_op {
                    Operation::Add => num += n,
                    Operation::Subtract => num -= n,
                    Operation::Divide => num /= n,
                    Operation::Multiply => num *= n,
                }
                next_op = Operation::Add;
            }
            Token::Op(op) => next_op = *op,
        }
    }
    num
}

pub fn test(input: &str) -> Result<f32, ()> {
    Ok(eval_math_expr(math_expr(input).map_err(|_| ())?.1))
}
