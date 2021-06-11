//#![no_std]

extern crate alloc;

#[macro_use]
extern crate nom;

use alloc::vec::Vec;
use nom::number::complete::float;
use nom::IResult;

#[derive(Debug)]
pub enum Token {
    Add,
    Subtract,
    Divide,
    Multiply,
    Number(f32),
}

named!(math_token<&str, Token>,
    alt!(
        float => { |f| Token::Number(f) } |
        tag!("*") => { |_| Token::Multiply } |
        tag!("+") => { |_| Token::Add} |
        tag!("-") => { |_| Token::Subtract} |
        tag!("/") => { |_| Token::Divide}
    )
);

named!(math_expr<&str, Vec<Token>>, many0!(complete!(math_token)));

pub fn test(input: &str) -> IResult<&str, Vec<Token>> {
    math_expr(dbg!(input))
}
