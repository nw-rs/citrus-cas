use core::str::FromStr;

use heapless::LinearMap;
use heapless::Vec;

use nom::{
    branch::alt,
    character::complete::{anychar, one_of},
    combinator::complete,
    error::Error,
    multi::fold_many0,
    number::complete::float as float_par,
    IResult,
};

use crate::{
    expression::{Approx, Expression},
    token::{Operation, Token},
};

fn operation(i: &str) -> IResult<&str, Token, Error<&str>> {
    let (i, t) = one_of("+-*/^")(i)?;

    Ok((
        i,
        Token::Op(match t {
            '+' => Operation::Add,
            '-' => Operation::Subtract,
            '/' => Operation::Divide,
            '*' => Operation::Multiply,
            '^' => Operation::Power,
            _ => unreachable!(),
        }),
    ))
}

fn parenthesis(i: &str) -> IResult<&str, Token, Error<&str>> {
    let (i, t) = one_of("()")(i)?;

    Ok((
        i,
        match t {
            '(' => Token::Paren(true),
            ')' => Token::Paren(false),
            _ => unreachable!(),
        },
    ))
}

fn variable(i: &str) -> IResult<&str, Token, Error<&str>> {
    let (i, c) = anychar(i)?;

    Ok((i, Token::Var(c)))
}

fn float(i: &str) -> IResult<&str, Token, Error<&str>> {
    let (i, f) = float_par(i)?;
    Ok((i, Token::Number(f)))
}

fn math_token(i: &str) -> IResult<&str, Token, Error<&str>> {
    alt((operation, float, parenthesis, variable))(i)
}

pub fn math_expr<const E: usize>(i: &str) -> IResult<&str, Vec<Token, E>, Error<&str>> {
    fold_many0(complete(math_token), Vec::new(), |mut acc, t| {
        acc.push(t).expect("Not enough memory."); //TODO: make this return an error instead of panicking
        acc
    })(i)
}

pub fn approx<const E: usize, const N: usize>(
    input: &str,
    map: &LinearMap<char, Expression<E>, N>,
) -> Result<Approx, crate::Error> {
    match Expression::from_str(input.trim()) {
        Ok(it) => it,
        Err(_err) => unimplemented!(),
    }
    .approximate(map)
}
