#![no_std]

extern crate arrayvec;
extern crate heapless;
extern crate micromath;

use arrayvec::{ArrayString, ArrayVec};
use core::{fmt, str::FromStr};
use heapless::LinearMap;
use micromath::F32;

use nom::{
    character::complete::{anychar, one_of},
    error::Error,
    number::complete::float as float_par,
    IResult,
};

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Number(F32),
    Op(Operation),
    Var(char),
    Paren(bool),
    Func(ArrayString<8>),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Number(n) => write!(f, "{}", n),
            Token::Op(op) => write!(f, "{}", op),
            Token::Var(var) => write!(f, "{}", var),
            Token::Paren(true) => write!(f, "("),
            Token::Paren(false) => write!(f, ")"),
            Token::Func(func) => write!(f, "{}", func),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Add,
    Subtract,
    Divide,
    Multiply,
    Power,
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operation::Add => write!(f, "+"),
            Operation::Subtract => write!(f, "-"),
            Operation::Divide => write!(f, "/"),
            Operation::Multiply => write!(f, "*"),
            Operation::Power => write!(f, "^"),
        }
    }
}

impl Operation {
    pub fn precedence(&self) -> u8 {
        match self {
            Operation::Add | Operation::Subtract => 1,
            Operation::Multiply | Operation::Divide => 2,
            Operation::Power => 3,
        }
    }
}

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
    Ok((i, Token::Number(F32(f))))
}

fn math_token(i: &str) -> IResult<&str, Token, Error<&str>> {
    nom::branch::alt((operation, float, parenthesis, variable))(i)
}

fn math_expr<const E: usize>(i: &str) -> IResult<&str, ArrayVec<Token, E>, Error<&str>> {
    nom::multi::fold_many0(
        nom::combinator::complete(math_token),
        ArrayVec::new(),
        |mut acc, t| {
            acc.push(t);
            acc
        },
    )(i)
}

/*
the shunting-yard algorithm converts an infix
notation expression, for example 5 + 2 * 7, into
a postfix notation expression, for example 5 2 7 * +
*/
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

/*
a ValueMap is a map of characters associated with
variables to the expressions that they should evaluate to.
*/
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
        let _ = self.map.insert(var, expr);
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

#[derive(Debug, Clone, Copy)]
pub enum Approx {
    Num(F32),
    Undef,
}

impl fmt::Display for Approx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Approx::Num(n) => write!(f, "{}", n),
            Approx::Undef => write!(f, "undefined"),
        }
    }
}

/*
an Expression is a sequence of tokens which comprise
a postfix notation expression
*/
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

pub fn parse_approx<const E: usize, const N: usize>(input: &str, map: &ValueMap<E, N>) -> Approx {
    match Expression::from_str(input.trim()) {
        Ok(it) => it,
        Err(_err) => unimplemented!(),
    }
    .approximate(map)
}
