#![no_std]

extern crate alloc;

#[macro_use]
extern crate nom;

use core::{fmt, str::FromStr};
use alloc::{vec::Vec, collections::BTreeMap};

use nom::{
    character::complete::anychar as char_par, 
    number::complete::float as float_par,
    error::Error, 
    Err,
};

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Number(f32),
    Op(Operation),
    Var(char),
    Paren(bool),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Number(n) => write!(f, "{}", n),
            Token::Op(op) => write!(f, "{}", op),
            Token::Var(var) => write!(f, "{}", var),
            Token::Paren(true) => write!(f, "("),
            Token::Paren(false) => write!(f, ")"),
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

fn float(input: &str) -> Result<(&str, f32), Err<Error<&str>>> {
    float_par::<&str, Error<&str>>(input)
}

fn char(input: &str) -> Result<(&str, char), Err<Error<&str>>> {
    char_par::<&str, Error<&str>>(input)
}

named!(math_token<&str, Token>,
    alt!(
        tag!("*") => { |_| Token::Op(Operation::Multiply) } |
        tag!("+") => { |_| Token::Op(Operation::Add)} |
        tag!("-") => { |_| Token::Op(Operation::Subtract)} |
        tag!("/") => { |_| Token::Op(Operation::Divide)} |
        tag!("^") => { |_| Token::Op(Operation::Power)} |

        float => { |f| Token::Number(f) } |

        tag!("(") => { |_| Token::Paren(true) } |
        tag!(")") => { |_| Token::Paren(false) } |

        char => { |c| Token::Var(c) }
    )
);

named!(math_expr<&str, Vec<Token>>, many0!(complete!(math_token)));

/*
the shunting-yard algorithm converts an infix
notation expression, for example 5 + 2 * 7, into
a postfix notation expression, for example 5 2 7 * +
*/
fn shunting_yard(tokens: Vec<Token>) -> Vec<Token> {
    let mut output = Vec::new();
    let mut stack = Vec::new();

    for token in tokens {
        match token {
            Token::Number(_) => output.push(token),
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
            Token::Var(_) => output.push(token), //TODO: implement implicit multiplication
            Token::Paren(true) => stack.push(token),
            Token::Paren(false) => { 
                while let Some(&Token::Paren(true)) = stack.last() {
                    output.push(stack.pop().unwrap()); //I literally cannot remeber if this is correct
                }
                stack.pop().unwrap();
            }
        }
    }

    while let Some(token) = stack.pop() {
        output.push(token);
    }

    output
}

pub struct ValueMap {
    pub map: BTreeMap<char, Expression>
}

impl ValueMap {
    pub fn new() -> Self {
        ValueMap {
            map: BTreeMap::new()
        }
    }

    pub fn insert(&mut self, var: char, expr: Expression) {
        self.map.insert(var, expr);
    }

    pub fn get(&self, var: char) -> Option<&Expression> {
        self.map.get(&var)
    }

    pub fn get_mut(&mut self, var: char) -> Option<&mut Expression> {
        self.map.get_mut(&var)
    }

    pub fn remove(&mut self, var: char) -> Option<Expression> {
        self.map.remove(&var)
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Approx {
    Num(f32),
    Undef
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
an expression is a sequence of tokens which comprise
a postfix notation expression
*/
pub struct Expression { //TODO: implement postfix to infix fmt conversion
    pub tokens: Vec<Token>, //replace with something not alloc?
}

impl FromStr for Expression {
    type Err = (); //TODO: error type

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let infix = math_expr(s).unwrap().1;
        let tokens = shunting_yard(infix);
        Ok(Expression { tokens })
    }
}

impl Expression {
    pub fn new(tokens: Vec<Token>) -> Self {
        Expression { tokens }
    }

    pub fn evaluate(&self) -> Expression {
        let mut stack: Vec<Token> = Vec::new();
        for token in self.tokens.iter() {
            match token {
                Token::Number(f) => stack.push(Token::Number(*f)),
                Token::Op(op) => {
                    let rhs: f32 = match stack.pop().unwrap() { //TODO: convert into a confined array to support non-binary operations
                        Token::Number(n) => n,
                        _ => return Expression::new(Vec::new()),
                    };
                    let lhs: f32 = match stack.pop().unwrap() {
                        Token::Number(n) => n,
                        _ => return Expression::new(Vec::new()),
                    };
                    
                    let result = match op {
                        Operation::Add => lhs + rhs,
                        Operation::Subtract => lhs - rhs,
                        Operation::Multiply => lhs * rhs,
                        Operation::Divide => lhs / rhs,
                        Operation::Power => unimplemented!(), //TODO: add micromath or libm for f32 approximation
                    };
                    stack.push(Token::Number(result));
                }
                Token::Var(_) => unimplemented!(), //TODO: create value enum and implement value precedence to sort and combine by num, then var, then func
                Token::Paren(_) => unimplemented!(),
            }
        }
        Expression::new(stack)
    }

    pub fn approximate(&self, vars: &ValueMap) -> Approx {
        let mut replace = self.tokens.clone();

        for token in replace.iter_mut() {
            match token {
                Token::Var(var) => {
                    if let Some(expr) = vars.get(*var) { //this feels like it's wrong somehow
                        *token = match expr.approximate(vars) {
                            Approx::Num(n) => Token::Number(n),
                            Approx::Undef => return Approx::Undef,
                        };
                    }
                    else {
                        return Approx::Undef;
                    }
                }
                _ => {}
            }
        }

        match (Expression::new(replace)).evaluate().tokens.pop().unwrap() {
            Token::Number(n) => Approx::Num(n),
            _ => Approx::Undef,
        }
    }
}

pub fn parse_approx(input: &str, map: &ValueMap) -> Approx {
    match Expression::from_str(input.trim()) {
        Ok(it) => it,
        Err(_err) => unimplemented!(),
    }.approximate(map)
}
