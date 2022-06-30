use core::{fmt::{Display, Formatter, Result}, cmp::Ordering};

use heapless::String;

#[derive(Debug, Clone)]
pub enum Token {
    Number(f32),
    Op(Operation),
    Var(char),
    Paren(bool),
    Func(String<8>),
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Number(n1), Token::Number(n2)) => n1 == n2,
            (Token::Op(op1), Token::Op(op2)) => op1 == op2,
            (Token::Var(v1), Token::Var(v2)) => v1 == v2,
            (Token::Paren(p1), Token::Paren(p2)) => p1 == p2,
            (Token::Func(f1), Token::Func(f2)) => f1 == f2,
            _ => false,
        }
    }
}

impl Eq for Token {}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
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

impl Ord for Operation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.precedence().cmp(&other.precedence())
    }
}

impl PartialOrd for Operation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Operation {
    fn eq(&self, other: &Self) -> bool {
        self.precedence() == other.precedence()
    }
}

impl Eq for Operation {}

impl Operation {
    pub fn precedence(&self) -> u8 {
        match self {
            Operation::Add | Operation::Subtract => 1,
            Operation::Multiply | Operation::Divide => 2,
            Operation::Power => 3,
        }
    }

    pub fn right_associative(&self) -> bool {
        match self {
            Operation::Power => true,
            _ => false,
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Operation::Add => write!(f, "+"),
            Operation::Subtract => write!(f, "-"),
            Operation::Divide => write!(f, "/"),
            Operation::Multiply => write!(f, "*"),
            Operation::Power => write!(f, "^"),
        }
    }
}
