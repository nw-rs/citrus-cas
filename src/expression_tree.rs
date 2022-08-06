use core::{fmt, str::FromStr};
use alloc::boxed::Box;

use heapless::{Vec, String,};

use crate::{parser::parse, Error, modifier::Modifier};

#[derive(Debug, Clone, PartialEq)]
pub enum Numeric {
    Integer(i32),
    Decimal(f32),
    Radical(i16, i16), //might be unnecessary?
}

impl Into<f32> for Numeric {
    fn into(self) -> f32 {
        match self {
            Numeric::Integer(i) => i as f32,
            Numeric::Decimal(d) => d,
            Numeric::Radical(n, d) => n as f32 / d as f32,
        }
    }
}

impl fmt::Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Numeric::Integer(i) => write!(f, "{}", i),
            Numeric::Decimal(d) => write!(f, "{}", d),
            Numeric::Radical(r1, r2) => write!(f, "({})/({})", r1, r2), //must always be in the form of (a)/(b), for reinterpretation to work (?)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Numeric(Numeric),
    Variable(char),
    Escape(char, u8),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Numeric(n) => write!(f, "{}", n),
            Atom::Variable(v) => write!(f, "{}", v),
            Atom::Escape(e, n) => write!(f, "_{}{}", e, n),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    //atoms
    Atom(Atom),

    //unary operators
    Negate(Box<Expression>),
    Factorial(Box<Expression>),
    Percent(Box<Expression>),

    //binary operators
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Power(Box<Expression>, Box<Expression>),
    Modulus(Box<Expression>, Box<Expression>),

    //n-ary operators
    Function { //TODO: make smaller somehow?
        name: String<8>,
        args: Vec<Box<Expression>, 8>,
    },
}

impl Expression {
    //iterates self bottom-up over a given modifier
    fn mod_iterate<T: Modifier>(&mut self, modifier: &T) -> bool {
        match self {
            Expression::Atom(_) => {
                modifier.modify(self)
            }
            Expression::Negate(e) |
            Expression::Factorial(e) |
            Expression::Percent(e) => {
                let sub_sustained = e.mod_iterate(modifier);
                modifier.modify(self) || sub_sustained
            },
            Expression::Add(e1, e2) |
            Expression::Subtract(e1, e2) |
            Expression::Multiply(e1, e2) |
            Expression::Divide(e1, e2) |
            Expression::Power(e1, e2) |
            Expression::Modulus(e1, e2) => {
                let sub_sustained = e1.mod_iterate(modifier);
                let sub_sustained = e2.mod_iterate(modifier) || sub_sustained;
                modifier.modify(self) || sub_sustained
            },
            Expression::Function { args, .. } => {
                let mut sub_sustained = false;
                for arg in args {
                    sub_sustained = arg.mod_iterate(modifier) || sub_sustained;
                }
                modifier.modify(self) || sub_sustained
            },
        }
    }

    //reorganizes the expression tree to combine similar operations
    pub fn simplify<S: Modifier>(&mut self, simplifier: &S) {
        loop {
            if !self.mod_iterate(simplifier) {
                break;
            }
        }
    }

    //simplifies, then uses the evaluation modifier on the tree
    pub fn evaluate<E: Modifier, S: Modifier>(&self, evaluator: &E, simplifier: &S) -> Expression {
        let mut expr = self.clone();

        loop {
            expr.simplify(simplifier);
            if !expr.mod_iterate(evaluator) {
                break;
            }
        }

        expr
    }

    //evaluates, then uses the approximation modifier on the tree
    pub fn approximate<A: Modifier, E: Modifier, S: Modifier>(&self, approximator: &A, evaluator: &E, simplifier: &S) -> Result<f32, ()> {
        let mut expr = self.clone();

        loop {
            expr = expr.evaluate(evaluator, simplifier);
            if !expr.mod_iterate(approximator) {
                break;
            }
        }

        match expr {
            Expression::Atom(a) => match a {
                Atom::Numeric(n) => Ok(n.into()),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}

impl fmt::Display for Expression {
    //TODO: smarter parentheses, likely using some kind of traversal?
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Atom(a) => write!(f, "{}", a),
            
            Expression::Negate(e) => write!(f, "-({})", e),
            Expression::Factorial(e) => write!(f, "({})!", e),
            Expression::Percent(e) => write!(f, "({})%", e),
            
            Expression::Add(l, r) => write!(f, "({} + {})", l, r),
            Expression::Subtract(l, r) => write!(f, "({} - {})", l, r),
            Expression::Multiply(l, r) => write!(f, "({} * {})", l, r),
            Expression::Divide(l, r) => write!(f, "({} / {})", l, r),
            Expression::Power(l, r) => write!(f, "({} ^ {})", l, r),
            Expression::Modulus(l, r) => write!(f, "({} % {})", l, r),
            
            Expression::Function { name, args } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl FromStr for Expression {
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(parse(s))
    }

    type Err = Error;
}

#[cfg(test)]
mod tests {
    use core::str::FromStr;

    use alloc::string::ToString;

    use super::Expression;

    #[test]
    fn test_fmt_parse() {
        //TODO: fix Expression to_string()
        assert_eq!(Expression::from_str("1 * 3 + 5 / 6 + sin(x) + -6").unwrap(),
            Expression::from_str(Expression::from_str("1 * 3 + 5 / 6 + sin(x) + -6").unwrap().to_string().as_str()).unwrap()
        )
    }
}