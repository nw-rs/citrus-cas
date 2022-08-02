use core::fmt;
use alloc::boxed::Box;

use heapless::{Vec, String};

#[derive(Debug, Clone, PartialEq)]
pub enum Numeric {
    Integer(i32),
    Decimal(f32),
    Radical(i16, i16), //might be unnecessary?
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
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Numeric(n) => write!(f, "{}", n),
            Atom::Variable(v) => write!(f, "{}", v),
        }
    }
}

//concrete syntax tree, not an abstract syntax tree
#[derive(Debug, PartialEq)]
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
    //reorganizes the expression tree to combine similar operations
    pub fn simplify(&mut self) {
        
    }
    //simplifies, then iterates the evaluation maps over the tree
    pub fn evaluate(&self) -> Expression {
        unimplemented!()
    }
    //evaluates, then iterates the approximation maps over the tree
    pub fn approximate(&self) -> Result<f32, ()> {
        unimplemented!()
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