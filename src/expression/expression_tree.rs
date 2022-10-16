use alloc::{boxed::Box, string::String, vec::Vec};
use core::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::{Add, Div, Mul, Neg, Sub},
    str::FromStr,
};

use heapless::LinearMap;

use crate::{
    expression::parser::parse,
    modifier::{adaptable_modifier::ModifierFunction, ModifierImmutable, ModifierMutable},
    Error,
};

// Numeric: representation of any numeric value
#[derive(Debug, Clone, Copy)]
pub enum Numeric {
    Integer(i32),
    Decimal(f32),
    Fraction(i32, i32), // might be unnecessary?
}

impl From<Numeric> for f32 {
    fn from(n: Numeric) -> f32 {
        match n {
            Numeric::Integer(i) => i as f32,
            Numeric::Decimal(d) => d,
            Numeric::Fraction(n, d) => n as f32 / d as f32,
        }
    }
}

impl From<f32> for Numeric {
    fn from(f: f32) -> Numeric {
        Numeric::Decimal(f)
    }
}

impl From<Numeric> for i32 {
    fn from(n: Numeric) -> i32 {
        match n {
            Numeric::Integer(i) => i,
            Numeric::Decimal(d) => d as i32,
            Numeric::Fraction(n, d) => n / d,
        }
    }
}

impl From<i32> for Numeric {
    fn from(i: i32) -> Numeric {
        Numeric::Integer(i)
    }
}

impl PartialOrd for Numeric {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Numeric::Integer(i1), Numeric::Integer(i2)) => i1.partial_cmp(i2),
            (Numeric::Integer(i1), Numeric::Decimal(d2)) => (*i1 as f32).partial_cmp(d2),
            (Numeric::Integer(i1), Numeric::Fraction(n2, d2)) => {
                (*i1 as f32).partial_cmp(&(*n2 as f32 / *d2 as f32))
            }
            (Numeric::Decimal(d1), Numeric::Integer(i2)) => d1.partial_cmp(&(*i2 as f32)),
            (Numeric::Decimal(d1), Numeric::Decimal(d2)) => d1.partial_cmp(d2),
            (Numeric::Decimal(d1), Numeric::Fraction(n2, d2)) => {
                d1.partial_cmp(&(*n2 as f32 / *d2 as f32))
            }
            (Numeric::Fraction(n1, d1), Numeric::Integer(i2)) => {
                (*n1 as f32 / *d1 as f32).partial_cmp(&(*i2 as f32))
            }
            (Numeric::Fraction(n1, d1), Numeric::Decimal(d2)) => {
                (*n1 as f32 / *d1 as f32).partial_cmp(d2)
            }
            (Numeric::Fraction(n1, d1), Numeric::Fraction(n2, d2)) => {
                (*n1 as f32 / *d1 as f32).partial_cmp(&(*n2 as f32 / *d2 as f32))
            }
        }
    }
}

impl PartialEq for Numeric {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Numeric::Integer(i), Numeric::Integer(j)) => i == j,
            (Numeric::Decimal(i), Numeric::Decimal(j)) => i == j,
            (Numeric::Fraction(i, j), Numeric::Fraction(k, l)) => i * l == j * k,
            (Numeric::Integer(i), Numeric::Decimal(j)) => *i as f32 == *j,
            (Numeric::Decimal(i), Numeric::Integer(j)) => *i == *j as f32,
            (Numeric::Integer(i), Numeric::Fraction(j, k)) => *i == *j / *k,
            (Numeric::Fraction(i, j), Numeric::Integer(k)) => *i == *k * *j,
            (Numeric::Decimal(i), Numeric::Fraction(j, k)) => *i == *j as f32 / *k as f32,
            (Numeric::Fraction(i, j), Numeric::Decimal(k)) => *i as f32 / *j as f32 == *k,
        }
    }
}

impl Eq for Numeric {}

impl Hash for Numeric {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Numeric::Integer(i) => i.hash(state),
            Numeric::Decimal(f) => f.to_bits().hash(state), // rough workaround
            Numeric::Fraction(n, d) => {
                n.hash(state);
                d.hash(state);
            }
        }
    }
}

impl Add for Numeric {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Numeric::Integer(a), Numeric::Integer(b)) => Numeric::Integer(a + b),
            (Numeric::Decimal(a), Numeric::Decimal(b)) => Numeric::Decimal(a + b),
            (Numeric::Integer(a), Numeric::Decimal(b)) => Numeric::Decimal(a as f32 + b),
            (Numeric::Decimal(a), Numeric::Integer(b)) => Numeric::Decimal(a + b as f32),
            (Numeric::Fraction(a, b), Numeric::Fraction(c, d)) => {
                if b == d {
                    Numeric::Fraction(a + c, b)
                } else {
                    Numeric::Decimal((a as f32 / b as f32) + (c as f32 / d as f32))
                }
            }
            (Numeric::Integer(a), Numeric::Fraction(b, c)) => {
                Numeric::Decimal(a as f32 + (b as f32 / c as f32))
            }
            (Numeric::Fraction(a, b), Numeric::Integer(c)) => {
                Numeric::Decimal((a as f32 / b as f32) + c as f32)
            }
            (Numeric::Decimal(a), Numeric::Fraction(b, c)) => {
                Numeric::Decimal(a + (b as f32 / c as f32))
            }
            (Numeric::Fraction(a, b), Numeric::Decimal(c)) => {
                Numeric::Decimal((a as f32 / b as f32) + c)
            }
        }
    }
}

impl Sub for Numeric {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Numeric::Integer(a), Numeric::Integer(b)) => Numeric::Integer(a - b),
            (Numeric::Decimal(a), Numeric::Decimal(b)) => Numeric::Decimal(a - b),
            (Numeric::Integer(a), Numeric::Decimal(b)) => Numeric::Decimal(a as f32 - b),
            (Numeric::Decimal(a), Numeric::Integer(b)) => Numeric::Decimal(a - b as f32),
            (Numeric::Fraction(a, b), Numeric::Fraction(c, d)) => {
                if b == d {
                    Numeric::Fraction(a - c, b)
                } else {
                    Numeric::Decimal((a as f32 / b as f32) - (c as f32 / d as f32))
                }
            }
            (Numeric::Integer(a), Numeric::Fraction(b, c)) => {
                Numeric::Decimal(a as f32 - (b as f32 / c as f32))
            }
            (Numeric::Fraction(a, b), Numeric::Integer(c)) => {
                Numeric::Decimal((a as f32 / b as f32) - c as f32)
            }
            (Numeric::Decimal(a), Numeric::Fraction(b, c)) => {
                Numeric::Decimal(a - (b as f32 / c as f32))
            }
            (Numeric::Fraction(a, b), Numeric::Decimal(c)) => {
                Numeric::Decimal((a as f32 / b as f32) - c)
            }
        }
    }
}

impl Mul for Numeric {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Numeric::Integer(a), Numeric::Integer(b)) => Numeric::Integer(a * b),
            (Numeric::Decimal(a), Numeric::Decimal(b)) => Numeric::Decimal(a * b),
            (Numeric::Integer(a), Numeric::Decimal(b)) => Numeric::Decimal(a as f32 * b),
            (Numeric::Decimal(a), Numeric::Integer(b)) => Numeric::Decimal(a * b as f32),
            (Numeric::Fraction(a, b), Numeric::Fraction(c, d)) => {
                Numeric::Fraction((a * d) + (b * c), b * d)
            }
            (Numeric::Integer(a), Numeric::Fraction(b, c)) => Numeric::Fraction((a * c) + b, c),
            (Numeric::Fraction(a, b), Numeric::Integer(c)) => Numeric::Fraction(a + (b * c), b),
            (Numeric::Decimal(a), Numeric::Fraction(b, c)) => {
                Numeric::Decimal(a * (b as f32 / c as f32))
            }
            (Numeric::Fraction(a, b), Numeric::Decimal(c)) => {
                Numeric::Decimal((a as f32 / b as f32) * c)
            }
        }
    }
}

impl Div for Numeric {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Numeric::Integer(a), Numeric::Integer(b)) => Numeric::Integer(a / b),
            (Numeric::Decimal(a), Numeric::Decimal(b)) => Numeric::Decimal(a / b),
            (Numeric::Integer(a), Numeric::Decimal(b)) => Numeric::Decimal(a as f32 / b),
            (Numeric::Decimal(a), Numeric::Integer(b)) => Numeric::Decimal(a / b as f32),
            (Numeric::Fraction(a, b), Numeric::Fraction(c, d)) => {
                Numeric::Fraction((a * d) - (b * c), b * d)
            }
            (Numeric::Integer(a), Numeric::Fraction(b, c)) => Numeric::Fraction((a * c) - b, c),
            (Numeric::Fraction(a, b), Numeric::Integer(c)) => Numeric::Fraction(a - (b * c), b),
            (Numeric::Decimal(a), Numeric::Fraction(b, c)) => {
                Numeric::Decimal(a / (b as f32 / c as f32))
            }
            (Numeric::Fraction(a, b), Numeric::Decimal(c)) => {
                Numeric::Decimal((a as f32 / b as f32) / c)
            }
        }
    }
}

impl Neg for Numeric {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Numeric::Integer(a) => Numeric::Integer(-a),
            Numeric::Decimal(a) => Numeric::Decimal(-a),
            Numeric::Fraction(a, b) => Numeric::Fraction(-a, b),
        }
    }
}

impl fmt::Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Numeric::Integer(i) => write!(f, "{}", i),
            Numeric::Decimal(d) => write!(f, "{}", d),
            Numeric::Fraction(r1, r2) => write!(f, "({})/({})", r1, r2), // must always be in the form of (a)/(b), for reinterpretation to work (?)
        }
    }
}

// Atom: the smallest unit of an expression
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Copy, Hash)]
pub enum Atom {
    Numeric(Numeric),
    Variable(char),
    // escapes indicate where something in an expression should be replaced:
    // - A for atoms
    // - F for functions
    // - V for vectors
    // - M for matrices
    // - * for everything
    Escape(char, u8),
    Error(crate::Error),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Numeric(n) => write!(f, "{}", n),
            Atom::Variable(v) => write!(f, "{}", v),
            Atom::Escape(e, n) => write!(f, "_{}{}", e, n),
            Atom::Error(e) => write!(f, "{}", e),
        }
    }
}

// Expression: a tree representing a mathematical expression
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    // atoms
    Atom(Atom),

    // unary operators
    Negate(Box<Self>),
    Factorial(Box<Self>),
    Percent(Box<Self>),

    // binary operators
    Add(Box<Self>, Box<Self>),
    Subtract(Box<Self>, Box<Self>),
    Multiply(Box<Self>, Box<Self>),
    Divide(Box<Self>, Box<Self>),
    Power(Box<Self>, Box<Self>),
    Modulus(Box<Self>, Box<Self>),

    // dynamic operators
    Function {
        name: String,
        args: Vec<Box<Self>>,
    },
    Vector {
        backing: Vec<Box<Self>>,
        size: u8,
    },
    Matrix {
        backing: Vec<Box<Self>>,
        shape: (u8, u8),
    },
}

impl Expression {
    // reorganizes the expression tree using the given modifier a max of L times, with a modifier that cannot be mutated
    pub fn simplify_im<S: ModifierImmutable, const L: usize>(&mut self, simplifier: &S) {
        for _ in 0..L {
            if !simplifier.modify_immut(self) {
                break;
            }
        }
    }

    // simplifies, then uses the evaluation modifier on the tree a max of L times, with a modifier that cannot be mutated
    pub fn evaluate_im<E: ModifierImmutable, S: ModifierImmutable, const L: usize>(
        &self,
        evaluator: &E,
        simplifier: &S,
    ) -> Expression {
        let mut expr = self.clone();

        for _ in 0..L {
            expr.simplify_im::<S, L>(simplifier);
            if !evaluator.modify_immut(&mut expr) {
                break;
            }
        }

        expr
    }

    // evaluates, then uses the approximation modifier on the tree a max of L times, with a modifier that cannot be mutated
    pub fn approximate_im<
        A: ModifierImmutable,
        E: ModifierImmutable,
        S: ModifierImmutable,
        const L: usize,
    >(
        &self,
        approximator: &A,
        evaluator: &E,
        simplifier: &S,
    ) -> Result<Expression, crate::Error> {
        let mut expr = self.clone();

        for _ in 0..L {
            expr = expr.evaluate_im::<E, S, L>(evaluator, simplifier);
            if !approximator.modify_immut(&mut expr) {
                break;
            }
        }

        expr.approximated()
    }

    // reorganizes the expression tree using the given modifier a max of L times
    pub fn simplify<S: ModifierMutable, const L: usize>(&mut self, simplifier: &mut S) {
        for _ in 0..L {
            if !simplifier.modify_mut(self) {
                break;
            }
        }
    }

    // simplifies, then uses the evaluation modifier on the tree a max of L times
    pub fn evaluate<E: ModifierMutable, S: ModifierMutable, const L: usize>(
        &self,
        evaluator: &mut E,
        simplifier: &mut S,
    ) -> Expression {
        let mut expr = self.clone();

        for _ in 0..L {
            expr.simplify::<S, L>(simplifier);
            if !evaluator.modify_mut(&mut expr) {
                break;
            }
        }

        expr
    }

    // evaluates, then uses the approximation modifier on the tree a max of L times
    pub fn approximate<
        A: ModifierMutable,
        E: ModifierMutable,
        S: ModifierMutable,
        const L: usize,
    >(
        &self,
        approximator: &mut A,
        evaluator: &mut E,
        simplifier: &mut S,
    ) -> Result<Expression, crate::Error> {
        let mut expr = self.clone();

        for _ in 0..L {
            expr = expr.evaluate::<E, S, L>(evaluator, simplifier);
            if !approximator.modify_mut(&mut expr) {
                break;
            }
        }

        expr.approximated()
    }

    fn approximated(self) -> Result<Expression, crate::Error> {
        match self {
            Expression::Atom(Atom::Numeric(n)) => Ok(match n < Numeric::Integer(0) {
                true => Expression::Negate(Box::new(Expression::Atom(Atom::Numeric(-n)))),
                false => Expression::Atom(Atom::Numeric(n)),
            }),
            Expression::Negate(e) => {
                if let Ok(n) = e.approximated() {
                    Ok(Expression::Negate(Box::new(n)))
                } else {
                    Err(Error::UndefinedSymbol)
                }
            }
            _ => Err(Error::UndefinedSymbol),
        }
    }

    // returns the number of escapes in the other expression, or None if the expressions are not equal
    pub fn level_eq(&self, other: &Self, map: &mut LinearMap<Atom, Expression, 8>) -> Option<u8> {
        match (self, other) {
            (e, Expression::Atom(a)) => match a {
                Atom::Escape(escape, _) => match escape {
                    'A' => match e {
                        Expression::Atom(a) => match map.get(a) {
                            Some(expr) => {
                                if expr == e {
                                    Some(1)
                                } else {
                                    None
                                }
                            }
                            None => {
                                map.insert(*a, e.clone())
                                    .map_err(|(_, _)| "too many escapes")
                                    .unwrap();
                                Some(1)
                            }
                        },
                        _ => None,
                    },
                    'F' => match e {
                        Expression::Function { name: _, args: _ } => match map.get(a) {
                            Some(expr) => {
                                if expr == e {
                                    Some(1)
                                } else {
                                    None
                                }
                            }
                            None => {
                                map.insert(*a, e.clone())
                                    .map_err(|(_, _)| "too many escapes")
                                    .unwrap();
                                Some(1)
                            }
                        },
                        _ => None,
                    },
                    'V' => match e {
                        Expression::Vector {
                            backing: _,
                            size: _,
                        } => match map.get(a) {
                            Some(expr) => {
                                if expr == e {
                                    Some(1)
                                } else {
                                    None
                                }
                            }
                            None => {
                                map.insert(*a, e.clone())
                                    .map_err(|(_, _)| "too many escapes")
                                    .unwrap();
                                Some(1)
                            }
                        },
                        _ => None,
                    },
                    'M' => match e {
                        Expression::Matrix {
                            backing: _,
                            shape: _,
                        } => match map.get(a) {
                            Some(expr) => {
                                if expr == e {
                                    Some(1)
                                } else {
                                    None
                                }
                            }
                            None => {
                                map.insert(*a, e.clone())
                                    .map_err(|(_, _)| "too many escapes")
                                    .unwrap();
                                Some(1)
                            }
                        },
                        _ => None,
                    },
                    '*' => match map.get(a) {
                        Some(expr) => {
                            if expr == e {
                                Some(1)
                            } else {
                                None
                            }
                        }
                        None => {
                            map.insert(*a, e.clone())
                                .map_err(|(_, _)| "too many escapes")
                                .unwrap();
                            Some(1)
                        }
                    },
                    _ => unimplemented!(),
                },
                _ => {
                    if self == other {
                        Some(0)
                    } else {
                        None
                    }
                }
            },
            (
                Expression::Function { name: n1, args: a1 },
                Expression::Function { name: n2, args: a2 },
            ) => {
                if n1 == n2 {
                    let mut level = 0;

                    for (arg1, arg2) in a1.iter().zip(a2.iter()) {
                        match arg1.level_eq(arg2, map) {
                            Some(l) => level += l,
                            None => return None,
                        };
                    }

                    Some(level)
                } else {
                    None
                }
            }
            (
                Expression::Vector {
                    backing: b1,
                    size: s1,
                },
                Expression::Vector {
                    backing: b2,
                    size: s2,
                },
            ) => {
                if s1 == s2 {
                    let mut level = 0;

                    for (expr1, expr2) in b1.iter().zip(b2.iter()) {
                        match expr1.level_eq(expr2, map) {
                            Some(l) => level += l,
                            None => return None,
                        };
                    }

                    Some(level)
                } else {
                    None
                }
            }
            (
                Expression::Matrix {
                    backing: b1,
                    shape: s1,
                },
                Expression::Matrix {
                    backing: b2,
                    shape: s2,
                },
            ) => {
                if s1 == s2 {
                    let mut level = 0;

                    for (expr1, expr2) in b1.iter().zip(b2.iter()) {
                        match expr1.level_eq(expr2, map) {
                            Some(l) => level += l,
                            None => return None,
                        };
                    }

                    Some(level)
                } else {
                    None
                }
            }
            (Expression::Negate(e1), Expression::Negate(e2))
            | (Expression::Factorial(e1), Expression::Factorial(e2))
            | (Expression::Percent(e1), Expression::Percent(e2)) => e1.level_eq(e2, map),
            (Expression::Add(e11, e12), Expression::Add(e21, e22))
            | (Expression::Subtract(e11, e12), Expression::Subtract(e21, e22))
            | (Expression::Multiply(e11, e12), Expression::Multiply(e21, e22))
            | (Expression::Divide(e11, e12), Expression::Divide(e21, e22))
            | (Expression::Power(e11, e12), Expression::Power(e21, e22))
            | (Expression::Modulus(e11, e12), Expression::Modulus(e21, e22)) => {
                let level1 = e11.level_eq(e21, map);
                let level2 = e12.level_eq(e22, map);

                match (level1, level2) {
                    (Some(l1), Some(l2)) => Some(l1 + l2),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    // extracts a modification function from the given expression
    pub fn conversion(self) -> ModifierFunction {
        Box::new(move |map: &LinearMap<Atom, Expression, 8>| {
            (
                match &self {
                    Expression::Atom(a) => match a {
                        Atom::Escape(_, _) => map.get(a).unwrap().clone(),
                        _ => self.clone(),
                    },
                    Expression::Function { name: n, args: a } => {
                        let mut args = Vec::new();

                        for arg in a.clone() {
                            args.push(Box::new(arg.conversion()(map).0)); //vec overflow is impossible here
                        }

                        Expression::Function {
                            name: n.clone(),
                            args,
                        }
                    }
                    Expression::Vector {
                        backing: b,
                        size: s,
                    } => {
                        let mut backing = alloc::vec::Vec::new();

                        for arg in b.clone() {
                            backing.push(Box::new(arg.conversion()(map).0));
                        }

                        Expression::Vector { backing, size: *s }
                    }
                    Expression::Matrix {
                        backing: b,
                        shape: s,
                    } => {
                        let mut backing = alloc::vec::Vec::new();

                        for arg in b.clone() {
                            backing.push(Box::new(arg.conversion()(map).0));
                        }

                        Expression::Matrix { backing, shape: *s }
                    }
                    Expression::Negate(e) => {
                        Expression::Negate(Box::new(e.clone().conversion()(map).0))
                    }
                    Expression::Factorial(e) => {
                        Expression::Factorial(Box::new(e.clone().conversion()(map).0))
                    }
                    Expression::Percent(e) => {
                        Expression::Percent(Box::new(e.clone().conversion()(map).0))
                    }
                    Expression::Add(e1, e2) => Expression::Add(
                        Box::new(e1.clone().conversion()(map).0),
                        Box::new(e2.clone().conversion()(map).0),
                    ),
                    Expression::Subtract(e1, e2) => Expression::Subtract(
                        Box::new(e1.clone().conversion()(map).0),
                        Box::new(e2.clone().conversion()(map).0),
                    ),
                    Expression::Multiply(e1, e2) => Expression::Multiply(
                        Box::new(e1.clone().conversion()(map).0),
                        Box::new(e2.clone().conversion()(map).0),
                    ),
                    Expression::Divide(e1, e2) => Expression::Divide(
                        Box::new(e1.clone().conversion()(map).0),
                        Box::new(e2.clone().conversion()(map).0),
                    ),
                    Expression::Power(e1, e2) => Expression::Power(
                        Box::new(e1.clone().conversion()(map).0),
                        Box::new(e2.clone().conversion()(map).0),
                    ),
                    Expression::Modulus(e1, e2) => Expression::Modulus(
                        Box::new(e1.clone().conversion()(map).0),
                        Box::new(e2.clone().conversion()(map).0),
                    ),
                },
                true,
            )
        })
    }

    // uses a template expression to extract the sub-expressions from the given expression
    pub fn extract_arguments(
        &self,
        template: &Expression,
        map: LinearMap<Atom, Expression, 8>,
    ) -> LinearMap<Atom, Expression, 8> {
        match (self, template) {
            (_, Expression::Atom(a)) => match a {
                Atom::Escape(_, _) => {
                    let mut map_n = map;
                    map_n
                        .insert(*a, self.clone())
                        .map_err(|_| "too many escapes")
                        .unwrap();
                    map_n
                }
                _ => map,
            },
            (
                Expression::Function { name: n1, args: a1 },
                Expression::Function { name: n2, args: a2 },
            ) => {
                if n1 == n2 {
                    let mut map_n = map;
                    for (arg1, arg2) in a1.iter().zip(a2.iter()) {
                        map_n = arg1.extract_arguments(arg2, map_n);
                    }
                    map_n
                } else {
                    map
                }
            }
            (
                Expression::Vector {
                    backing: b1,
                    size: s1,
                },
                Expression::Vector {
                    backing: b2,
                    size: s2,
                },
            ) => {
                if s1 == s2 {
                    let mut map_n = map;
                    for (arg1, arg2) in b1.iter().zip(b2.iter()) {
                        map_n = arg1.extract_arguments(arg2, map_n);
                    }
                    map_n
                } else {
                    map
                }
            }
            (
                Expression::Matrix {
                    backing: b1,
                    shape: s1,
                },
                Expression::Matrix {
                    backing: b2,
                    shape: s2,
                },
            ) => {
                if s1 == s2 {
                    let mut map_n = map;
                    for (arg1, arg2) in b1.iter().zip(b2.iter()) {
                        map_n = arg1.extract_arguments(arg2, map_n);
                    }
                    map_n
                } else {
                    map
                }
            }
            (Expression::Negate(e1), Expression::Negate(e2))
            | (Expression::Factorial(e1), Expression::Factorial(e2))
            | (Expression::Percent(e1), Expression::Percent(e2)) => e1.extract_arguments(e2, map),
            (Expression::Add(e11, e12), Expression::Add(e21, e22))
            | (Expression::Subtract(e11, e12), Expression::Subtract(e21, e22))
            | (Expression::Multiply(e11, e12), Expression::Multiply(e21, e22))
            | (Expression::Divide(e11, e12), Expression::Divide(e21, e22))
            | (Expression::Power(e11, e12), Expression::Power(e21, e22))
            | (Expression::Modulus(e11, e12), Expression::Modulus(e21, e22)) => {
                let map_n = e11.extract_arguments(e21, map);
                e12.extract_arguments(e22, map_n)
            }
            _ => map,
        }
    }
}

impl Add for Expression {
    type Output = Expression;

    fn add(self, rhs: Expression) -> Expression {
        Expression::Add(Box::new(self), Box::new(rhs))
    }
}

impl Sub for Expression {
    type Output = Expression;

    fn sub(self, rhs: Expression) -> Expression {
        Expression::Subtract(Box::new(self), Box::new(rhs))
    }
}

impl Mul for Expression {
    type Output = Expression;

    fn mul(self, rhs: Expression) -> Expression {
        Expression::Multiply(Box::new(self), Box::new(rhs))
    }
}

impl Div for Expression {
    type Output = Expression;

    fn div(self, rhs: Expression) -> Expression {
        Expression::Divide(Box::new(self), Box::new(rhs))
    }
}

impl PartialOrd for Expression {
    //escapes are equivalent to their given expression types
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Expression::Atom(_), _) | (_, Expression::Atom(_)) => Some(Ordering::Equal),
            (
                Expression::Function { name: n1, args: a1 },
                Expression::Function { name: n2, args: a2 },
            ) => a1.partial_cmp(a2).and(n1.partial_cmp(n2)),
            (
                Expression::Vector {
                    backing: b1,
                    size: s1,
                },
                Expression::Vector {
                    backing: b2,
                    size: s2,
                },
            ) => b1.partial_cmp(b2).and(s1.partial_cmp(s2)),
            (
                Expression::Matrix {
                    backing: b1,
                    shape: s1,
                },
                Expression::Matrix {
                    backing: b2,
                    shape: s2,
                },
            ) => b1.partial_cmp(b2).and(s1.partial_cmp(s2)),
            (Expression::Negate(e1), Expression::Negate(e2))
            | (Expression::Factorial(e1), Expression::Factorial(e2))
            | (Expression::Percent(e1), Expression::Percent(e2)) => e1.partial_cmp(e2),
            (Expression::Add(a1, a2), Expression::Add(b1, b2))
            | (Expression::Subtract(a1, a2), Expression::Subtract(b1, b2))
            | (Expression::Multiply(a1, a2), Expression::Multiply(b1, b2))
            | (Expression::Divide(a1, a2), Expression::Divide(b1, b2))
            | (Expression::Power(a1, a2), Expression::Power(b1, b2))
            | (Expression::Modulus(a1, a2), Expression::Modulus(b1, b2)) => {
                match a1.partial_cmp(b1) {
                    Some(Ordering::Equal) => a2.partial_cmp(b2),
                    o => o,
                }
            }
            (Expression::Function { name: _, args: _ }, _) => Some(Ordering::Greater),
            (_, Expression::Function { name: _, args: _ }) => Some(Ordering::Less),
            (
                Expression::Vector {
                    backing: _,
                    size: _,
                },
                _,
            ) => Some(Ordering::Greater),
            (
                _,
                Expression::Vector {
                    backing: _,
                    size: _,
                },
            ) => Some(Ordering::Less),
            (
                Expression::Matrix {
                    backing: _,
                    shape: _,
                },
                _,
            ) => Some(Ordering::Greater),
            (
                _,
                Expression::Matrix {
                    backing: _,
                    shape: _,
                },
            ) => Some(Ordering::Less),
            (Expression::Negate(_), _) => Some(Ordering::Greater),
            (_, Expression::Negate(_)) => Some(Ordering::Less),
            (Expression::Factorial(_), _) => Some(Ordering::Greater),
            (_, Expression::Factorial(_)) => Some(Ordering::Less),
            (Expression::Percent(_), _) => Some(Ordering::Greater),
            (_, Expression::Percent(_)) => Some(Ordering::Less),
            (Expression::Add(_, _), _) => Some(Ordering::Greater),
            (_, Expression::Add(_, _)) => Some(Ordering::Less),
            (Expression::Subtract(_, _), _) => Some(Ordering::Greater),
            (_, Expression::Subtract(_, _)) => Some(Ordering::Less),
            (Expression::Multiply(_, _), _) => Some(Ordering::Greater),
            (_, Expression::Multiply(_, _)) => Some(Ordering::Less),
            (Expression::Divide(_, _), _) => Some(Ordering::Greater),
            (_, Expression::Divide(_, _)) => Some(Ordering::Less),
            (Expression::Power(_, _), _) => Some(Ordering::Greater),
            (_, Expression::Power(_, _)) => Some(Ordering::Less),
        }
    }
}

impl fmt::Display for Expression {
    // TODO: smarter parentheses, likely using some kind of traversal?
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Atom(a) => write!(f, "{}", a),

            Expression::Negate(e) => write!(f, "-{}", e),
            Expression::Factorial(e) => write!(f, "{}!", e),
            Expression::Percent(e) => write!(f, "{}%", e),

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

            Expression::Vector {
                backing: vec,
                size: _,
            } => {
                write!(f, "<")?;
                for (i, e) in vec.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ">")
            }

            Expression::Matrix {
                backing: vec,
                shape: (rs, cs),
            } => {
                write!(f, "[")?;
                for r in 0..*rs {
                    if r > 0 {
                        write!(f, "; ")?;
                    }
                    for c in 0..*cs {
                        if c > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", vec[(*cs * r + c) as usize])?;
                    }
                }
                write!(f, "]")
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

    use crate::expression::expression_tree::{Atom, Numeric};

    use super::Expression;

    #[test]
    fn test_numeric_eq() {
        assert_ne!(
            Expression::Atom(Atom::Numeric(Numeric::Integer(1))),
            Expression::Atom(Atom::Numeric(Numeric::Decimal(1.2)))
        );
    }

    #[test]
    fn test_fmt_parse() {
        assert_eq!(
            Expression::from_str("1 * 3").unwrap(),
            Expression::from_str(Expression::from_str("1 * 3").unwrap().to_string().as_str())
                .unwrap()
        );

        assert_eq!(
            Expression::from_str("5 / 6").unwrap(),
            Expression::from_str(Expression::from_str("5 / 6").unwrap().to_string().as_str())
                .unwrap()
        );

        assert_eq!(
            Expression::from_str("5 + 6 + 7 + -8").unwrap(),
            Expression::from_str(
                Expression::from_str("5 + 6 + 7 + -8")
                    .unwrap()
                    .to_string()
                    .as_str()
            )
            .unwrap()
        );

        assert_eq!(
            Expression::from_str("3 + 5 + sin(x) + -6").unwrap(),
            Expression::from_str(
                Expression::from_str("3 + 5 + sin(x) + -6")
                    .unwrap()
                    .to_string()
                    .as_str()
            )
            .unwrap()
        );

        assert_eq!(
            Expression::from_str("1 * 3 + 5 / 6 + sin(x) + -6").unwrap(),
            Expression::from_str(
                Expression::from_str("1 * 3 + 5 / 6 + sin(x) + -6")
                    .unwrap()
                    .to_string()
                    .as_str()
            )
            .unwrap()
        );
    }
}
