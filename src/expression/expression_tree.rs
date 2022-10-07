use core::{fmt, str::FromStr, cmp::Ordering, ops::{Add, Sub, Mul, Div}};
use alloc::boxed::Box;

use heapless::{Vec, String, LinearMap,};

use crate::{expression::parser::parse, Error, modifier::Modifier};

//Numeric: representation of any numeric value
#[derive(Debug, Clone, PartialEq, PartialOrd, Copy)]
pub enum Numeric {
    Integer(i32),
    Decimal(f32),
    Fraction(i32, i32), //might be unnecessary?
}

impl Eq for Numeric {}

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
            (Numeric::Integer(a), Numeric::Fraction(b, c)) => {
                Numeric::Fraction((a * c) + b, c)
            }
            (Numeric::Fraction(a, b), Numeric::Integer(c)) => {
                Numeric::Fraction(a + (b * c), b)
            }
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
            (Numeric::Integer(a), Numeric::Fraction(b, c)) => {
                Numeric::Fraction((a * c) - b, c)
            }
            (Numeric::Fraction(a, b), Numeric::Integer(c)) => {
                Numeric::Fraction(a - (b * c), b)
            }
            (Numeric::Decimal(a), Numeric::Fraction(b, c)) => {
                Numeric::Decimal(a / (b as f32 / c as f32))
            }
            (Numeric::Fraction(a, b), Numeric::Decimal(c)) => {
                Numeric::Decimal((a as f32 / b as f32) / c)
            }
        }
    }
}

impl Into<f32> for Numeric {
    fn into(self) -> f32 {
        match self {
            Numeric::Integer(i) => i as f32,
            Numeric::Decimal(d) => d,
            Numeric::Fraction(n, d) => n as f32 / d as f32,
        }
    }
}

impl fmt::Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Numeric::Integer(i) => write!(f, "{}", i),
            Numeric::Decimal(d) => write!(f, "{}", d),
            Numeric::Fraction(r1, r2) => write!(f, "({})/({})", r1, r2), //must always be in the form of (a)/(b), for reinterpretation to work (?)
        }
    }
}

//Atom: the smallest unit of an expression
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Copy)]
pub enum Atom {
    Numeric(Numeric),
    Variable(char),
    Escape(char, u8), //escapes indicate where something in an expression should be replaced: A for atoms, F for functions, and * for everything
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

//Expression: a tree representing a mathematical expression
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    //atoms
    Atom(Atom),

    //unary operators
    Negate(Box<Self>),
    Factorial(Box<Self>),
    Percent(Box<Self>),

    //binary operators
    Add(Box<Self>, Box<Self>),
    Subtract(Box<Self>, Box<Self>),
    Multiply(Box<Self>, Box<Self>),
    Divide(Box<Self>, Box<Self>),
    Power(Box<Self>, Box<Self>),
    Modulus(Box<Self>, Box<Self>),

    //n-ary operators
    Function { //TODO: make smaller somehow?
        name: String<8>,
        args: Vec<Box<Self>, 8>,
    },
}

impl Expression {
    //reorganizes the expression tree using the given modifier a max of L times
    pub fn simplify<S: Modifier, const L: usize>(&mut self, simplifier: &S) {
        for _ in 0..L {
            if !simplifier.modify(self) {
                break;
            }
        }
    }

    //simplifies, then uses the evaluation modifier on the tree a max of L times
    pub fn evaluate<E: Modifier, S: Modifier, const L: usize>(&self, evaluator: &E, simplifier: &S) -> Expression {
        let mut expr = self.clone();

        for _ in 0..L {
            expr.simplify::<S, L>(simplifier);
            if !evaluator.modify(&mut expr) {
                break;
            }
        }

        expr
    }

    //evaluates, then uses the approximation modifier on the tree a max of L times
    pub fn approximate<A: Modifier, E: Modifier, S: Modifier, const L: usize>(&self, approximator: &A, evaluator: &E, simplifier: &S) -> Result<Numeric, ()> {
        let mut expr = self.clone();

        for _ in 0..L {
            expr = expr.evaluate::<E, S, L>(evaluator, simplifier);
            if !approximator.modify(&mut expr) {
                break;
            }
        }

        match expr {
            Expression::Atom(a) => match a {
                Atom::Numeric(n) => Ok(n),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    //returns the number of escapes in the other expression, or None if the expressions are not equal
    pub fn level_eq(&self, other: &Self) -> Option<(u8, LinearMap<Atom, Expression, 8>)> {
        match (self, other) {
            (e, Expression::Atom(a)) => match a {
                Atom::Escape(escape, _) => match escape {
                    'A' => match e {
                        Expression::Atom(a) => {
                            let mut map = LinearMap::new();
                            map.insert(a.clone(), e.clone());
                            Some((1, map))
                        },
                        _ => None,
                    }
                    'F' => match e {
                        Expression::Function { name: _, args: _ } => {
                            let mut map = LinearMap::new();
                            map.insert(a.clone(), e.clone());
                            Some((1, map))
                        },
                        _ => None,
                    }
                    '*' => {
                        let mut map = LinearMap::new();
                        map.insert(a.clone(), e.clone());
                        Some((1, map))
                    },
                    _ => unimplemented!(),
                }
                _ => if self == other { Some((0, LinearMap::new())) } else { None },
            },
            (Expression::Function { name: n1, args: a1 }, Expression::Function { name: n2, args: a2 }) => {
                if n1 == n2 {
                    let mut level = 0;
                    let mut map = LinearMap::new();

                    for (arg1, arg2) in a1.iter().zip(a2.iter()) {
                        match arg1.level_eq(arg2) {
                            Some(l) => {
                                level += l.0;
                                l.1.iter().for_each(|(k, v)| {
                                    if let Some(value) = map.get(k) {
                                        if value != v {
                                            level=u8::MAX;
                                        }
                                    } else {
                                        map.insert(k.clone(), v.clone());
                                    }
                                });
                                if level == u8::MAX {
                                    return None;
                                }
                            },
                            None => return None,
                        }
                    }

                    Some((level, map))
                } else {
                    None
                }
            }
            (Expression::Negate(e1), Expression::Negate(e2)) |
            (Expression::Factorial(e1), Expression::Factorial(e2)) |
            (Expression::Percent(e1), Expression::Percent(e2)) => {
                e1.level_eq(e2)
            },
            (Expression::Add(e11, e12), Expression::Add(e21, e22)) |
            (Expression::Subtract(e11, e12), Expression::Subtract(e21, e22)) |
            (Expression::Multiply(e11, e12), Expression::Multiply(e21, e22)) |
            (Expression::Divide(e11, e12), Expression::Divide(e21, e22)) |
            (Expression::Power(e11, e12), Expression::Power(e21, e22)) |
            (Expression::Modulus(e11, e12), Expression::Modulus(e21, e22)) => {
                let level1 = e11.level_eq(e21);
                let level2 = e12.level_eq(e22);
                match (level1, level2) {
                    (Some(l1), Some(l2)) => {
                        let mut level = l1.0 + l2.0;
                        let mut map = l1.1;
                        l2.1.iter().for_each(|(k, v)| {
                            if let Some(value) = map.get(k) {
                                if value != v {
                                    level=u8::MAX;
                                }
                            } else {
                                map.insert(k.clone(), v.clone());
                            }
                        });
                        if level == u8::MAX {
                            None
                        } else {
                            Some((level, map))
                        }
                    },
                    _ => None,
                }
            }
            _ => None,
        }
    }

    //extracts a modification function from the given expression
    pub fn conversion(self) -> Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> (Expression, bool)> {
        Box::new(move |map: &LinearMap<Atom, Expression, 8>| {
            (match &self {
                Expression::Atom(a) => match a {
                    Atom::Escape(_, _) => {
                         map.get(&a).unwrap().clone()
                    }
                    _ => {
                        self.clone()
                    }
                }
                Expression::Function { name: n1, args: a1 } => {
                    let mut args = Vec::new();

                    for arg in a1.clone() {
                        args.push(Box::new(arg.conversion()(&map).0)).unwrap(); //vec overflow is impossible here
                    }
                    
                    Expression::Function { name: n1.clone(), args }
                }
                Expression::Negate(e) => {
                    Expression::Negate(Box::new(e.clone().conversion()(&map).0))
                }
                Expression::Factorial(e) => {
                    Expression::Factorial(Box::new(e.clone().conversion()(&map).0))
                }
                Expression::Percent(e) => {
                    Expression::Percent(Box::new(e.clone().conversion()(&map).0))
                }
                Expression::Add(e1, e2) => {
                    Expression::Add(Box::new(e1.clone().conversion()(&map).0), Box::new(e2.clone().conversion()(&map).0))
                }
                Expression::Subtract(e1, e2) => {
                    Expression::Subtract(Box::new(e1.clone().conversion()(&map).0), Box::new(e2.clone().conversion()(&map).0))
                }
                Expression::Multiply(e1, e2) => {
                    Expression::Multiply(Box::new(e1.clone().conversion()(&map).0), Box::new(e2.clone().conversion()(&map).0))
                }
                Expression::Divide(e1, e2) => {
                    Expression::Divide(Box::new(e1.clone().conversion()(&map).0), Box::new(e2.clone().conversion()(&map).0))
                }
                Expression::Power(e1, e2) => {
                    Expression::Power(Box::new(e1.clone().conversion()(&map).0), Box::new(e2.clone().conversion()(&map).0))
                }
                Expression::Modulus(e1, e2) => {
                    Expression::Modulus(Box::new(e1.clone().conversion()(&map).0), Box::new(e2.clone().conversion()(&map).0))
                }
            }, true)},
        )
    }

    //uses a template expression to extract the sub-expressions from the given expression
    pub fn extract_arguments(&self, template: &Expression, map: LinearMap<Atom, Expression, 8>) -> LinearMap<Atom, Expression, 8> {
        match (self, template) {
            (_, Expression::Atom(a)) => {
                match a {
                    Atom::Escape(_, _) => {
                        let mut map = map;
                        map.insert(a.clone(), self.clone()).map_err(|_| "too many arguments").unwrap();
                        map
                    }
                    _ => map
                }
            }
            (Expression::Function { name: n1, args: a1 }, Expression::Function { name: n2, args: a2 }) => {
                if n1 == n2 {
                    let mut map = map;
                    for (arg1, arg2) in a1.iter().zip(a2.iter()) {
                        map = arg1.extract_arguments(arg2, map);
                    }
                    map
                } else {
                    map
                }
            }
            (Expression::Negate(e1), Expression::Negate(e2)) |
            (Expression::Factorial(e1), Expression::Factorial(e2)) |
            (Expression::Percent(e1), Expression::Percent(e2)) => {
                e1.extract_arguments(e2, map)
            },
            (Expression::Add(e11, e12), Expression::Add(e21, e22)) |
            (Expression::Subtract(e11, e12), Expression::Subtract(e21, e22)) |
            (Expression::Multiply(e11, e12), Expression::Multiply(e21, e22)) |
            (Expression::Divide(e11, e12), Expression::Divide(e21, e22)) |
            (Expression::Power(e11, e12), Expression::Power(e21, e22)) |
            (Expression::Modulus(e11, e12), Expression::Modulus(e21, e22)) => {
                let map = e11.extract_arguments(e21, map);
                e12.extract_arguments(e22, map)
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
            (Expression::Atom(a), e) | (e, Expression::Atom(a)) => Some(Ordering::Equal),
            (Expression::Function { name: n1, args: a1 }, Expression::Function { name: n2, args: a2 }) => a1.partial_cmp(a2).and(n1.partial_cmp(n2)),
            (Expression::Negate(e1), Expression::Negate(e2)) |
            (Expression::Factorial(e1), Expression::Factorial(e2)) |
            (Expression::Percent(e1), Expression::Percent(e2)) => {
                e1.partial_cmp(e2)
            }
            (Expression::Add(a1, a2), Expression::Add(b1, b2)) |
            (Expression::Subtract(a1, a2), Expression::Subtract(b1, b2)) |
            (Expression::Multiply(a1, a2), Expression::Multiply(b1, b2)) |
            (Expression::Divide(a1, a2), Expression::Divide(b1, b2)) |
            (Expression::Power(a1, a2), Expression::Power(b1, b2)) |
            (Expression::Modulus(a1, a2), Expression::Modulus(b1, b2)) => {
                match a1.partial_cmp(b1) {
                    Some(Ordering::Equal) => a2.partial_cmp(b2),
                    Some(Ordering::Less) => Some(Ordering::Less),
                    Some(Ordering::Greater) => Some(Ordering::Greater),
                    None => None,
                }
            }
            (Expression::Function { name: _, args: _ }, _) => Some(Ordering::Greater),
            (_, Expression::Function { name: _, args: _ }) => Some(Ordering::Less),
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

    use crate::expression::expression_tree::{Atom, Numeric};

    use super::Expression;

    #[test]
    fn test_numeric_eq() {
        assert_ne!(Expression::Atom(Atom::Numeric(Numeric::Integer(1))), Expression::Atom(Atom::Numeric(Numeric::Decimal(1.2))));
    }

    #[test]
    fn test_fmt_parse() {
        //TODO: fix Expression to_string()
        assert_eq!(Expression::from_str("1 * 3 + 5 / 6 + sin(x) + -6").unwrap(),
            Expression::from_str(Expression::from_str("1 * 3 + 5 / 6 + sin(x) + -6").unwrap().to_string().as_str()).unwrap()
        )
    }
}