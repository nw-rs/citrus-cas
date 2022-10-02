use alloc::{boxed::Box, vec::Vec, vec};
use heapless::LinearMap;

use crate::expression_tree::{Expression, Atom};

pub trait Modifier {
    fn modify(&self, expression: &mut Expression) -> bool; //returns true if modified
}

//TODO: destroy
//generic BST struct that has multiple keys
pub struct MultiKeyBinarySearchTree<T, K> where T: PartialOrd {
    pub value_pairs: Vec<(T, K)>,
    pub left: Option<Box<MultiKeyBinarySearchTree<T, K>>>,
    pub right: Option<Box<MultiKeyBinarySearchTree<T, K>>>,
}

impl<T, K> MultiKeyBinarySearchTree<T, K> where T: PartialOrd {
    pub fn new(value_pairs: Vec<(T, K)>) -> Self {
        Self {
            value_pairs/*.sort_by(Expression::hard_order)*/,
            left: None,
            right: None,
        }
    }

    pub fn insert(&mut self, value_pair: (T, K)) {
        if let Some(value) = self.value_pairs.first() {
            if value_pair.0 < self.value_pairs.first().unwrap().0 {
                match &mut self.left {
                    Some(left) => left.insert(value_pair),
                    None => self.left = Some(Box::new(Self::new(vec![value_pair]))),
                }
            } else if value_pair.0 > self.value_pairs.first().unwrap().0 {
                match &mut self.right {
                    Some(right) => right.insert(value_pair),
                    None => self.right = Some(Box::new(Self::new(vec![value_pair]))),
                }
            } else {
                self.value_pairs.push(value_pair);
                //self.keys.sort_by(Expression::hard_order);
            }
        } else {
            self.value_pairs.push(value_pair);
        }
    }

    pub fn get(&self, value: &T) -> Option<&Vec<(T, K)>> {
        if value < &self.value_pairs.first().unwrap().0 {
            match &self.left {
                Some(left) => left.get(value),
                None => None,
            }
        } else if value > &self.value_pairs.first().unwrap().0 {
            match &self.right {
                Some(right) => right.get(value),
                None => None,
            }
        } else {
            Some(&self.value_pairs)
        }
    }
}

//modifier whose rules can be added to at runtime
pub struct AdaptableModifier {
    pub search_tree: MultiKeyBinarySearchTree<Expression, Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> Expression>>,
}

impl AdaptableModifier {
    pub fn from_str_list(rules: Vec<(&str, &str)>) -> Self {
        let mut search_tree = MultiKeyBinarySearchTree::new(Vec::new());
        for (key, value) in rules {
            search_tree.insert((key.parse::<Expression>().unwrap(), value.parse::<Expression>().unwrap().conversion()));
        }
        Self {
            search_tree,
        }
    }
    pub fn from_fn_list(list: Vec<(Expression, Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> Expression>)>) -> Self {
        let mut search_tree = MultiKeyBinarySearchTree::new(Vec::new());
        for (expression, function) in list {
            search_tree.insert((expression, function));
        }
        Self {
            search_tree,
        }
    }
    pub fn insert_rule(&mut self, expr: Expression, func: Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> Expression>) {
        self.search_tree.insert((expr, func));
    }
    pub fn get_rule(&self, expr: &Expression) -> Option<&(Expression, Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> Expression>)> {
        match self.search_tree.get(expr) {
            Some(list) => Some({
                list.iter().reduce(|accum, pair| {
                    if let Some(pair_eq) = expr.level_eq(&pair.0) {
                        if expr.level_eq(&accum.0).unwrap() <= pair_eq {
                            accum
                        } else {
                            pair
                        }
                    }
                    else {
                        accum
                    }
                }).unwrap()
            }),
            None => None,
        }
    }
}

impl Modifier for AdaptableModifier {
    fn modify(&self, expression: &mut Expression) -> bool {
        match expression {
            Expression::Atom(_) => {
                if let Some(rule) = self.get_rule(expression) {
                    *expression = rule.1(&expression.extract_arguments(&rule.0, LinearMap::new()));
                    true
                } else {
                    false
                }
            }
            Expression::Function { name: _, args: a } => {
                let mut modified = false;

                for expr in a {
                    modified = self.modify(expr) || modified;
                }

                if let Some(rule) = self.get_rule(expression) {
                    *expression = rule.1(&expression.extract_arguments(&rule.0, LinearMap::new()));
                    true
                } else {
                    modified
                }
            }
            Expression::Negate(e1) |
            Expression::Factorial(e1) |
            Expression::Percent(e1) => {
                let modified = self.modify(e1);

                if let Some(rule) = self.get_rule(expression) {
                    *expression = rule.1(&expression.extract_arguments(&rule.0, LinearMap::new()));
                    true
                } else {
                    modified
                }
            }
            Expression::Add(e1, e2) |
            Expression::Subtract(e1, e2) |
            Expression::Multiply(e1, e2) |
            Expression::Divide(e1, e2) |
            Expression::Power(e1, e2) |
            Expression::Modulus(e1, e2) => {
                let modified = self.modify(e1) || self.modify(e2);

                if let Some(rule) = self.get_rule(expression) {
                    *expression = rule.1(&expression.extract_arguments(&rule.0, LinearMap::new()));
                    true
                } else {
                    modified
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use core::str::FromStr;

    use alloc::{boxed::Box, vec};
    use heapless::LinearMap;
    use libm::sinf;

    use crate::expression_tree::{Expression, Atom, Numeric};
    use super::{Modifier, AdaptableModifier};

    struct SimpleMod;

    impl Modifier for SimpleMod {
        fn modify(&self, expression: &mut Expression) -> bool {
            match expression {
                Expression::Function { name, args } => {
                    match name.as_str() {
                        "sin" => {
                            *expression = Expression::Atom(
                                Atom::Numeric(
                                    Numeric::Decimal(
                                        sinf(match &*args[0] {
                                            Expression::Atom(a) => match a {
                                                Atom::Numeric(n) => match n {
                                                    Numeric::Decimal(d) => *d,
                                                    _ => unimplemented!(),
                                                },
                                                _ => panic!("sin expects a numeric argument"),
                                            },
                                            _ => return false,
                                        })
                                    )
                                )
                            );
                            true
                        }
                        _ => false,
                    }
                }
                _ => false,
            }
        }
    }

    struct NothingMod;

    impl Modifier for NothingMod {
        fn modify(&self, _expression: &mut Expression) -> bool {
            false
        }
    }

    #[test]
    fn test_modifier_approximate() {
        let expr = Expression::Function {
            name: heapless::String::from_str("sin").unwrap(),
            args: vec![Box::new(Expression::Atom(
                Atom::Numeric(
                    Numeric::Decimal(20.0)
                )
            )),
            ].into_iter().collect(),
        };

        assert_eq!(expr.approximate(&SimpleMod, &NothingMod, &NothingMod), Ok(Numeric::Decimal(0.91294525073)));
    }

    #[test]
    fn test_simple_adaptable() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 - _*2", "_*1 + -_*2")
        ]);

        let mut expr = Expression::from_str("1*2 - 3*4").unwrap();
        let expected_expr = Expression::from_str("1*2 + -(3*4)").unwrap();
        modifier.modify(&mut expr);
        
        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_truth() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 - _*2", "_*1 + -_*2")
        ]);

        let mut expr = Expression::from_str("1 - 2").unwrap();

        assert_eq!(modifier.modify(&mut expr), true);

        let mut expr = Expression::from_str("1 + 2").unwrap();

        assert_eq!(modifier.modify(&mut expr), false);
    }

    #[test]
    fn test_adaptable_insert() {
        let mut modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 - _*2", "_*1 + -_*2")
        ]);

        modifier.insert_rule(Expression::from_str("1 + 2").unwrap(), Box::new(|_| Expression::from_str("3").unwrap()));

        let mut expr = Expression::from_str("1 + 2").unwrap();
        let expected_expr = Expression::from_str("3").unwrap();
        modifier.modify(&mut expr);
        
        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_overlap() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 - _*2", "_*1 + -_*2"),
            ("_*1 - 5", "50")
        ]);

        let mut expr1 = Expression::from_str("1 - 5").unwrap();
        let expected_expr1 = Expression::from_str("50").unwrap();
        modifier.modify(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("1 - 2").unwrap();
        let expected_expr2 = Expression::from_str("1 + -2").unwrap();
        assert_eq!(expr2.level_eq(&"_*1 - 5".parse::<Expression>().unwrap()), None);

        modifier.modify(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_recursive() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 * _*2 + _*3", "_*1 / _*3"),
            ("5 * 6", "30"),
            ("_*1 / _*2", "5"),
            ("_*1 ^ _*2", "6"),
        ]);

        let mut expr = Expression::from_str("(8 * 9 + 5) * (2 ^ 10)").unwrap();
        let expected_expr = Expression::from_str("30").unwrap();
        expr.simplify(&modifier);

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_atom() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 ^ _A1", "log(_A1)"),
        ]);

        let mut expr1 = Expression::from_str("2 ^ 10").unwrap();
        let expected_expr1 = Expression::from_str("log(10)").unwrap();
        expr1.simplify(&modifier);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("2 ^ (10 + 5)").unwrap();
        let expected_expr2 = Expression::from_str("2 ^ (10 + 5)").unwrap();
        expr2.simplify(&modifier);

        assert_eq!(expr2, expected_expr2);

        let mut expr3 = Expression::from_str("2 ^ x").unwrap();
        let expected_expr3 = Expression::from_str("log(x)").unwrap();
        expr3.simplify(&modifier);

        assert_eq!(expr3, expected_expr3);
    }

    #[test]
    fn test_adaptable_function() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_F1 * 15", "cos(25)"),
            ("_F1 + _F2", "_F2 * 15")
        ]);

        let mut expr1 = Expression::from_str("sin(10) + cos(10)").unwrap();
        let expected_expr1 = Expression::from_str("cos(25)").unwrap();
        expr1.simplify(&modifier);

        assert_eq!(expr1, expected_expr1);
    }
}