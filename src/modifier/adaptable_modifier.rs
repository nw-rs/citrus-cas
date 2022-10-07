use core::{ops::{Add, AddAssign}, fmt};

use alloc::{boxed::Box, vec::Vec, vec};
use heapless::LinearMap;

use crate::expression::expression_tree::{Expression, Atom};

use super::Modifier;

//TODO: create more efficient AdaptableModifier backend
//MultiKeyBinarySearchTree: generic BST struct that has multiple keys per node
struct MultiKeyBinarySearchTree<T, K> where T: PartialOrd + fmt::Display {
    pub value_pairs: Vec<(T, K)>,
    pub left: Option<Box<MultiKeyBinarySearchTree<T, K>>>,
    pub right: Option<Box<MultiKeyBinarySearchTree<T, K>>>,
}

impl<T, K> MultiKeyBinarySearchTree<T, K> where T: PartialOrd + fmt::Display {
    pub fn new(value_pairs: Vec<(T, K)>) -> Self {
        Self {
            value_pairs,
            left: None,
            right: None,
        }
    }

    pub fn insert(&mut self, value_pair: (T, K)) {
        if let Some(value) = self.value_pairs.first() {
            if value_pair.0 < value.0 {
                //println!("left: {}", value_pair.0);
                match &mut self.left {
                    Some(left) => left.insert(value_pair),
                    None => self.left = Some(Box::new(Self::new(vec![value_pair]))),
                }
            } else if value_pair.0 > value.0 {
                //println!("right: {}", value_pair.0);
                match &mut self.right {
                    Some(right) => right.insert(value_pair),
                    None => self.right = Some(Box::new(Self::new(vec![value_pair]))),
                }
            } else {
                //println!("in_exists: {}", value_pair.0);
                self.value_pairs.push(value_pair);
            }
        } else {
            //println!("in_new: {}", value_pair.0);
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

//AdaptableModifier: a modifier whose rules can be added to at runtime
pub struct AdaptableModifier {
    //8 here is a magic number: it's the max number of arguments that can be passed to a function
    search_tree: MultiKeyBinarySearchTree<Expression, Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> (Expression, bool)>>,
}

impl AdaptableModifier {
    //automatically derives an AdaptableModifier from a list of rules in string form
    pub fn from_str_list(rules: Vec<(&str, &str)>) -> Self {
        let mut search_tree = MultiKeyBinarySearchTree::new(Vec::new());
        
        for (key, value) in rules {
            search_tree.insert((key.parse::<Expression>().unwrap(), value.parse::<Expression>().unwrap().conversion()));
        }

        Self {
            search_tree,
        }
    }

    //derives an AdaptableModifier from a list of rules in Expression and Function form
    pub fn from_fn_list(list: Vec<(Expression, Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> (Expression, bool)>)>) -> Self {
        let mut search_tree = MultiKeyBinarySearchTree::new(Vec::new());
        
        for (expression, function) in list {
            search_tree.insert((expression, function));
        }

        Self {
            search_tree,
        }
    }

    pub fn insert_rule(&mut self, expr: Expression, func: Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> (Expression, bool)>) {
        self.search_tree.insert((expr, func));
    }

    //retrieves the rule which is the closest match with the given expression
    pub fn get_rule(&self, expr: &Expression) -> Vec<&(Expression, Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> (Expression, bool)>)> {
        match self.search_tree.get(expr) {
            Some(list) => {
                let (mut level, mut new_list) = (8, Vec::new()); //8 is mod magic here
                list.iter().for_each(|pair| {
                    if let Some(l) = expr.level_eq(&pair.0) {
                        if l.0 == level {
                            new_list.push(pair);
                        } else if l.0 < level {
                            new_list.clear();
                            new_list.push(pair);
                            level = l.0;
                        }
                    }
                });
                new_list
            },
            None => Vec::new(),
        }
    }
}

impl Modifier for AdaptableModifier {
    fn modify(&self, expression: &mut Expression) -> bool {
        let mut modified = false;

        match expression {
            Expression::Atom(_) => {}

            Expression::Function { name: _, args: a } => {
                for expr in a {
                    modified = self.modify(expr) || modified;
                }           
            }
            
            Expression::Negate(e1) |
            Expression::Factorial(e1) |
            Expression::Percent(e1) => modified = self.modify(e1),
            
            Expression::Add(e1, e2) |
            Expression::Subtract(e1, e2) |
            Expression::Multiply(e1, e2) |
            Expression::Divide(e1, e2) |
            Expression::Power(e1, e2) |
            Expression::Modulus(e1, e2) => {
                let m1 = self.modify(e1); 
                let m2 = self.modify(e2);
                modified = m1 || m2;
            } 
        }

        let mut rule_mod = false;
        for rule in self.get_rule(expression) {
            (*expression, rule_mod) = rule.1(&expression.extract_arguments(&rule.0, LinearMap::new()));
            if rule_mod {
                modified = true;
                break;
            }
        }
        
        modified
    }
}

fn get_value_pairs(mkbst: MultiKeyBinarySearchTree<Expression, Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> (Expression, bool)>>) -> Vec<(Expression, Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> (Expression, bool)>)> {
    let mut value_pairs = mkbst.value_pairs;
    
    if let Some(left) = mkbst.left {
        value_pairs.extend(get_value_pairs(*left));
    }
    
    if let Some(right) = mkbst.right {
        value_pairs.extend(get_value_pairs(*right));
    }
    
    value_pairs
}

impl Add for AdaptableModifier {
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        for (expr, func) in get_value_pairs(other.search_tree) {
            self.insert_rule(expr, func);
        }
        
        self
    }
}

impl AddAssign for AdaptableModifier {
    fn add_assign(&mut self, other: Self) {
        for (expr, func) in get_value_pairs(other.search_tree) {
            self.insert_rule(expr, func);
        }
    }
}

impl fmt::Display for MultiKeyBinarySearchTree<Expression, Box<dyn Fn (&LinearMap<Atom, Expression, 8>) -> (Expression, bool)>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (expr, _) in self.value_pairs.iter() {
            write!(f, "{}", expr)?;
        }

        if let Some(left) = &self.left {
            write!(f, "\n..{}", left)?;
        }

        if let Some(right) = &self.right {
            write!(f, "\n.{}", right)?;
        }

        write!(f, "end")
    }
}

impl fmt::Display for AdaptableModifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.search_tree)
    }
}

#[cfg(test)]
mod tests {
    use core::str::FromStr;
    use alloc::{boxed::Box, vec};
    use heapless::LinearMap;

    use crate::expression::expression_tree::{Expression, Atom};
    use super::{Modifier, AdaptableModifier};

    #[test]
    fn test_adaptable_simple() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 - _*2", "_*1 + -_*2")
        ]);

        let mut expr = Expression::from_str("1*2 - 3*4").unwrap();
        let expected_expr = Expression::from_str("1*2 + -(3*4)").unwrap();
        modifier.modify(&mut expr);
        
        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_order() {       
        let modifier2 = AdaptableModifier::from_str_list(vec![
            ("_A1 + _A2", "3"),
            ("_A1 * _A2", "4"),
            ("_F1 + _A1", "1"),
            ("_F1 * _A1", "2"),
        ]);

        let mut expr3 = "sin(x) * 5".parse::<Expression>().unwrap();
        expr3.simplify::<AdaptableModifier, 100>(&modifier2);

        assert_eq!(expr3, "2".parse::<Expression>().unwrap());

        let modifier1 = AdaptableModifier::from_str_list(vec![
            ("_F1 + _A1", "1"),
            ("_F1 * _A1", "2"),
            ("_A1 + _A2", "3"),
            ("_A1 * _A2", "4"),
        ]);

        let mut expr1 = "x + 5".parse::<Expression>().unwrap();
        expr1.simplify::<AdaptableModifier, 100>(&modifier1);

        assert_eq!(expr1, "3".parse::<Expression>().unwrap());

        let mut expr4 = "sin(x) * 5".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&modifier1);

        assert_eq!(expr4, "2".parse::<Expression>().unwrap());
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

        modifier.insert_rule(Expression::from_str("1 + 2").unwrap(), Box::new(|_| (Expression::from_str("3").unwrap(), true)));

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
        expr.simplify::<AdaptableModifier, 50>(&modifier);

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_atom() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 ^ _A1", "log(_A1)"),
        ]);

        let mut expr1 = Expression::from_str("2 ^ 10").unwrap();
        let expected_expr1 = Expression::from_str("log(10)").unwrap();
        expr1.simplify::<AdaptableModifier, 50>(&modifier);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("2 ^ (10 + 5)").unwrap();
        let expected_expr2 = Expression::from_str("2 ^ (10 + 5)").unwrap();
        expr2.simplify::<AdaptableModifier, 50>(&modifier);

        assert_eq!(expr2, expected_expr2);

        let mut expr3 = Expression::from_str("2 ^ x").unwrap();
        let expected_expr3 = Expression::from_str("log(x)").unwrap();
        expr3.simplify::<AdaptableModifier, 50>(&modifier);

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
        expr1.simplify::<AdaptableModifier, 50>(&modifier);

        assert_eq!(expr1, expected_expr1);
    }

    #[test]
    fn test_adaptable_addition() {
        let modifier1 = AdaptableModifier::from_str_list(vec![
            ("_*1 - _*2", "_*1 + -_*2"),
        ]);

        let modifier2 = AdaptableModifier::from_str_list(vec![
            ("_*1 / _*2", "_*1 * (_*2 ^ -1)"),
        ]);

        let modifier3 = modifier1 + modifier2;

        let mut expr1 = Expression::from_str("1 - 2").unwrap();
        let expected_expr1 = Expression::from_str("1 + -2").unwrap();
        modifier3.modify(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("1 / 2").unwrap();
        let expected_expr2 = Expression::from_str("1 * (2 ^ -1)").unwrap();
        modifier3.modify(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_complex() {
        let mut modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 - -_*2", "_*1 + _*2"),
        ]);

        let numeric_add = Box::new(move |map: &LinearMap<Atom, Expression, 8>| {
            match (map.get(&Atom::Escape('A', 1)).unwrap(), map.get(&Atom::Escape('A', 2)).unwrap()) {
                (Expression::Atom(a), Expression::Atom(b)) => match (a, b) {
                    (Atom::Numeric(a), Atom::Numeric(b)) => (Expression::Atom(Atom::Numeric(*a + *b)), true),
                    _ => (Expression::Atom(*a) + Expression::Atom(*b), false)
                }
                _ => unreachable!()
            }
        });

        modifier.insert_rule("_A1 + _A2".parse::<Expression>().unwrap(), numeric_add);

        let mut expr1 = Expression::from_str("1 - -2").unwrap();
        let expected_expr1 = Expression::from_str("3").unwrap();
        expr1.simplify::<AdaptableModifier, 2>(&modifier);

        assert_eq!(expr1, expected_expr1);
    }
}