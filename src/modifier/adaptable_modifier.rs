use core::{
    fmt,
    hash::BuildHasher,
    ops::{Add, AddAssign},
};

use alloc::{boxed::Box, vec, vec::Vec};
use heapless::LinearMap;
use indexmap::IndexMap;

use crate::expression::expression_tree::{Atom, Expression};

use super::{ModifierImmutable, ModifierMutable};

// TODO: create more efficient AdaptableModifier backend
// MultiKeyBinarySearchTree: generic BST struct that has multiple keys per node
struct MultiKeyBinarySearchTree<T, K>
where
    T: PartialOrd + fmt::Display,
{
    pub value_pairs: Vec<(T, K)>,
    pub left: Option<Box<MultiKeyBinarySearchTree<T, K>>>,
    pub right: Option<Box<MultiKeyBinarySearchTree<T, K>>>,
}

impl<T, K> MultiKeyBinarySearchTree<T, K>
where
    T: PartialOrd + fmt::Display,
{
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
                match &mut self.left {
                    Some(left) => left.insert(value_pair),
                    None => self.left = Some(Box::new(Self::new(vec![value_pair]))),
                }
            } else if value_pair.0 > value.0 {
                match &mut self.right {
                    Some(right) => right.insert(value_pair),
                    None => self.right = Some(Box::new(Self::new(vec![value_pair]))),
                }
            } else {
                self.value_pairs.push(value_pair);
            }
        } else {
            self.value_pairs.push(value_pair);
        }
    }

    pub fn get(&self, value: &T) -> Option<&Vec<(T, K)>> {
        if let Some(v) = self.value_pairs.first() {
            if value < &v.0 {
                match &self.left {
                    Some(left) => left.get(value),
                    None => None,
                }
            } else if value > &v.0 {
                match &self.right {
                    Some(right) => right.get(value),
                    None => None,
                }
            } else {
                Some(&self.value_pairs)
            }
        } else {
            None
        }
    }
}

// ModifierFunction: a function pointer that generates a new expression using an escape arugment map
// 8 here is a magic number: it's the max number of arguments that can be passed to a function
pub type ModifierFunction = Box<dyn Fn(&LinearMap<Atom, Expression, 8>) -> (Expression, bool)>;

// AdaptableModifier: a modifier whose rules can be added to at runtime
pub struct AdaptableModifier {
    search_tree: MultiKeyBinarySearchTree<Expression, ModifierFunction>,
}

impl AdaptableModifier {
    // automatically derives an AdaptableModifier from a list of rules in string form
    pub fn from_str_list(rules: Vec<(&str, &str)>) -> Self {
        let mut search_tree = MultiKeyBinarySearchTree::new(Vec::new());

        for (key, value) in rules {
            search_tree.insert((
                key.parse::<Expression>().unwrap(),
                value.parse::<Expression>().unwrap().conversion(),
            ));
        }

        Self { search_tree }
    }

    // derives an AdaptableModifier from a list of rules in Expression and Function form
    pub fn from_fn_list(list: Vec<(Expression, ModifierFunction)>) -> Self {
        let mut search_tree = MultiKeyBinarySearchTree::new(Vec::new());

        for (expression, function) in list {
            search_tree.insert((expression, function));
        }

        Self { search_tree }
    }

    pub fn insert_rule(&mut self, expr: Expression, func: ModifierFunction) {
        self.search_tree.insert((expr, func));
    }

    // retrieves the rule which is the closest match with the given expression
    fn get_rule(&self, expr: &Expression) -> Vec<&(Expression, ModifierFunction)> {
        match self.search_tree.get(expr) {
            Some(list) => {
                let (mut level, mut new_list) = (8, Vec::new()); // 8 is mod magic here
                list.iter().for_each(|pair| {
                    if let Some(l) = expr.level_eq(&pair.0, &mut LinearMap::new()) {
                        match l.cmp(&level) {
                            core::cmp::Ordering::Less => {
                                level = l;
                                new_list = vec![pair];
                            }
                            core::cmp::Ordering::Equal => new_list.push(pair),
                            _ => (),
                        }
                    }
                });
                new_list
            }
            None => Vec::new(),
        }
    }
}

impl ModifierImmutable for AdaptableModifier {
    fn modify_immut(&self, expression: &mut Expression) -> bool {
        let mut modified = false;

        match expression {
            Expression::Atom(_) => {}

            Expression::Vector {
                backing: vec,
                size: _,
            } => {
                for e in vec {
                    modified = self.modify_immut(e) || modified;
                }
            }
            Expression::Matrix {
                backing: vec,
                shape: (_, _),
            } => {
                for e in vec {
                    modified = self.modify_immut(e) || modified;
                }
            }

            Expression::Function { name: _, args: a } => {
                for expr in a {
                    modified = self.modify_immut(expr) || modified;
                }
            }

            Expression::Negate(e1) | Expression::Factorial(e1) | Expression::Percent(e1) => {
                modified = self.modify_immut(e1)
            }

            Expression::Add(e1, e2)
            | Expression::Subtract(e1, e2)
            | Expression::Multiply(e1, e2)
            | Expression::Divide(e1, e2)
            | Expression::Power(e1, e2)
            | Expression::Modulus(e1, e2) => {
                let m1 = self.modify_immut(e1);
                let m2 = self.modify_immut(e2);
                modified = m1 || m2;
            }
        }

        let mut rule_mod;
        for rule in self.get_rule(expression) {
            (*expression, rule_mod) =
                rule.1(&expression.extract_arguments(&rule.0, LinearMap::new()));
            if rule_mod {
                modified = true;
                break;
            }
        }

        modified
    }
}

impl ModifierMutable for AdaptableModifier {
    fn modify_mut(&mut self, expression: &mut Expression) -> bool {
        let mut modified = false;

        match expression {
            Expression::Atom(_) => {}

            Expression::Vector {
                backing: vec,
                size: _,
            } => {
                for e in vec {
                    modified = self.modify_mut(e) || modified;
                }
            }
            Expression::Matrix {
                backing: vec,
                shape: (_, _),
            } => {
                for e in vec {
                    modified = self.modify_mut(e) || modified;
                }
            }

            Expression::Function { name: _, args: a } => {
                for expr in a {
                    modified = self.modify_mut(expr) || modified;
                }
            }

            Expression::Negate(e1) | Expression::Factorial(e1) | Expression::Percent(e1) => {
                modified = self.modify_mut(e1)
            }

            Expression::Add(e1, e2)
            | Expression::Subtract(e1, e2)
            | Expression::Multiply(e1, e2)
            | Expression::Divide(e1, e2)
            | Expression::Power(e1, e2)
            | Expression::Modulus(e1, e2) => {
                let m1 = self.modify_mut(e1);
                let m2 = self.modify_mut(e2);
                modified = m1 || m2;
            }
        }

        let mut rule_mod;
        for rule in self.get_rule(expression) {
            (*expression, rule_mod) =
                rule.1(&expression.extract_arguments(&rule.0, LinearMap::new()));
            if rule_mod {
                modified = true;
                break;
            }
        }

        modified
    }
}

fn get_value_pairs(
    mkbst: MultiKeyBinarySearchTree<Expression, ModifierFunction>,
) -> Vec<(Expression, ModifierFunction)> {
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

impl fmt::Display for MultiKeyBinarySearchTree<Expression, ModifierFunction> {
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

// CachingAdaptableModifier: an AdaptableModifier that caches the results of its modifications
pub struct CachingAdaptableModifier<S>
where
    S: Default + BuildHasher,
{
    pub modifier: AdaptableModifier,
    // because modificiation is recursive and repetitive, we memoize the result of modification, to skip false results
    cache: IndexMap<Expression, bool, S>,
    // if memoization_limit is Some(n), then the memoizing_map will be halfed after n insertions
    limit: Option<usize>,
}

impl<S> CachingAdaptableModifier<S>
where
    S: Default + BuildHasher,
{
    pub fn from_str_list(rules: Vec<(&str, &str)>, limit: Option<usize>) -> Self {
        let modifier = AdaptableModifier::from_str_list(rules);

        Self {
            modifier,
            cache: IndexMap::with_hasher(S::default()),
            limit,
        }
    }

    pub fn from_fn_list(rules: Vec<(Expression, ModifierFunction)>, limit: Option<usize>) -> Self {
        let modifier = AdaptableModifier::from_fn_list(rules);

        Self {
            modifier,
            cache: IndexMap::with_hasher(S::default()),
            limit,
        }
    }

    pub fn insert_rule(&mut self, expr: Expression, func: ModifierFunction) {
        self.modifier.insert_rule(expr, func);
        self.cache.clear();
    }
}

impl<S> ModifierMutable for CachingAdaptableModifier<S>
where
    S: Default + BuildHasher,
{
    fn modify_mut(&mut self, expression: &mut Expression) -> bool {
        if self.cache.get(expression).is_some() {
            false
        } else {
            let mem_save = expression.clone();

            let modified = self.modifier.modify_immut(expression);

            if !modified {
                if let Some(limit) = self.limit {
                    if self.cache.len() >= limit {
                        self.cache.drain(limit / 2..limit);
                    }
                }

                self.cache.insert(mem_save, false);
            }

            modified
        }
    }
}

impl<S> Add<AdaptableModifier> for CachingAdaptableModifier<S>
where
    S: Default + BuildHasher,
{
    type Output = Self;

    fn add(mut self, other: AdaptableModifier) -> Self {
        self.modifier += other;

        self
    }
}

impl<S> AddAssign<AdaptableModifier> for CachingAdaptableModifier<S>
where
    S: Default + BuildHasher,
{
    fn add_assign(&mut self, other: AdaptableModifier) {
        self.modifier += other;
    }
}

impl<S> Add for CachingAdaptableModifier<S>
where
    S: Default + BuildHasher,
{
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        self.modifier += other.modifier;

        self
    }
}

impl<S> AddAssign for CachingAdaptableModifier<S>
where
    S: Default + BuildHasher,
{
    fn add_assign(&mut self, other: Self) {
        self.modifier += other.modifier;
    }
}

#[cfg(test)]
mod tests {
    use alloc::{boxed::Box, vec};
    use core::str::FromStr;
    use heapless::LinearMap;

    use super::{AdaptableModifier, ModifierImmutable};
    use crate::{
        expression::expression_tree::{Atom, Expression},
        modifier::{adaptable_modifier::CachingAdaptableModifier, ModifierMutable},
    };

    #[test]
    fn test_adaptable_simple() {
        let modifier = AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2")]);

        let mut expr = Expression::from_str("1*2 - 3*4").unwrap();
        let expected_expr = Expression::from_str("1*2 + -(3*4)").unwrap();
        modifier.modify_immut(&mut expr);

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_order() {
        let mut modifier2 = AdaptableModifier::from_str_list(vec![
            ("_A1 + _A2", "3"),
            ("_A1 * _A2", "4"),
            ("_F1 + _A1", "1"),
            ("_F1 * _A1", "2"),
        ]);

        let mut expr3 = "sin(x) * 5".parse::<Expression>().unwrap();
        expr3.simplify_im::<AdaptableModifier, 100>(&mut modifier2);

        assert_eq!(expr3, "2".parse::<Expression>().unwrap());

        let mut modifier1 = AdaptableModifier::from_str_list(vec![
            ("_F1 + _A1", "1"),
            ("_F1 * _A1", "2"),
            ("_A1 + _A2", "3"),
            ("_A1 * _A2", "4"),
        ]);

        let mut expr1 = "x + 5".parse::<Expression>().unwrap();
        expr1.simplify_im::<AdaptableModifier, 100>(&mut modifier1);

        assert_eq!(expr1, "3".parse::<Expression>().unwrap());

        let mut expr4 = "sin(x) * 5".parse::<Expression>().unwrap();
        expr4.simplify_im::<AdaptableModifier, 100>(&mut modifier1);

        assert_eq!(expr4, "2".parse::<Expression>().unwrap());
    }

    #[test]
    fn test_adaptable_truth() {
        let modifier = AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2")]);

        let mut expr = Expression::from_str("1 - 2").unwrap();

        assert_eq!(modifier.modify_immut(&mut expr), true);

        let mut expr = Expression::from_str("1 + 2").unwrap();

        assert_eq!(modifier.modify_immut(&mut expr), false);
    }

    #[test]
    fn test_adaptable_insert() {
        let mut modifier = AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2")]);

        modifier.insert_rule(
            Expression::from_str("1 + 2").unwrap(),
            Box::new(|_| (Expression::from_str("3").unwrap(), true)),
        );

        let mut expr = Expression::from_str("1 + 2").unwrap();
        let expected_expr = Expression::from_str("3").unwrap();
        modifier.modify_immut(&mut expr);

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_overlap() {
        let modifier =
            AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2"), ("_*1 - 5", "50")]);

        let mut expr1 = Expression::from_str("1 - 5").unwrap();
        let expected_expr1 = Expression::from_str("50").unwrap();
        modifier.modify_immut(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("1 - 2").unwrap();
        let expected_expr2 = Expression::from_str("1 + -2").unwrap();

        modifier.modify_immut(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_recursive() {
        let mut modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 * _*2 + _*3", "_*1 / _*3"),
            ("5 * 6", "30"),
            ("_*1 / _*2", "5"),
            ("_*1 ^ _*2", "6"),
        ]);

        let mut expr = Expression::from_str("(8 * 9 + 5) * (2 ^ 10)").unwrap();
        let expected_expr = Expression::from_str("30").unwrap();
        expr.simplify_im::<AdaptableModifier, 50>(&mut modifier);

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_atom() {
        let mut modifier = AdaptableModifier::from_str_list(vec![("_*1 ^ _A1", "log(_A1)")]);

        let mut expr1 = Expression::from_str("2 ^ 10").unwrap();
        let expected_expr1 = Expression::from_str("log(10)").unwrap();
        expr1.simplify_im::<AdaptableModifier, 50>(&mut modifier);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("2 ^ (10 + 5)").unwrap();
        let expected_expr2 = Expression::from_str("2 ^ (10 + 5)").unwrap();
        expr2.simplify_im::<AdaptableModifier, 50>(&mut modifier);

        assert_eq!(expr2, expected_expr2);

        let mut expr3 = Expression::from_str("2 ^ x").unwrap();
        let expected_expr3 = Expression::from_str("log(x)").unwrap();
        expr3.simplify_im::<AdaptableModifier, 50>(&mut modifier);

        assert_eq!(expr3, expected_expr3);
    }

    #[test]
    fn test_adaptable_function() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_F1 * 15", "cos(25)"),
            ("_F1 + _F2", "_F2 * 15"),
        ]);

        let mut expr1 = Expression::from_str("sin(10) + cos(10)").unwrap();
        let expected_expr1 = Expression::from_str("cos(25)").unwrap();
        expr1.simplify_im::<AdaptableModifier, 50>(&modifier);

        assert_eq!(expr1, expected_expr1);
    }

    #[test]
    fn test_adaptable_addition() {
        let modifier1 = AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2")]);

        let modifier2 = AdaptableModifier::from_str_list(vec![("_*1 / _*2", "_*1 * (_*2 ^ -1)")]);

        let modifier3 = modifier1 + modifier2;

        let mut expr1 = Expression::from_str("1 - 2").unwrap();
        let expected_expr1 = Expression::from_str("1 + -2").unwrap();
        modifier3.modify_immut(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("1 / 2").unwrap();
        let expected_expr2 = Expression::from_str("1 * (2 ^ -1)").unwrap();
        modifier3.modify_immut(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_complex() {
        let mut modifier = AdaptableModifier::from_str_list(vec![("_*1 - -_*2", "_*1 + _*2")]);

        let numeric_add = Box::new(move |map: &LinearMap<Atom, Expression, 8>| {
            match (
                map.get(&Atom::Escape('A', 1)).unwrap(),
                map.get(&Atom::Escape('A', 2)).unwrap(),
            ) {
                (Expression::Atom(a), Expression::Atom(b)) => match (a, b) {
                    (Atom::Numeric(a), Atom::Numeric(b)) => {
                        (Expression::Atom(Atom::Numeric(*a + *b)), true)
                    }
                    _ => (Expression::Atom(*a) + Expression::Atom(*b), false),
                },
                _ => unreachable!(),
            }
        });

        modifier.insert_rule("_A1 + _A2".parse::<Expression>().unwrap(), numeric_add);

        let mut expr1 = Expression::from_str("1 - -2").unwrap();
        let expected_expr1 = Expression::from_str("3").unwrap();
        expr1.simplify_im::<AdaptableModifier, 2>(&modifier);

        assert_eq!(expr1, expected_expr1);
    }

    #[test]
    fn test_adaptable_vector() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_V1 - _V2", "_V1 + -_V2"),
            ("_V1 - <1, 2>", "50"),
        ]);

        let mut expr1 = Expression::from_str("<1, 2, 3, 4> - <1, 5, 7>").unwrap();
        let expected_expr1 = Expression::from_str("<1, 2, 3, 4> + -<1, 5, 7>").unwrap();
        modifier.modify_immut(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("<1, 2, 3, 4> - <1, 2>").unwrap();
        let expected_expr2 = Expression::from_str("50").unwrap();
        modifier.modify_immut(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_matrix() {
        let modifier = AdaptableModifier::from_str_list(vec![
            ("_M1 - _M2", "_M1 + -_M2"),
            ("_M1 - [1; 3]", "50"),
        ]);

        let mut expr1 = Expression::from_str("[1, 2; 3, 4] - [1, 5; 6, 7]").unwrap();
        let expected_expr1 = Expression::from_str("[1, 2; 3, 4] + -[1, 5; 6, 7]").unwrap();
        modifier.modify_immut(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("[1, 2; 3, 4] - [1; 3]").unwrap();
        let expected_expr2 = Expression::from_str("50").unwrap();
        modifier.modify_immut(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_simple_mutable() {
        let mut modifier = AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2")]);

        let mut expr = Expression::from_str("1*2 - 3*4").unwrap();
        let expected_expr = Expression::from_str("1*2 + -(3*4)").unwrap();
        modifier.modify_mut(&mut expr);

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_order_mutable() {
        let mut modifier2 = AdaptableModifier::from_str_list(vec![
            ("_A1 + _A2", "3"),
            ("_A1 * _A2", "4"),
            ("_F1 + _A1", "1"),
            ("_F1 * _A1", "2"),
        ]);

        let mut expr3 = "sin(x) * 5".parse::<Expression>().unwrap();
        expr3.simplify::<AdaptableModifier, 100>(&mut modifier2);

        assert_eq!(expr3, "2".parse::<Expression>().unwrap());

        let mut modifier1 = AdaptableModifier::from_str_list(vec![
            ("_F1 + _A1", "1"),
            ("_F1 * _A1", "2"),
            ("_A1 + _A2", "3"),
            ("_A1 * _A2", "4"),
        ]);

        let mut expr1 = "x + 5".parse::<Expression>().unwrap();
        expr1.simplify::<AdaptableModifier, 100>(&mut modifier1);

        assert_eq!(expr1, "3".parse::<Expression>().unwrap());

        let mut expr4 = "sin(x) * 5".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&mut modifier1);

        assert_eq!(expr4, "2".parse::<Expression>().unwrap());
    }

    #[test]
    fn test_adaptable_truth_mutable() {
        let mut modifier = AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2")]);

        let mut expr = Expression::from_str("1 - 2").unwrap();

        assert_eq!(modifier.modify_mut(&mut expr), true);

        let mut expr = Expression::from_str("1 + 2").unwrap();

        assert_eq!(modifier.modify_mut(&mut expr), false);
    }

    #[test]
    fn test_adaptable_insert_mutable() {
        let mut modifier = AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2")]);

        modifier.insert_rule(
            Expression::from_str("1 + 2").unwrap(),
            Box::new(|_| (Expression::from_str("3").unwrap(), true)),
        );

        let mut expr = Expression::from_str("1 + 2").unwrap();
        let expected_expr = Expression::from_str("3").unwrap();
        modifier.modify_mut(&mut expr);

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_overlap_mutable() {
        let mut modifier =
            AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2"), ("_*1 - 5", "50")]);

        let mut expr1 = Expression::from_str("1 - 5").unwrap();
        let expected_expr1 = Expression::from_str("50").unwrap();
        modifier.modify_mut(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("1 - 2").unwrap();
        let expected_expr2 = Expression::from_str("1 + -2").unwrap();

        modifier.modify_mut(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_recursive_mutable() {
        let mut modifier = AdaptableModifier::from_str_list(vec![
            ("_*1 * _*2 + _*3", "_*1 / _*3"),
            ("5 * 6", "30"),
            ("_*1 / _*2", "5"),
            ("_*1 ^ _*2", "6"),
        ]);

        let mut expr = Expression::from_str("(8 * 9 + 5) * (2 ^ 10)").unwrap();
        let expected_expr = Expression::from_str("30").unwrap();
        expr.simplify::<AdaptableModifier, 50>(&mut modifier);

        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn test_adaptable_atom_mutable() {
        let mut modifier = AdaptableModifier::from_str_list(vec![("_*1 ^ _A1", "log(_A1)")]);

        let mut expr1 = Expression::from_str("2 ^ 10").unwrap();
        let expected_expr1 = Expression::from_str("log(10)").unwrap();
        expr1.simplify::<AdaptableModifier, 50>(&mut modifier);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("2 ^ (10 + 5)").unwrap();
        let expected_expr2 = Expression::from_str("2 ^ (10 + 5)").unwrap();
        expr2.simplify::<AdaptableModifier, 50>(&mut modifier);

        assert_eq!(expr2, expected_expr2);

        let mut expr3 = Expression::from_str("2 ^ x").unwrap();
        let expected_expr3 = Expression::from_str("log(x)").unwrap();
        expr3.simplify::<AdaptableModifier, 50>(&mut modifier);

        assert_eq!(expr3, expected_expr3);
    }

    #[test]
    fn test_adaptable_function_mutable() {
        let mut modifier = AdaptableModifier::from_str_list(vec![
            ("_F1 * 15", "cos(25)"),
            ("_F1 + _F2", "_F2 * 15"),
        ]);

        let mut expr1 = Expression::from_str("sin(10) + cos(10)").unwrap();
        let expected_expr1 = Expression::from_str("cos(25)").unwrap();
        expr1.simplify::<AdaptableModifier, 50>(&mut modifier);

        assert_eq!(expr1, expected_expr1);
    }

    #[test]
    fn test_adaptable_addition_mutable() {
        let modifier1 = AdaptableModifier::from_str_list(vec![("_*1 - _*2", "_*1 + -_*2")]);

        let modifier2 = AdaptableModifier::from_str_list(vec![("_*1 / _*2", "_*1 * (_*2 ^ -1)")]);

        let mut modifier3 = modifier1 + modifier2;

        let mut expr1 = Expression::from_str("1 - 2").unwrap();
        let expected_expr1 = Expression::from_str("1 + -2").unwrap();
        modifier3.modify_mut(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("1 / 2").unwrap();
        let expected_expr2 = Expression::from_str("1 * (2 ^ -1)").unwrap();
        modifier3.modify_mut(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_complex_mutable() {
        let mut modifier = AdaptableModifier::from_str_list(vec![("_*1 - -_*2", "_*1 + _*2")]);

        let numeric_add = Box::new(move |map: &LinearMap<Atom, Expression, 8>| {
            match (
                map.get(&Atom::Escape('A', 1)).unwrap(),
                map.get(&Atom::Escape('A', 2)).unwrap(),
            ) {
                (Expression::Atom(a), Expression::Atom(b)) => match (a, b) {
                    (Atom::Numeric(a), Atom::Numeric(b)) => {
                        (Expression::Atom(Atom::Numeric(*a + *b)), true)
                    }
                    _ => (Expression::Atom(*a) + Expression::Atom(*b), false),
                },
                _ => unreachable!(),
            }
        });

        modifier.insert_rule("_A1 + _A2".parse::<Expression>().unwrap(), numeric_add);

        let mut expr1 = Expression::from_str("1 - -2").unwrap();
        let expected_expr1 = Expression::from_str("3").unwrap();
        expr1.simplify::<AdaptableModifier, 2>(&mut modifier);

        assert_eq!(expr1, expected_expr1);
    }

    #[test]
    fn test_adaptable_vector_mutable() {
        let mut modifier = AdaptableModifier::from_str_list(vec![
            ("_V1 - _V2", "_V1 + -_V2"),
            ("_V1 - <1, 2>", "50"),
        ]);

        let mut expr1 = Expression::from_str("<1, 2, 3, 4> - <1, 5, 7>").unwrap();
        let expected_expr1 = Expression::from_str("<1, 2, 3, 4> + -<1, 5, 7>").unwrap();
        modifier.modify_mut(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("<1, 2, 3, 4> - <1, 2>").unwrap();
        let expected_expr2 = Expression::from_str("50").unwrap();
        modifier.modify_mut(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_matrix_mutable() {
        let mut modifier = AdaptableModifier::from_str_list(vec![
            ("_M1 - _M2", "_M1 + -_M2"),
            ("_M1 - [1; 3]", "50"),
        ]);

        let mut expr1 = Expression::from_str("[1, 2; 3, 4] - [1, 5; 6, 7]").unwrap();
        let expected_expr1 = Expression::from_str("[1, 2; 3, 4] + -[1, 5; 6, 7]").unwrap();
        modifier.modify_mut(&mut expr1);

        assert_eq!(expr1, expected_expr1);

        let mut expr2 = Expression::from_str("[1, 2; 3, 4] - [1; 3]").unwrap();
        let expected_expr2 = Expression::from_str("50").unwrap();
        modifier.modify_mut(&mut expr2);

        assert_eq!(expr2, expected_expr2);
    }

    #[test]
    fn test_adaptable_mutable_comparison() {
        let modifier1 = AdaptableModifier::from_str_list(vec![("_*1 + _F1", "_*1 + 5")]);

        let mut modifier2 = AdaptableModifier::from_str_list(vec![("_*1 + _F1", "_*1 + 5")]);

        let expr =
            Expression::from_str("5 + cos(10)+ cos(1)+ cos(10)+ cos(5)+ cos(10)+ cos(10)+ cos(10)")
                .unwrap();
        let expected_expr = Expression::from_str("5 + 5 + 5 + 5 + 5 + 5 + 5 + 5").unwrap();

        let mut expr_immut = expr.clone();
        modifier1.modify_immut(&mut expr_immut);

        assert_eq!(expr_immut, expected_expr);

        let mut expr_mut = expr.clone();
        modifier2.modify_mut(&mut expr_mut);

        assert_eq!(expr_mut, expected_expr);
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_caching_am() {
        use std::collections::hash_map::RandomState;

        let mut modifier = CachingAdaptableModifier::<RandomState>::from_str_list(
            vec![("_*1 - _*2", "_*1 + -_*2")],
            None,
        );

        let mut expr1 = Expression::from_str("1 - 2").unwrap();
        let expected_expr1 = Expression::from_str("1 + -2").unwrap();
        expr1.simplify::<CachingAdaptableModifier<RandomState>, 50>(&mut modifier);

        assert_eq!(expr1, expected_expr1);
    }
}
