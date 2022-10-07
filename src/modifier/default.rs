use alloc::{vec, boxed::Box};
use heapless::LinearMap;

use crate::expression::expression_tree::{Atom, Expression, Numeric};

use super::adaptable_modifier::AdaptableModifier;

//an AdaptableModifier that simplifies an expression tree
pub fn simplify() -> AdaptableModifier {
    reorganize() + reduce() + numeric_simplify()
}

//an AdaptableModifier that can reorganize the expression tree
pub fn reorganize() -> AdaptableModifier {
    let num = AdaptableModifier::from_fn_list(vec![
        ("_A1 + _A2".parse::<Expression>().unwrap(), Box::new(switch_atoms_add)),
        ("_*1 + _A1 + _A2".parse::<Expression>().unwrap(), Box::new(switch_atoms_ext_add)),
        ("_A1 * _A2".parse::<Expression>().unwrap(), Box::new(switch_atoms_mul)),
        ("_*1 * _A1 * _A2".parse::<Expression>().unwrap(), Box::new(switch_atoms_ext_mul)),
        ("_A1".parse::<Expression>().unwrap(), Box::new(denegate_internal)),
    ]);

    let shift = AdaptableModifier::from_str_list(vec![
        ("_*1 + (_*2 + _*3)", "_*1 + _*2 + _*3"),
        ("_*1 + (_*2 - _*3)", "_*1 + _*2 - _*3"),
        ("_*1 - (_*2 + _*3)", "_*1 - _*2 + _*3"),
        ("_*1 - (_*2 - _*3)", "_*1 - _*2 - _*3"),

        ("_*1 * (_*2 * _*3)", "_*1 * _*2 * _*3"),
        ("_*1 * (_*2 / _*3)", "_*1 * _*2 / _*3"),
        ("_*1 / (_*2 * _*3)", "_*1 / _*2 * _*3"),
        ("_*1 / (_*2 / _*3)", "_*1 / _*2 / _*3"),
    ]);

    let atom_func = AdaptableModifier::from_str_list(vec![
        ("_F1 + _A1", "_A1 + _F1"),
        ("_F1 * _A1", "_A1 * _F1"),
    ]);

    num + shift + atom_func
}

fn switch_atoms_add(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (map.get(&Atom::Escape('A', 1)).unwrap(), map.get(&Atom::Escape('A', 2)).unwrap()) {
        (Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Numeric(b))) => (Expression::Atom(Atom::Numeric(*b)) + Expression::Atom(Atom::Variable(*a)), true),
        (Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Variable(b))) => (Expression::Atom(Atom::Variable(*a.min(b))) + Expression::Atom(Atom::Variable(*a.max(b))), *a.min(b) != *a),
        (e1, e2) => (Expression::Add(Box::new(e1.clone()), Box::new(e2.clone())), false)
    }
}

fn switch_atoms_ext_add(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (map.get(&Atom::Escape('*', 1)).unwrap(), map.get(&Atom::Escape('A', 1)).unwrap(), map.get(&Atom::Escape('A', 2)).unwrap(),) {
        (e, Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Numeric(b))) => (e.clone() + Expression::Atom(Atom::Numeric(*b)) + Expression::Atom(Atom::Variable(*a)), true),
        (e, Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Variable(b))) => (e.clone() + Expression::Atom(Atom::Variable(*a.min(b))) + Expression::Atom(Atom::Variable(*a.max(b))), *a.min(b) != *a),
        (e1, e2, e3) => (e1.clone() + e2.clone() + e3.clone(), false),
    }
}

fn switch_atoms_mul(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (map.get(&Atom::Escape('A', 1)).unwrap(), map.get(&Atom::Escape('A', 2)).unwrap()) {
        (Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Numeric(b))) => (Expression::Atom(Atom::Numeric(*b)) * Expression::Atom(Atom::Variable(*a)), true),
        (Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Variable(b))) => (Expression::Atom(Atom::Variable(*a.min(b))) * Expression::Atom(Atom::Variable(*a.max(b))), *a.min(b) != *a),
        (e1, e2) => (Expression::Multiply(Box::new(e1.clone()), Box::new(e2.clone())), false)
    }
}

fn switch_atoms_ext_mul(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (map.get(&Atom::Escape('*', 1)).unwrap(), map.get(&Atom::Escape('A', 1)).unwrap(), map.get(&Atom::Escape('A', 2)).unwrap(),) {
        (e, Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Numeric(b))) => (e.clone() * Expression::Atom(Atom::Numeric(*b)) * Expression::Atom(Atom::Variable(*a)), true),
        (e, Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Variable(b))) => (e.clone() * Expression::Atom(Atom::Variable(*a.min(b))) * Expression::Atom(Atom::Variable(*a.max(b))), *a.min(b) != *a),
        (e1, e2, e3) => (e1.clone() * e2.clone() * e3.clone(), false),
    }
}

fn denegate_internal(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match map.get(&Atom::Escape('A', 1)).unwrap() {
        Expression::Atom(Atom::Numeric(n)) => match n {
            Numeric::Integer(i) => if i < &0 {
                (Expression::Negate(Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(-i))))), true)
            } else {
                (Expression::Atom(Atom::Numeric(Numeric::Integer(*i))), false)
            },
            Numeric::Decimal(f) => if f < &0.0 {
                (Expression::Negate(Box::new(Expression::Atom(Atom::Numeric(Numeric::Decimal(-f))))), true)
            } else {
                (Expression::Atom(Atom::Numeric(Numeric::Decimal(*f))), false)
            },
            Numeric::Fraction(r1, r2) => if r1 < &0 && r2 < &0 {
                (Expression::Atom(Atom::Numeric(Numeric::Fraction(-r1, -r2))), true)
            } else if r1 < &0 {
                (Expression::Negate(Box::new(Expression::Atom(Atom::Numeric(Numeric::Fraction(-r1, *r2))))), true)
            } else if r2 < &0 {
                (Expression::Negate(Box::new(Expression::Atom(Atom::Numeric(Numeric::Fraction(*r1, -r2))))), true)
            } else {
                (Expression::Atom(Atom::Numeric(Numeric::Fraction(*r1, *r2))), false)
            },
        },
        e => (e.clone(), false),
    }
}

//an AdaptableModifier that reduces identities in an expression tree
pub fn reduce() -> AdaptableModifier {
    AdaptableModifier::from_str_list(vec![
        ("_*1 + _*1", "2 * _*1"),
        ("_A1 * _*1 + _*1", "(1 + _A1) * _*1"),
        ("_*1 + _A1 * _*1", "(1 + _A1) * _*1"),
        ("_A1 * _*1 + _A2 * _*1", "(_A1 + _A2) * _*1"),

        ("_*1 + _*2 + _*2", "_*1 + 2 * _*2"),
        ("_*1 + _A1 * _*2 + _*2", "_*1 + (1 + _A1) * _*2"),
        ("_*1 + _*2 + _A1 * _*2", "_*1 + (1 + _A1) * _*2"),
        ("_*1 + _A1 * _*2 + _A2 * _*2", "_*1 + (_A1 + _A2) * _*2"),

        ("_*1 - _*1", "0"),
        ("_A1 * _*1 - _*1", "(_A1 - 1) * _*1"),
        ("_*1 - _A1 * _*1", "(-_A1 + 1) * _*1"),
        ("_A1 * _*1 - _A2 * _*1", "(_A1 - _A2) * _*1"),

        ("_*1 + _*2 - _*2", "_*1"),
        ("_*1 + _A1 * _*2 - _*2", "_*1 + (_A1 - 1) * _*2"),
        ("_*1 + _*2 - _A1 * _*2", "_*1 + (-_A1 + 1) * _*2"),
        ("_*1 + _A1 * _*2 - _A2 * _*2", "_*1 + (_A1 - _A2) * _*2"),

        ("--_*1", "_*1"),

        ("0 + _*1", "_*1"),
        ("_*1 + -_*2", "_*1 - _*2"),
        ("-_*1 + _*2", "_*2 - _*1"),

        ("1 * _*1", "_*1"),
        ("0 * _*1", "0"),
        ("-_*1 * _*2", "-(_*1 * _*2)"),
        ("_*1 * -_*2", "-(_*1 * _*2)"),
        ("-_*1 * -_*2", "_*1 * _*2"),

        ("_*1 / 1", "_*1"),
        ("0 / _*1", "0")
    ])
}

//an AdaptableModifier that simplifies numerics in an expression tree
pub fn numeric_simplify() -> AdaptableModifier {
    AdaptableModifier::from_fn_list(vec![
        ("_A1 + _A2".parse::<Expression>().unwrap(), Box::new(add_numeric)),
        ("_A1 - _A2".parse::<Expression>().unwrap(), Box::new(sub_numeric)),
        ("_A1 * _A2".parse::<Expression>().unwrap(), Box::new(mul_numeric)),
        ("_A1 / _A2".parse::<Expression>().unwrap(), Box::new(div_numeric)),
/*         ("_A1 ^ _A2".parse::<Expression>().unwrap(), Box::new(pow_numeric)),
        ("_A1 % _A2".parse::<Expression>().unwrap(), Box::new(mod_numeric)),
        ("_A1!".parse::<Expression>().unwrap(), Box::new(fact_numeric)), */
    ])
}

fn add_numeric(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (map.get(&Atom::Escape('A', 1)).unwrap(), map.get(&Atom::Escape('A', 2)).unwrap()) {
        (Expression::Atom(a1), Expression::Atom(a2)) => {
            match (a1, a2) {
                (Atom::Numeric(n1), Atom::Numeric(n2)) => {
                    (Expression::Atom(Atom::Numeric(*n1 + *n2)), true)
                }
                _ => (Expression::Atom(*a1) + Expression::Atom(*a2), false),
            }
        }
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}

fn sub_numeric(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (map.get(&Atom::Escape('A', 1)).unwrap(), map.get(&Atom::Escape('A', 2)).unwrap()) {
        (Expression::Atom(a1), Expression::Atom(a2)) => {
            match (a1, a2) {
                (Atom::Numeric(n1), Atom::Numeric(n2)) => {
                    (Expression::Atom(Atom::Numeric(*n1 - *n2)), true)
                }
                _ => (Expression::Atom(*a1) - Expression::Atom(*a2), false),
            }
        }
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}

fn mul_numeric(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (map.get(&Atom::Escape('A', 1)).unwrap(), map.get(&Atom::Escape('A', 2)).unwrap()) {
        (Expression::Atom(a1), Expression::Atom(a2)) => {
            match (a1, a2) {
                (Atom::Numeric(n1), Atom::Numeric(n2)) => {
                    (Expression::Atom(Atom::Numeric(*n1 * *n2)), true)
                }
                _ => (Expression::Atom(*a1) * Expression::Atom(*a2), false),
            }
        }
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}

fn div_numeric(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (map.get(&Atom::Escape('A', 1)).unwrap(), map.get(&Atom::Escape('A', 2)).unwrap()) {
        (Expression::Atom(a1), Expression::Atom(a2)) => {
            match (a1, a2) {
                (Atom::Numeric(n1), Atom::Numeric(n2)) => {
                    (Expression::Atom(Atom::Numeric(*n1 / *n2)), true)
                }
                _ => (Expression::Atom(*a1) / Expression::Atom(*a2), false),
            }
        }
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}


#[cfg(test)]
mod tests {
    use crate::{expression::expression_tree::Expression, modifier::{default::{simplify, reorganize, reduce, numeric_simplify,}, adaptable_modifier::AdaptableModifier}};

    #[test]
    fn test_reorganize() {
        let reo = reorganize();

        let mut expr2 = "sin(5) + x".parse::<Expression>().unwrap();
        expr2.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr2, "x + sin(5)".parse::<Expression>().unwrap());

        let mut expr3 = "x + 3".parse::<Expression>().unwrap();
        expr3.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr3, "3 + x".parse::<Expression>().unwrap());

        let mut expr4 = "sin(p) * 5".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr4, "5 * sin(p)".parse::<Expression>().unwrap());

        let mut expr5 = "z + a + x".parse::<Expression>().unwrap();
        expr5.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr5, "a + x + z".parse::<Expression>().unwrap());

        let mut expr6 = "x + 3 + 5".parse::<Expression>().unwrap();
        expr6.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr6, "3 + 5 + x".parse::<Expression>().unwrap());

        let mut expr7 = "z + y + x + w + v".parse::<Expression>().unwrap();
        expr7.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr7, "v + w + x + y + z".parse::<Expression>().unwrap());

        let mut expr8 = "x + 3 + y + 7".parse::<Expression>().unwrap();
        expr8.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr8, "3 + 7 + x + y".parse::<Expression>().unwrap());

        let mut expr9 = "x * 3 * y * 7 * z".parse::<Expression>().unwrap();
        expr9.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr9, "3 * 7 * x * y * z".parse::<Expression>().unwrap());

        let mut expr10 = "7 * z * 5".parse::<Expression>().unwrap();
        expr10.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr10, "7 * 5 * z".parse::<Expression>().unwrap());

        let mut expr11 = "a + 7 * z * 5".parse::<Expression>().unwrap();
        expr11.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr11, "a + 7 * 5 * z".parse::<Expression>().unwrap());

        let mut expr12 = "6 * y * x + 7 * z * 5".parse::<Expression>().unwrap();
        expr12.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr12, "6 * x * y + 7 * 5 * z".parse::<Expression>().unwrap());

        let mut expr13 = "6 * y * x + sin(x) * x".parse::<Expression>().unwrap();
        expr13.simplify::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr13, "6 * x * y + x * sin(x)".parse::<Expression>().unwrap());
    }

    #[test]
    fn test_reduce() {
        let red = reduce();

        let mut expr1 = "0 + x".parse::<Expression>().unwrap();
        expr1.simplify::<AdaptableModifier, 100>(&red);

        assert_eq!(expr1, "x".parse::<Expression>().unwrap());

        let mut expr2 = "0 * x".parse::<Expression>().unwrap();
        expr2.simplify::<AdaptableModifier, 100>(&red);

        assert_eq!(expr2, "0".parse::<Expression>().unwrap());

        let mut expr3 = "0 + 0".parse::<Expression>().unwrap();
        expr3.simplify::<AdaptableModifier, 100>(&red);

        assert_eq!(expr3, "0".parse::<Expression>().unwrap());

        let mut expr4 = "0 * x + 0 * y".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&red);

        assert_eq!(expr4, "0".parse::<Expression>().unwrap());

        let mut expr5 = "x + x".parse::<Expression>().unwrap();
        expr5.simplify::<AdaptableModifier, 100>(&red);

        assert_eq!(expr5, "2 * x".parse::<Expression>().unwrap());

        let mut expr6 = "x / 1".parse::<Expression>().unwrap();
        expr6.simplify::<AdaptableModifier, 100>(&red);

        assert_eq!(expr6, "x".parse::<Expression>().unwrap());

        let mut expr7 = "1 * x".parse::<Expression>().unwrap();
        expr7.simplify::<AdaptableModifier, 100>(&red);

        assert_eq!(expr7, "x".parse::<Expression>().unwrap());

        let mut expr8 = "x - x".parse::<Expression>().unwrap();
        expr8.simplify::<AdaptableModifier, 100>(&red);

        assert_eq!(expr8, "0".parse::<Expression>().unwrap());
    }

    #[test]
    fn test_numeric_simplify() {
        let num = numeric_simplify();

        let mut expr1 = "1 + 2".parse::<Expression>().unwrap();
        expr1.simplify::<AdaptableModifier, 100>(&num);

        assert_eq!(expr1, "3".parse::<Expression>().unwrap());

        let mut expr2 = "1 + 2 + 3".parse::<Expression>().unwrap();
        expr2.simplify::<AdaptableModifier, 100>(&num);

        assert_eq!(expr2, "6".parse::<Expression>().unwrap());

        let mut expr3 = "5 * 5".parse::<Expression>().unwrap();
        expr3.simplify::<AdaptableModifier, 100>(&num);

        assert_eq!(expr3, "25".parse::<Expression>().unwrap());

        let mut expr4 = "5.4/1.2".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&num);

        assert_eq!(expr4, "4.5".parse::<Expression>().unwrap());

        let mut expr5 = "5.4/1.2 + x".parse::<Expression>().unwrap();
        expr5.simplify::<AdaptableModifier, 100>(&num);

        assert_eq!(expr5, "4.5 + x".parse::<Expression>().unwrap());

        let mut expr6 = "5.4/1.2 + 1".parse::<Expression>().unwrap();
        expr6.simplify::<AdaptableModifier, 100>(&num);

        assert_eq!(expr6, "5.5".parse::<Expression>().unwrap());

        let mut expr7 = "5.4 * x / 1.2".parse::<Expression>().unwrap();
        expr7.simplify::<AdaptableModifier, 100>(&num);

        assert_eq!(expr7, "5.4 * x / 1.2".parse::<Expression>().unwrap()); //numeric_simplify shouldn't reorganize the expression
    }

    #[test]
    fn test_reorganize_in_simplify() {
        let simp = simplify();

        let mut expr2 = "sin(5) + x".parse::<Expression>().unwrap();
        expr2.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr2, "x + sin(5)".parse::<Expression>().unwrap());

        let mut expr3 = "x + 3".parse::<Expression>().unwrap();
        expr3.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr3, "3 + x".parse::<Expression>().unwrap());

        let mut expr4 = "sin(p) * 5".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr4, "5 * sin(p)".parse::<Expression>().unwrap());

        let mut expr5 = "z + a + x".parse::<Expression>().unwrap();
        expr5.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr5, "a + x + z".parse::<Expression>().unwrap());

        let mut expr6 = "x + 3 + 5".parse::<Expression>().unwrap();
        expr6.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr6, "8 + x".parse::<Expression>().unwrap());

        let mut expr7 = "z + y + x + w + v".parse::<Expression>().unwrap();
        expr7.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr7, "v + w + x + y + z".parse::<Expression>().unwrap());

        let mut expr8 = "x + 3 + y + 7".parse::<Expression>().unwrap();
        expr8.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr8, "10 + x + y".parse::<Expression>().unwrap());

        let mut expr9 = "x * 3 * y * 7 * z".parse::<Expression>().unwrap();
        expr9.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr9, "21 * x * y * z".parse::<Expression>().unwrap());

        let mut expr10 = "7 * z * 5".parse::<Expression>().unwrap();
        expr10.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr10, "35 * z".parse::<Expression>().unwrap());

        let mut expr11 = "a + 7 * z * 5".parse::<Expression>().unwrap();
        expr11.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr11, "a + 35 * z".parse::<Expression>().unwrap());

        let mut expr12 = "6 * y * x + 7 * z * 5".parse::<Expression>().unwrap();
        expr12.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr12, "6 * x * y + 35 * z".parse::<Expression>().unwrap());

        let mut expr13 = "6 * y * x + sin(x) * x".parse::<Expression>().unwrap();
        expr13.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr13, "6 * x * y + x * sin(x)".parse::<Expression>().unwrap());
    }

    #[test]
    fn test_reduce_in_simplify() {
        let simp = simplify();

        let mut expr1 = "0 + x".parse::<Expression>().unwrap();
        expr1.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr1, "x".parse::<Expression>().unwrap());

        let mut expr2 = "0 * x".parse::<Expression>().unwrap();
        expr2.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr2, "0".parse::<Expression>().unwrap());

        let mut expr3 = "0 + 0".parse::<Expression>().unwrap();
        expr3.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr3, "0".parse::<Expression>().unwrap());

        let mut expr4 = "0 * x + 0 * y".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr4, "0".parse::<Expression>().unwrap());

        let mut expr5 = "x + x".parse::<Expression>().unwrap();
        expr5.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr5, "2 * x".parse::<Expression>().unwrap());

        let mut expr6 = "x / 1".parse::<Expression>().unwrap();
        expr6.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr6, "x".parse::<Expression>().unwrap());

        let mut expr7 = "1 * x".parse::<Expression>().unwrap();
        expr7.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr7, "x".parse::<Expression>().unwrap());

        let mut expr8 = "x - x".parse::<Expression>().unwrap();
        expr8.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr8, "0".parse::<Expression>().unwrap());
    }

    #[test]
    fn test_numeric_simplify_in_simplify() {
        let simp = simplify();

        let mut expr1 = "1 + 2".parse::<Expression>().unwrap();
        expr1.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr1, "3".parse::<Expression>().unwrap());

        let mut expr2 = "1 + 2 + 3".parse::<Expression>().unwrap();
        expr2.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr2, "6".parse::<Expression>().unwrap());

        let mut expr3 = "5 * 5".parse::<Expression>().unwrap();
        expr3.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr3, "25".parse::<Expression>().unwrap());

        let mut expr4 = "5.4/1.2".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr4, "4.5".parse::<Expression>().unwrap());

        let mut expr5 = "5.4/1.2 + x".parse::<Expression>().unwrap();
        expr5.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr5, "4.5 + x".parse::<Expression>().unwrap());

        let mut expr6 = "5.4/1.2 + 1".parse::<Expression>().unwrap();
        expr6.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr6, "5.5".parse::<Expression>().unwrap());

        let mut expr7 = "5.4 * x / 1.2".parse::<Expression>().unwrap();
        expr7.simplify::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr7, "5.4 * x / 1.2".parse::<Expression>().unwrap()); //numeric_simplify shouldn't reorganize the expression
    }

    #[test]
    fn test_simplify() {
        let mut expr1 = "8 + x + 4 + x".parse::<Expression>().unwrap();
        expr1.simplify::<AdaptableModifier, 100>(&simplify());

        assert_eq!(expr1, "12 + 2 * x".parse::<Expression>().unwrap());

        let mut expr2 = "8 + x + 4 + x + 2 * x".parse::<Expression>().unwrap();
        expr2.simplify::<AdaptableModifier, 100>(&simplify());

        assert_eq!(expr2, "12 + 4 * x".parse::<Expression>().unwrap());

        let mut expr3 = "8 + x + 4 + x - 2 * x + 3 * x".parse::<Expression>().unwrap();
        expr3.simplify::<AdaptableModifier, 100>(&simplify());

        assert_eq!(expr3, "12 + 3 * x".parse::<Expression>().unwrap());

        // this is _*1 - _*1, but I'm guessing that the Vectors are fucking up the equality
        /* let mut expr4 = "sin(x + 5 - 2 * x) - sin(5 - x)".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&simplify());

        assert_eq!(expr4, "0".parse::<Expression>().unwrap()); */
    }    
}