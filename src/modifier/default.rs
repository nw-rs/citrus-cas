use alloc::{
    boxed::Box,
    string::{String, ToString},
    vec,
};
use heapless::LinearMap;

use crate::expression::expression_tree::{Atom, Expression, Numeric};

use super::adaptable_modifier::{AdaptableModifier, ModifierFunction};

// an AdaptableModifier that simplifies an expression tree
pub fn simplifier() -> AdaptableModifier {
    reorganize() + reduce() + numeric_simplify()
}

// an AdaptableModifier that can reorganize the expression tree
pub fn reorganize() -> AdaptableModifier {
    let num = AdaptableModifier::from_fn_list(vec![
        (
            "_A1 + _A2".parse::<Expression>().unwrap(),
            Box::new(switch_atoms_add),
        ),
        (
            "_*1 + _A1 + _A2".parse::<Expression>().unwrap(),
            Box::new(switch_atoms_ext_add),
        ),
        (
            "_A1 * _A2".parse::<Expression>().unwrap(),
            Box::new(switch_atoms_mul),
        ),
        (
            "_*1 * _A1 * _A2".parse::<Expression>().unwrap(),
            Box::new(switch_atoms_ext_mul),
        ),
        (
            "_A1".parse::<Expression>().unwrap(),
            Box::new(denegate_internal),
        ),
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
    match (
        map.get(&Atom::Escape('A', 1)).unwrap(),
        map.get(&Atom::Escape('A', 2)).unwrap(),
    ) {
        (Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Numeric(b))) => (
            Expression::Atom(Atom::Numeric(*b)) + Expression::Atom(Atom::Variable(*a)),
            true,
        ),
        (Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Variable(b))) => (
            Expression::Atom(Atom::Variable(*a.min(b)))
                + Expression::Atom(Atom::Variable(*a.max(b))),
            *a.min(b) != *a,
        ),
        (e1, e2) => (
            Expression::Add(Box::new(e1.clone()), Box::new(e2.clone())),
            false,
        ),
    }
}

fn switch_atoms_ext_add(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (
        map.get(&Atom::Escape('*', 1)).unwrap(),
        map.get(&Atom::Escape('A', 1)).unwrap(),
        map.get(&Atom::Escape('A', 2)).unwrap(),
    ) {
        (e, Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Numeric(b))) => (
            e.clone() + Expression::Atom(Atom::Numeric(*b)) + Expression::Atom(Atom::Variable(*a)),
            true,
        ),
        (e, Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Variable(b))) => (
            e.clone()
                + Expression::Atom(Atom::Variable(*a.min(b)))
                + Expression::Atom(Atom::Variable(*a.max(b))),
            *a.min(b) != *a,
        ),
        (e1, e2, e3) => (e1.clone() + e2.clone() + e3.clone(), false),
    }
}

fn switch_atoms_mul(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (
        map.get(&Atom::Escape('A', 1)).unwrap(),
        map.get(&Atom::Escape('A', 2)).unwrap(),
    ) {
        (Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Numeric(b))) => (
            Expression::Atom(Atom::Numeric(*b)) * Expression::Atom(Atom::Variable(*a)),
            true,
        ),
        (Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Variable(b))) => (
            Expression::Atom(Atom::Variable(*a.min(b)))
                * Expression::Atom(Atom::Variable(*a.max(b))),
            *a.min(b) != *a,
        ),
        (e1, e2) => (
            Expression::Multiply(Box::new(e1.clone()), Box::new(e2.clone())),
            false,
        ),
    }
}

fn switch_atoms_ext_mul(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (
        map.get(&Atom::Escape('*', 1)).unwrap(),
        map.get(&Atom::Escape('A', 1)).unwrap(),
        map.get(&Atom::Escape('A', 2)).unwrap(),
    ) {
        (e, Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Numeric(b))) => (
            e.clone() * Expression::Atom(Atom::Numeric(*b)) * Expression::Atom(Atom::Variable(*a)),
            true,
        ),
        (e, Expression::Atom(Atom::Variable(a)), Expression::Atom(Atom::Variable(b))) => (
            e.clone()
                * Expression::Atom(Atom::Variable(*a.min(b)))
                * Expression::Atom(Atom::Variable(*a.max(b))),
            *a.min(b) != *a,
        ),
        (e1, e2, e3) => (e1.clone() * e2.clone() * e3.clone(), false),
    }
}

fn denegate_internal(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match map.get(&Atom::Escape('A', 1)).unwrap() {
        Expression::Atom(Atom::Numeric(n)) => match n {
            Numeric::Integer(i) => {
                if i < &0 {
                    (
                        Expression::Negate(Box::new(Expression::Atom(Atom::Numeric(
                            Numeric::Integer(-i),
                        )))),
                        true,
                    )
                } else {
                    (Expression::Atom(Atom::Numeric(Numeric::Integer(*i))), false)
                }
            }
            Numeric::Decimal(f) => {
                if f < &0.0 {
                    (
                        Expression::Negate(Box::new(Expression::Atom(Atom::Numeric(
                            Numeric::Decimal(-f),
                        )))),
                        true,
                    )
                } else {
                    (Expression::Atom(Atom::Numeric(Numeric::Decimal(*f))), false)
                }
            }
            Numeric::Fraction(r1, r2) => {
                if r1 < &0 && r2 < &0 {
                    (
                        Expression::Atom(Atom::Numeric(Numeric::Fraction(-r1, -r2))),
                        true,
                    )
                } else if r1 < &0 {
                    (
                        Expression::Negate(Box::new(Expression::Atom(Atom::Numeric(
                            Numeric::Fraction(-r1, *r2),
                        )))),
                        true,
                    )
                } else if r2 < &0 {
                    (
                        Expression::Negate(Box::new(Expression::Atom(Atom::Numeric(
                            Numeric::Fraction(*r1, -r2),
                        )))),
                        true,
                    )
                } else {
                    (
                        Expression::Atom(Atom::Numeric(Numeric::Fraction(*r1, *r2))),
                        false,
                    )
                }
            }
        },
        e => (e.clone(), false),
    }
}

// an AdaptableModifier that reduces identities in an expression tree
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
        ("0 / _*1", "0"),
    ])
}

// an AdaptableModifier that simplifies numerics in an expression tree
pub fn numeric_simplify() -> AdaptableModifier {
    AdaptableModifier::from_fn_list(vec![
        (
            "_A1 + _A2".parse::<Expression>().unwrap(),
            Box::new(add_numeric),
        ),
        (
            "_A1 - _A2".parse::<Expression>().unwrap(),
            Box::new(sub_numeric),
        ),
        (
            "_A1 * _A2".parse::<Expression>().unwrap(),
            Box::new(mul_numeric),
        ),
        (
            "_A1 / _A2".parse::<Expression>().unwrap(),
            Box::new(div_numeric),
        ),
        (
            "_A1 ^ _A2".parse::<Expression>().unwrap(),
            Box::new(pow_numeric),
        ),
        /*(
            "_A1 % _A2".parse::<Expression>().unwrap(),
            Box::new(mod_numeric)
        ),
        ("_A1!".parse::<Expression>().unwrap(), Box::new(fact_numeric)), */
    ])
}

fn add_numeric(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (
        map.get(&Atom::Escape('A', 1)).unwrap(),
        map.get(&Atom::Escape('A', 2)).unwrap(),
    ) {
        (Expression::Atom(a1), Expression::Atom(a2)) => match (a1, a2) {
            (Atom::Numeric(n1), Atom::Numeric(n2)) => {
                (Expression::Atom(Atom::Numeric(*n1 + *n2)), true)
            }
            _ => (Expression::Atom(*a1) + Expression::Atom(*a2), false),
        },
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}

fn sub_numeric(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (
        map.get(&Atom::Escape('A', 1)).unwrap(),
        map.get(&Atom::Escape('A', 2)).unwrap(),
    ) {
        (Expression::Atom(a1), Expression::Atom(a2)) => match (a1, a2) {
            (Atom::Numeric(n1), Atom::Numeric(n2)) => {
                (Expression::Atom(Atom::Numeric(*n1 - *n2)), true)
            }
            _ => (Expression::Atom(*a1) - Expression::Atom(*a2), false),
        },
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}

fn mul_numeric(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (
        map.get(&Atom::Escape('A', 1)).unwrap(),
        map.get(&Atom::Escape('A', 2)).unwrap(),
    ) {
        (Expression::Atom(a1), Expression::Atom(a2)) => match (a1, a2) {
            (Atom::Numeric(n1), Atom::Numeric(n2)) => {
                (Expression::Atom(Atom::Numeric(*n1 * *n2)), true)
            }
            _ => (Expression::Atom(*a1) * Expression::Atom(*a2), false),
        },
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}

fn div_numeric(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (
        map.get(&Atom::Escape('A', 1)).unwrap(),
        map.get(&Atom::Escape('A', 2)).unwrap(),
    ) {
        (Expression::Atom(a1), Expression::Atom(a2)) => match (a1, a2) {
            (Atom::Numeric(n1), Atom::Numeric(n2)) => {
                (Expression::Atom(Atom::Numeric(*n1 / *n2)), true)
            }
            _ => (Expression::Atom(*a1) / Expression::Atom(*a2), false),
        },
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}

fn pow_numeric(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match (
        map.get(&Atom::Escape('A', 1)).unwrap(),
        map.get(&Atom::Escape('A', 2)).unwrap(),
    ) {
        (Expression::Atom(a1), Expression::Atom(a2)) => match (a1, a2) {
            (Atom::Numeric(n1), Atom::Numeric(n2)) => (
                Expression::Atom(Atom::Numeric(Numeric::Decimal(libm::powf(
                    (*n1).into(),
                    (*n2).into(),
                )))),
                true,
            ),
            _ => (
                Expression::Power(
                    Box::new(Expression::Atom(*a1)),
                    Box::new(Expression::Atom(*a2)),
                ),
                false,
            ),
        },
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}

// an AdpatableModifier that evaluates an expression
pub fn evaluator() -> AdaptableModifier {
    AdaptableModifier::from_str_list(vec![])
}

// an AdaptableModifer that approximates an expression
pub fn approximator() -> AdaptableModifier {
    num_approx_helper() + trig_approx() + log_approx() + numeric_fun_approx() + calculus_approx()
}

pub fn num_approx_helper() -> AdaptableModifier {
    AdaptableModifier::from_fn_list(vec![(
        "-_A1".parse::<Expression>().unwrap(),
        Box::new(internal_negate_num),
    )])
}

fn internal_negate_num(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    match map.get(&Atom::Escape('A', 1)).unwrap() {
        Expression::Atom(a1) => match a1 {
            Atom::Numeric(n1) => (Expression::Atom(Atom::Numeric(-*n1)), false),
            _ => (Expression::Atom(*a1), false),
        },
        _ => unreachable!("this pointer was called with non-atom expressions"),
    }
}

fn single_num_approx(func: fn(f32) -> f32, name: String) -> ModifierFunction {
    Box::new(move |map: &LinearMap<Atom, Expression, 8>| {
        match map.get(&Atom::Escape('A', 1)).unwrap() {
            Expression::Atom(a1) => match a1 {
                Atom::Numeric(n1) => (
                    Expression::Atom(Atom::Numeric(Numeric::Decimal(func((*n1).into())))),
                    true,
                ),
                _ => (
                    Expression::Function {
                        name: name.clone(),
                        args: vec![Box::new(Expression::Atom(*a1))],
                    },
                    false,
                ),
            },
            _ => unreachable!("this pointer was called with non-atom expressions"),
        }
    })
}

pub fn trig_approx() -> AdaptableModifier {
    AdaptableModifier::from_fn_list(vec![
        (
            "sin(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::sinf, "sin".to_string()),
        ),
        (
            "cos(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::cosf, "cos".to_string()),
        ),
        (
            "tan(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::tanf, "tan".to_string()),
        ),
        (
            "asin(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::asinf, "asin".to_string()),
        ),
        (
            "acos(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::acosf, "acos".to_string()),
        ),
        (
            "atan(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::atanf, "atan".to_string()),
        ),
        (
            "sinh(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::sinhf, "sinh".to_string()),
        ),
        (
            "cosh(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::coshf, "cosh".to_string()),
        ),
        (
            "tanh(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::tanhf, "tanh".to_string()),
        ),
        (
            "asinh(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::asinhf, "asinh".to_string()),
        ),
        (
            "acosh(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::acoshf, "acosh".to_string()),
        ),
        (
            "atanh(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::atanhf, "atanh".to_string()),
        ),
    ])
}

pub fn log_approx() -> AdaptableModifier {
    AdaptableModifier::from_fn_list(vec![
        (
            "log(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::logf, "log".to_string()),
        ),
        (
            "log2(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::log2f, "log2".to_string()),
        ),
        (
            "log10(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::log10f, "log10".to_string()),
        ),
        (
            "exp(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::expf, "exp".to_string()),
        ),
        (
            "exp2(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::exp2f, "exp2".to_string()),
        ),
        (
            "exp10(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::exp10f, "exp10".to_string()),
        ),
    ])
}

pub fn numeric_fun_approx() -> AdaptableModifier {
    AdaptableModifier::from_fn_list(vec![
        (
            "abs(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::fabsf, "abs".to_string()),
        ),
        (
            "sqrt(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::sqrtf, "sqrt".to_string()),
        ),
        (
            "cbrt(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::cbrtf, "cbrt".to_string()),
        ),
        (
            "ceil(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::ceilf, "ceil".to_string()),
        ),
        (
            "floor(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::floorf, "floor".to_string()),
        ),
        (
            "round(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::roundf, "round".to_string()),
        ),
        (
            "trunc(_A1)".parse::<Expression>().unwrap(),
            single_num_approx(libm::truncf, "trunc".to_string()),
        ),
    ])
}

pub fn calculus_approx() -> AdaptableModifier {
    AdaptableModifier::from_fn_list(vec![
        (
            // diff(expression, for this variable, at this value)
            "diff(_*1, _*2, _A1)".parse::<Expression>().unwrap(),
            Box::new(diff_approx),
        ),
        (
            // integrate(expression, for this variable, from this value, to this value)
            "int(_*1, _*2, _A1, _A2)".parse::<Expression>().unwrap(),
            Box::new(int_approx),
        ),
        /* (
            "limit(_*1, _*2, _*3)".parse::<Expression>().unwrap(),
            Box::new(limit_approx),
        ), */
    ])
}

fn value_replace(expr: &Expression, var: &Expression, val: &Expression) -> Expression {
    if expr == var {
        val.clone()
    } else {
        match expr {
            Expression::Atom(a) => Expression::Atom(*a),

            Expression::Negate(e) => Expression::Negate(Box::new(value_replace(e, var, val))),
            Expression::Factorial(e) => Expression::Factorial(Box::new(value_replace(e, var, val))),
            Expression::Percent(e) => Expression::Percent(Box::new(value_replace(e, var, val))),

            Expression::Add(e1, e2) => Expression::Add(
                Box::new(value_replace(e1, var, val)),
                Box::new(value_replace(e2, var, val)),
            ),
            Expression::Subtract(e1, e2) => Expression::Subtract(
                Box::new(value_replace(e1, var, val)),
                Box::new(value_replace(e2, var, val)),
            ),
            Expression::Multiply(e1, e2) => Expression::Multiply(
                Box::new(value_replace(e1, var, val)),
                Box::new(value_replace(e2, var, val)),
            ),
            Expression::Divide(e1, e2) => Expression::Divide(
                Box::new(value_replace(e1, var, val)),
                Box::new(value_replace(e2, var, val)),
            ),
            Expression::Power(e1, e2) => Expression::Power(
                Box::new(value_replace(e1, var, val)),
                Box::new(value_replace(e2, var, val)),
            ),
            Expression::Modulus(e1, e2) => Expression::Modulus(
                Box::new(value_replace(e1, var, val)),
                Box::new(value_replace(e2, var, val)),
            ),

            Expression::Function { name, args } => Expression::Function {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|e| Box::new(value_replace(e, var, val)))
                    .collect(),
            },
            Expression::Vector { backing, size } => Expression::Vector {
                backing: backing
                    .iter()
                    .map(|e| Box::new(value_replace(e, var, val)))
                    .collect(),
                size: *size,
            },
            Expression::Matrix { backing, shape } => Expression::Matrix {
                backing: backing
                    .iter()
                    .map(|e| Box::new(value_replace(e, var, val)))
                    .collect(),
                shape: *shape,
            },
        }
    }
}

// approximate the derivative of a function
fn diff_approx(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    let expr = map.get(&Atom::Escape('*', 1)).unwrap();
    let var = map.get(&Atom::Escape('*', 2)).unwrap();
    let val = map.get(&Atom::Escape('A', 1)).unwrap();

    let h = 0.0001;

    match val {
        Expression::Atom(Atom::Numeric(n)) => (
            // symmetric difference quotient at h = 0.0001
            Expression::Divide(
                Box::new(Expression::Subtract(
                    Box::new(value_replace(
                        expr,
                        var,
                        &Expression::Atom(Atom::Numeric(*n + Numeric::Decimal(h))),
                    )),
                    Box::new(value_replace(
                        expr,
                        var,
                        &Expression::Atom(Atom::Numeric(*n - Numeric::Decimal(h))),
                    )),
                )),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Decimal(2.0 * h)))),
            ),
            true,
        ),
        _ => (
            Expression::Atom(Atom::Error(crate::Error::InvalidSyntax)),
            false,
        ),
    }
}

// approximate the integral of a function
fn int_approx(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    let expr = map.get(&Atom::Escape('*', 1)).unwrap();
    let var = map.get(&Atom::Escape('*', 2)).unwrap();
    let val1 = map.get(&Atom::Escape('A', 1)).unwrap();
    let val2 = map.get(&Atom::Escape('A', 2)).unwrap();

    // TODO: use a more accurate approximation
    // trapeziodal rule: (b-a)*((f(a)+f(b))/2)
    match (val1, val2) {
        (Expression::Atom(Atom::Numeric(_)), Expression::Atom(Atom::Numeric(_))) => (
            Expression::Multiply(
                Box::new(Expression::Subtract(
                    Box::new(val2.clone()),
                    Box::new(val1.clone()),
                )),
                Box::new(Expression::Divide(
                    Box::new(Expression::Add(
                        Box::new(value_replace(expr, var, val1)),
                        Box::new(value_replace(expr, var, val2)),
                    )),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Decimal(2.0)))),
                )),
            ),
            true,
        ),
        _ => (
            Expression::Atom(Atom::Error(crate::Error::InvalidSyntax)),
            false,
        ),
    }
}

// TODO: implement limit approximation
// approximate the limit of a function
/* fn limit_approx(map: &LinearMap<Atom, Expression, 8>) -> (Expression, bool) {
    unimplemented!()
} */

#[cfg(test)]
mod tests {
    use crate::{
        expression::expression_tree::Expression,
        modifier::{
            adaptable_modifier::AdaptableModifier,
            default::{
                approximator, calculus_approx, evaluator, log_approx, num_approx_helper,
                numeric_fun_approx, numeric_simplify, reduce, reorganize, simplifier, trig_approx,
            },
        },
    };

    #[test]
    fn test_reorganize() {
        let reo = reorganize();

        let mut expr2 = "sin(5) + x".parse::<Expression>().unwrap();
        expr2.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr2, "x + sin(5)".parse::<Expression>().unwrap());

        let mut expr3 = "x + 3".parse::<Expression>().unwrap();
        expr3.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr3, "3 + x".parse::<Expression>().unwrap());

        let mut expr4 = "sin(p) * 5".parse::<Expression>().unwrap();
        expr4.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr4, "5 * sin(p)".parse::<Expression>().unwrap());

        let mut expr5 = "z + a + x".parse::<Expression>().unwrap();
        expr5.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr5, "a + x + z".parse::<Expression>().unwrap());

        let mut expr6 = "x + 3 + 5".parse::<Expression>().unwrap();
        expr6.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr6, "3 + 5 + x".parse::<Expression>().unwrap());

        let mut expr7 = "z + y + x + w + v".parse::<Expression>().unwrap();
        expr7.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr7, "v + w + x + y + z".parse::<Expression>().unwrap());

        let mut expr8 = "x + 3 + y + 7".parse::<Expression>().unwrap();
        expr8.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr8, "3 + 7 + x + y".parse::<Expression>().unwrap());

        let mut expr9 = "x * 3 * y * 7 * z".parse::<Expression>().unwrap();
        expr9.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr9, "3 * 7 * x * y * z".parse::<Expression>().unwrap());

        let mut expr10 = "7 * z * 5".parse::<Expression>().unwrap();
        expr10.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr10, "7 * 5 * z".parse::<Expression>().unwrap());

        let mut expr11 = "a + 7 * z * 5".parse::<Expression>().unwrap();
        expr11.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(expr11, "a + 7 * 5 * z".parse::<Expression>().unwrap());

        let mut expr12 = "6 * y * x + 7 * z * 5".parse::<Expression>().unwrap();
        expr12.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(
            expr12,
            "6 * x * y + 7 * 5 * z".parse::<Expression>().unwrap()
        );

        let mut expr13 = "6 * y * x + sin(x) * x".parse::<Expression>().unwrap();
        expr13.simplify_im::<AdaptableModifier, 100>(&reo);

        assert_eq!(
            expr13,
            "6 * x * y + x * sin(x)".parse::<Expression>().unwrap()
        );
    }

    #[test]
    fn test_reduce() {
        let red = reduce();

        let mut expr1 = "0 + x".parse::<Expression>().unwrap();
        expr1.simplify_im::<AdaptableModifier, 100>(&red);

        assert_eq!(expr1, "x".parse::<Expression>().unwrap());

        let mut expr2 = "0 * x".parse::<Expression>().unwrap();
        expr2.simplify_im::<AdaptableModifier, 100>(&red);

        assert_eq!(expr2, "0".parse::<Expression>().unwrap());

        let mut expr3 = "0 + 0".parse::<Expression>().unwrap();
        expr3.simplify_im::<AdaptableModifier, 100>(&red);

        assert_eq!(expr3, "0".parse::<Expression>().unwrap());

        let mut expr4 = "0 * x + 0 * y".parse::<Expression>().unwrap();
        expr4.simplify_im::<AdaptableModifier, 100>(&red);

        assert_eq!(expr4, "0".parse::<Expression>().unwrap());

        let mut expr5 = "x + x".parse::<Expression>().unwrap();
        expr5.simplify_im::<AdaptableModifier, 100>(&red);

        assert_eq!(expr5, "2 * x".parse::<Expression>().unwrap());

        let mut expr6 = "x / 1".parse::<Expression>().unwrap();
        expr6.simplify_im::<AdaptableModifier, 100>(&red);

        assert_eq!(expr6, "x".parse::<Expression>().unwrap());

        let mut expr7 = "1 * x".parse::<Expression>().unwrap();
        expr7.simplify_im::<AdaptableModifier, 100>(&red);

        assert_eq!(expr7, "x".parse::<Expression>().unwrap());

        let mut expr8 = "x - x".parse::<Expression>().unwrap();
        expr8.simplify_im::<AdaptableModifier, 100>(&red);

        assert_eq!(expr8, "0".parse::<Expression>().unwrap());
    }

    #[test]
    fn test_numeric_simplify() {
        let num = numeric_simplify();

        let mut expr1 = "1 + 2".parse::<Expression>().unwrap();
        expr1.simplify_im::<AdaptableModifier, 100>(&num);

        assert_eq!(expr1, "3".parse::<Expression>().unwrap());

        let mut expr2 = "1 + 2 + 3".parse::<Expression>().unwrap();
        expr2.simplify_im::<AdaptableModifier, 100>(&num);

        assert_eq!(expr2, "6".parse::<Expression>().unwrap());

        let mut expr3 = "5 * 5".parse::<Expression>().unwrap();
        expr3.simplify_im::<AdaptableModifier, 100>(&num);

        assert_eq!(expr3, "25".parse::<Expression>().unwrap());

        let mut expr4 = "5.4/1.2".parse::<Expression>().unwrap();
        expr4.simplify_im::<AdaptableModifier, 100>(&num);

        assert_eq!(expr4, "4.5".parse::<Expression>().unwrap());

        let mut expr5 = "5.4/1.2 + x".parse::<Expression>().unwrap();
        expr5.simplify_im::<AdaptableModifier, 100>(&num);

        assert_eq!(expr5, "4.5 + x".parse::<Expression>().unwrap());

        let mut expr6 = "5.4/1.2 + 1".parse::<Expression>().unwrap();
        expr6.simplify_im::<AdaptableModifier, 100>(&num);

        assert_eq!(expr6, "5.5".parse::<Expression>().unwrap());

        let mut expr7 = "5.4 * x / 1.2".parse::<Expression>().unwrap();
        expr7.simplify_im::<AdaptableModifier, 100>(&num);

        assert_eq!(expr7, "5.4 * x / 1.2".parse::<Expression>().unwrap()); // numeric_simplify shouldn't reorganize the expression
    }

    #[test]
    fn test_reorganize_in_simplify() {
        let simp = simplifier();

        let mut expr2 = "sin(5) + x".parse::<Expression>().unwrap();
        expr2.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr2, "x + sin(5)".parse::<Expression>().unwrap());

        let mut expr3 = "x + 3".parse::<Expression>().unwrap();
        expr3.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr3, "3 + x".parse::<Expression>().unwrap());

        let mut expr4 = "sin(p) * 5".parse::<Expression>().unwrap();
        expr4.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr4, "5 * sin(p)".parse::<Expression>().unwrap());

        let mut expr5 = "z + a + x".parse::<Expression>().unwrap();
        expr5.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr5, "a + x + z".parse::<Expression>().unwrap());

        let mut expr6 = "x + 3 + 5".parse::<Expression>().unwrap();
        expr6.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr6, "8 + x".parse::<Expression>().unwrap());

        let mut expr7 = "z + y + x + w + v".parse::<Expression>().unwrap();
        expr7.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr7, "v + w + x + y + z".parse::<Expression>().unwrap());

        let mut expr8 = "x + 3 + y + 7".parse::<Expression>().unwrap();
        expr8.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr8, "10 + x + y".parse::<Expression>().unwrap());

        let mut expr9 = "x * 3 * y * 7 * z".parse::<Expression>().unwrap();
        expr9.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr9, "21 * x * y * z".parse::<Expression>().unwrap());

        let mut expr10 = "7 * z * 5".parse::<Expression>().unwrap();
        expr10.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr10, "35 * z".parse::<Expression>().unwrap());

        let mut expr11 = "a + 7 * z * 5".parse::<Expression>().unwrap();
        expr11.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr11, "a + 35 * z".parse::<Expression>().unwrap());

        let mut expr12 = "6 * y * x + 7 * z * 5".parse::<Expression>().unwrap();
        expr12.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr12, "6 * x * y + 35 * z".parse::<Expression>().unwrap());

        let mut expr13 = "6 * y * x + sin(x) * x".parse::<Expression>().unwrap();
        expr13.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(
            expr13,
            "6 * x * y + x * sin(x)".parse::<Expression>().unwrap()
        );
    }

    #[test]
    fn test_reduce_in_simplify() {
        let simp = simplifier();

        let mut expr1 = "0 + x".parse::<Expression>().unwrap();
        expr1.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr1, "x".parse::<Expression>().unwrap());

        let mut expr2 = "0 * x".parse::<Expression>().unwrap();
        expr2.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr2, "0".parse::<Expression>().unwrap());

        let mut expr3 = "0 + 0".parse::<Expression>().unwrap();
        expr3.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr3, "0".parse::<Expression>().unwrap());

        let mut expr4 = "0 * x + 0 * y".parse::<Expression>().unwrap();
        expr4.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr4, "0".parse::<Expression>().unwrap());

        let mut expr5 = "x + x".parse::<Expression>().unwrap();
        expr5.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr5, "2 * x".parse::<Expression>().unwrap());

        let mut expr6 = "x / 1".parse::<Expression>().unwrap();
        expr6.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr6, "x".parse::<Expression>().unwrap());

        let mut expr7 = "1 * x".parse::<Expression>().unwrap();
        expr7.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr7, "x".parse::<Expression>().unwrap());

        let mut expr8 = "x - x".parse::<Expression>().unwrap();
        expr8.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr8, "0".parse::<Expression>().unwrap());
    }

    #[test]
    fn test_numeric_simplify_in_simplify() {
        let simp = simplifier();

        let mut expr1 = "1 + 2".parse::<Expression>().unwrap();
        expr1.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr1, "3".parse::<Expression>().unwrap());

        let mut expr2 = "1 + 2 + 3".parse::<Expression>().unwrap();
        expr2.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr2, "6".parse::<Expression>().unwrap());

        let mut expr3 = "5 * 5".parse::<Expression>().unwrap();
        expr3.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr3, "25".parse::<Expression>().unwrap());

        let mut expr4 = "5.4/1.2".parse::<Expression>().unwrap();
        expr4.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr4, "4.5".parse::<Expression>().unwrap());

        let mut expr5 = "5.4/1.2 + x".parse::<Expression>().unwrap();
        expr5.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr5, "4.5 + x".parse::<Expression>().unwrap());

        let mut expr6 = "5.4/1.2 + 1".parse::<Expression>().unwrap();
        expr6.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr6, "5.5".parse::<Expression>().unwrap());

        let mut expr7 = "5.4 * x / 1.2".parse::<Expression>().unwrap();
        expr7.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr7, "5.4 * x / 1.2".parse::<Expression>().unwrap()); // numeric_simplify shouldn't reorganize the expression
    }

    #[test]
    fn test_simplify() {
        let simp = simplifier();

        let mut expr1 = "8 + x + 4 + x".parse::<Expression>().unwrap();
        expr1.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr1, "12 + 2 * x".parse::<Expression>().unwrap());

        let mut expr2 = "8 + x + 4 + x + 2 * x".parse::<Expression>().unwrap();
        expr2.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr2, "12 + 4 * x".parse::<Expression>().unwrap());

        let mut expr3 = "8 + x + 4 + x - 2 * x + 3 * x"
            .parse::<Expression>()
            .unwrap();
        expr3.simplify_im::<AdaptableModifier, 100>(&simp);

        assert_eq!(expr3, "12 + 3 * x".parse::<Expression>().unwrap());

        // this is _*1 - _*1, but I'm guessing that the Vectors are fucking up the equality
        /* let mut expr4 = "sin(x + 5 - 2 * x) - sin(5 - x)".parse::<Expression>().unwrap();
        expr4.simplify::<AdaptableModifier, 100>(&simplify());

        assert_eq!(expr4, "0".parse::<Expression>().unwrap()); */
    }

    #[test]
    fn test_trig_approx() {
        let trig = trig_approx();
        let eval = evaluator();
        let simp = simplifier();

        let form = |expr: &Expression| {
            expr.approximate_im::<AdaptableModifier, AdaptableModifier, AdaptableModifier, 100>(
                &trig, &eval, &simp,
            )
            .unwrap()
        };

        let expr1 = "sin(90)".parse::<Expression>().unwrap();
        let expr1_comp = form(&expr1);

        assert_eq!(expr1_comp, "0.8939966636".parse::<Expression>().unwrap());

        let expr2 = "cos(90)".parse::<Expression>().unwrap();
        let expr2_comp = form(&expr2);

        assert_eq!(expr2_comp, "-0.4480736161".parse::<Expression>().unwrap());

        let expr3 = "tan(90)".parse::<Expression>().unwrap();
        let expr3_comp = form(&expr3);

        assert_eq!(expr3_comp, "-1.995200412".parse::<Expression>().unwrap());

        let expr4 = "asin(1)".parse::<Expression>().unwrap();
        let expr4_comp = form(&expr4);

        assert_eq!(expr4_comp, "1.570796327".parse::<Expression>().unwrap());

        let expr5 = "acos(1)".parse::<Expression>().unwrap();
        let expr5_comp = form(&expr5);

        assert_eq!(expr5_comp, "0".parse::<Expression>().unwrap());
    }

    #[test]
    fn test_log_approx() {
        let log = log_approx();
        let eval = evaluator();
        let simp = simplifier();

        let form = |expr: &Expression| {
            expr.approximate_im::<AdaptableModifier, AdaptableModifier, AdaptableModifier, 100>(
                &log, &eval, &simp,
            )
        };

        let expr1 = "log(10)".parse::<Expression>().unwrap();
        let expr1_comp = form(&expr1);

        assert_eq!(expr1_comp, Ok("2.302585093".parse::<Expression>().unwrap()));

        let expr2 = "log(100)".parse::<Expression>().unwrap();
        let expr2_comp = form(&expr2);

        assert_eq!(expr2_comp, Ok("4.605170186".parse::<Expression>().unwrap()));

        let expr3 = "log(1000)".parse::<Expression>().unwrap();
        let expr3_comp = form(&expr3);

        assert_eq!(expr3_comp, Ok("6.907755278".parse::<Expression>().unwrap()));

        let expr4 = "log(10000)".parse::<Expression>().unwrap();
        let expr4_comp = form(&expr4);

        assert_eq!(expr4_comp, Ok("9.210340371".parse::<Expression>().unwrap()));

        let expr5 = "log(100000)".parse::<Expression>().unwrap();
        let expr5_comp = form(&expr5);

        assert_eq!(expr5_comp, Ok("11.51292546".parse::<Expression>().unwrap()));

        let expr6 = "log(1000000)".parse::<Expression>().unwrap();
        let expr6_comp = form(&expr6);

        assert_eq!(expr6_comp, Ok("13.81551055".parse::<Expression>().unwrap()));

        let expr7 = "log(10000000)".parse::<Expression>().unwrap();
        let expr7_comp = form(&expr7);

        assert_eq!(expr7_comp, Ok("16.11809564".parse::<Expression>().unwrap()));

        let expr8 = "log2(10)".parse::<Expression>().unwrap();
        let expr8_comp = form(&expr8);

        assert_eq!(expr8_comp, Ok("3.321928095".parse::<Expression>().unwrap()));

        let expr9 = "log10(10)".parse::<Expression>().unwrap();
        let expr9_comp = form(&expr9);

        assert_eq!(expr9_comp, Ok("1".parse::<Expression>().unwrap()));

        let expr10 = "exp(10)".parse::<Expression>().unwrap();
        let expr10_comp = form(&expr10);

        assert_eq!(
            expr10_comp,
            Ok("22026.46579".parse::<Expression>().unwrap())
        );

        let expr11 = "exp2(10)".parse::<Expression>().unwrap();
        let expr11_comp = form(&expr11);

        assert_eq!(expr11_comp, Ok("1024".parse::<Expression>().unwrap()));

        let expr12 = "exp10(2)".parse::<Expression>().unwrap();
        let expr12_comp = form(&expr12);

        assert_eq!(expr12_comp, Ok("100".parse::<Expression>().unwrap()));
    }

    #[test]
    fn test_numeric_fun_approx() {
        let num = numeric_fun_approx() + num_approx_helper();
        let eval = evaluator();
        let simp = simplifier();

        let form = |expr: &Expression| {
            expr.approximate_im::<AdaptableModifier, AdaptableModifier, AdaptableModifier, 100>(
                &num, &eval, &simp,
            )
        };

        let expr1 = "abs(-10)".parse::<Expression>().unwrap();
        let expr1_comp = form(&expr1);

        assert_eq!(expr1_comp, Ok("10".parse::<Expression>().unwrap()));

        let expr2 = "abs(10)".parse::<Expression>().unwrap();
        let expr2_comp = form(&expr2);

        assert_eq!(expr2_comp, Ok("10".parse::<Expression>().unwrap()));

        let expr3 = "sqrt(4)".parse::<Expression>().unwrap();
        let expr3_comp = form(&expr3);

        assert_eq!(expr3_comp, Ok("2".parse::<Expression>().unwrap()));

        let expr4 = "sqrt(16)".parse::<Expression>().unwrap();
        let expr4_comp = form(&expr4);

        assert_eq!(expr4_comp, Ok("4".parse::<Expression>().unwrap()));

        let expr5 = "cbrt(8)".parse::<Expression>().unwrap();
        let expr5_comp = form(&expr5);

        assert_eq!(expr5_comp, Ok("2".parse::<Expression>().unwrap()));

        let expr6 = "ceil(2.5)".parse::<Expression>().unwrap();
        let expr6_comp = form(&expr6);

        assert_eq!(expr6_comp, Ok("3".parse::<Expression>().unwrap()));

        let expr7 = "floor(2.5)".parse::<Expression>().unwrap();
        let expr7_comp = form(&expr7);

        assert_eq!(expr7_comp, Ok("2".parse::<Expression>().unwrap()));

        let expr8 = "round(2.5)".parse::<Expression>().unwrap();
        let expr8_comp = form(&expr8);

        assert_eq!(expr8_comp, Ok("3".parse::<Expression>().unwrap()));

        let expr9 = "round(2.4)".parse::<Expression>().unwrap();
        let expr9_comp = form(&expr9);

        assert_eq!(expr9_comp, Ok("2".parse::<Expression>().unwrap()));

        let expr10 = "trunc(2.6)".parse::<Expression>().unwrap();
        let expr10_comp = form(&expr10);

        assert_eq!(expr10_comp, Ok("2".parse::<Expression>().unwrap()));

        let expr11 = "trunc(2.4)".parse::<Expression>().unwrap();
        let expr11_comp = form(&expr11);

        assert_eq!(expr11_comp, Ok("2".parse::<Expression>().unwrap()));
    }

    #[test]
    fn test_calculus_approx() {
        let num = calculus_approx() + num_approx_helper();
        let eval = evaluator();
        let simp = simplifier();

        let form = |expr: &Expression| {
            expr.approximate_im::<AdaptableModifier, AdaptableModifier, AdaptableModifier, 100>(
                &num, &eval, &simp,
            )
        };
        let expr1 = "diff(x^2, x, 2)".parse::<Expression>().unwrap();
        let expr1_comp = form(&expr1);

        assert_eq!(expr1_comp, Ok("3.9982796".parse::<Expression>().unwrap())); // should be 4, but approx isn't exact

        let expr2 = "diff(x^2, x, 3)".parse::<Expression>().unwrap();
        let expr2_comp = form(&expr2);

        assert_eq!(expr2_comp, Ok("5.993843".parse::<Expression>().unwrap())); // should be 6, but approx isn't exact

        let expr3 = "int(x^2, x, 4, 6)".parse::<Expression>().unwrap();
        let expr3_comp = form(&expr3);

        assert_eq!(expr3_comp, Ok("52".parse::<Expression>().unwrap()));

        let expr4 = "int(2*x^2-x, x, 4, 6)".parse::<Expression>().unwrap();
        let expr4_comp = form(&expr4);

        assert_eq!(expr4_comp, Ok("94".parse::<Expression>().unwrap())); // should be 91.33333333333333, but approx isn't exact
    }

    #[test]
    fn test_approximator() {
        let num = approximator();
        let eval = evaluator();
        let simp = simplifier();

        let form = |expr: &Expression| {
            expr.approximate_im::<AdaptableModifier, AdaptableModifier, AdaptableModifier, 100>(
                &num, &eval, &simp,
            )
        };

        let expr1 = "sin(90) + cos(90)".parse::<Expression>().unwrap();
        let expr1_comp = form(&expr1);

        assert_eq!(expr1_comp, Ok("0.44592303".parse::<Expression>().unwrap()));

        let expr2 = "tan(90) + log(5)".parse::<Expression>().unwrap();
        let expr2_comp = form(&expr2);

        assert_eq!(expr2_comp, Ok("-0.38576245".parse::<Expression>().unwrap()));

        let expr3 = "log(10000000) + exp(10)".parse::<Expression>().unwrap();
        let expr3_comp = form(&expr3);

        assert_eq!(expr3_comp, Ok("22042.582".parse::<Expression>().unwrap()));

        let expr4 = "exp2(10) + abs(-7)".parse::<Expression>().unwrap();
        let expr4_comp = form(&expr4);

        assert_eq!(expr4_comp, Ok("1031".parse::<Expression>().unwrap()));

        let expr5 = "sqrt(4) + cbrt(8)".parse::<Expression>().unwrap();
        let expr5_comp = form(&expr5);

        assert_eq!(expr5_comp, Ok("4".parse::<Expression>().unwrap()));

        let expr6 = "ceil(2.5) + floor(2.5)".parse::<Expression>().unwrap();
        let expr6_comp = form(&expr6);

        assert_eq!(expr6_comp, Ok("5".parse::<Expression>().unwrap()));

        let expr7 = "round(log(78))".parse::<Expression>().unwrap();
        let expr7_comp = form(&expr7);

        assert_eq!(expr7_comp, Ok("4".parse::<Expression>().unwrap()));

        let expr8 = "diff(log(x), x, 4)".parse::<Expression>().unwrap();
        let expr8_comp = form(&expr8);

        assert_eq!(expr8_comp, Ok("0.2503395".parse::<Expression>().unwrap())); // should be 0.25, but approx isn't exact

        let expr9 = "int(log(x), x, 4, 6)".parse::<Expression>().unwrap();
        let expr9_comp = form(&expr9);

        assert_eq!(expr9_comp, Ok("3.1780539".parse::<Expression>().unwrap())); // should be ~3.20537, but approx isn't exact
    }
}
