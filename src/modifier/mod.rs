use crate::expression::expression_tree::Expression;

// Modifier: objects which can modify an expression
pub trait ModifierImmutable {
    fn modify_immut(&self, expression: &mut Expression) -> bool; // returns true if modified
}

// modifiers which can modify themselves
pub trait ModifierMutable {
    fn modify_mut(&mut self, expression: &mut Expression) -> bool; // returns true if modified
}

pub mod adaptable_modifier;
pub mod default;

#[cfg(test)]
mod tests {
    use alloc::{boxed::Box, string::ToString, vec};

    use libm::sinf;

    use super::ModifierImmutable;
    use crate::expression::expression_tree::{Atom, Expression, Numeric};

    struct SimpleMod;

    impl ModifierImmutable for SimpleMod {
        fn modify_immut(&self, expression: &mut Expression) -> bool {
            match expression {
                Expression::Function { name, args } => match name.as_str() {
                    "sin" => {
                        *expression = Expression::Atom(Atom::Numeric(Numeric::Decimal(sinf(
                            match &*args[0] {
                                Expression::Atom(a) => match a {
                                    Atom::Numeric(n) => match n {
                                        Numeric::Decimal(d) => *d,
                                        _ => unimplemented!(),
                                    },
                                    _ => panic!("sin expects a numeric argument"),
                                },
                                _ => return false,
                            },
                        ))));
                        true
                    }
                    _ => false,
                },
                _ => false,
            }
        }
    }

    struct NothingMod;

    impl ModifierImmutable for NothingMod {
        fn modify_immut(&self, _expression: &mut Expression) -> bool {
            false
        }
    }

    #[test]
    fn test_modifier_approximate() {
        let expr = Expression::Function {
            name: "sin".to_string(),
            args: vec![Box::new(Expression::Atom(Atom::Numeric(Numeric::Decimal(
                20.0,
            ))))]
            .into_iter()
            .collect(),
        };

        assert_eq!(
            expr.evaluate_im::<SimpleMod, NothingMod, NothingMod, 50>(
                &SimpleMod,
                &NothingMod,
                &NothingMod
            )
            .1
            .unwrap(),
            Expression::Atom(Atom::Numeric(Numeric::Decimal(0.91294525073)))
        );
    }
}
