use crate::expression::expression_tree::Expression;

//Modifier: objects which can modify an expression
pub trait ModifierImmutable {
    fn modify_immut(&self, expression: &mut Expression) -> bool; //returns true if modified
}

//modifiers which can modify themselves
pub trait ModifierMutable { 
    fn modify_mut(&mut self, expression: &mut Expression) -> bool; //returns true if modified
}

pub mod default;
pub mod adaptable_modifier;

#[cfg(test)]
mod tests {
    use core::str::FromStr;
    use alloc::{vec, boxed::Box};

    use libm::sinf;

    use crate::expression::expression_tree::{Expression, Atom, Numeric};
    use super::ModifierImmutable;

    struct SimpleMod;

    impl ModifierImmutable for SimpleMod {
        fn modify_immut(&self, expression: &mut Expression) -> bool {
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

    impl ModifierImmutable for NothingMod {
        fn modify_immut(&self, _expression: &mut Expression) -> bool {
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

        assert_eq!(expr.approximate_im::<SimpleMod, NothingMod, NothingMod, 50>(&SimpleMod, &NothingMod, &NothingMod), Ok(Numeric::Decimal(0.91294525073)));
    }

}