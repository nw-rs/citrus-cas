use alloc::{boxed::Box, vec::Vec};

use nom::{
    IResult, 
    sequence::{delimited, tuple,}, 
    character::complete::{space0, char,}, 
    combinator::{map, fail}, 
    branch::alt, 
    bytes::complete::{take_while1, take_till1},
    multi::many0,
};

use crate::expression_tree::{Expression, Numeric, Atom};

pub fn parse(input: &str) -> Expression {
    parse_add_sub(input).map_err(|_| "failed to parse").unwrap().1
}

fn parse_recursive(input: &str) -> IResult<&str, Expression> {
    alt((parse_parentheses, parse_numeric))(input)
}

fn parse_exponents(input: &str) -> IResult<&str, Expression> {
    let (input, num) = parse_recursive(input)?;
    let (input, ops) = many0(tuple((char('^'), parse_exponents)))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn parse_mult_div_mod(input: &str) -> IResult<&str, Expression> {
    let (input, num) = parse_exponents(input)?;
    let (input, ops) = many0(tuple((alt((char('*'), char('/'), char('%'))), parse_exponents)))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn parse_add_sub(input: &str) -> IResult<&str, Expression> {
    let (input, num) = parse_mult_div_mod(input)?;
    let (input, ops) = many0(tuple((alt((char('+'), char('-'))), parse_mult_div_mod)))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn fold_binary_operators(expr: Expression, ops: Vec<(char, Expression)>) -> Expression {
    ops.into_iter().fold(expr, |acc, val| parse_binary_op(val, acc))
}

fn parse_binary_op(operator_pair: (char, Expression), expr1: Expression) -> Expression {
    let (op, expr2) = operator_pair;
    match op {
        '+' => Expression::Add(Box::new(expr1), Box::new(expr2)),
        '-' => Expression::Subtract(Box::new(expr1), Box::new(expr2)),
        '*' => Expression::Multiply(Box::new(expr1), Box::new(expr2)),
        '/' => Expression::Divide(Box::new(expr1), Box::new(expr2)),
        '^' => Expression::Power(Box::new(expr1), Box::new(expr2)),
        '%' => Expression::Modulus(Box::new(expr1), Box::new(expr2)),
        _ => panic!("Invalid operator"),
    }
}

fn parse_parentheses(input: &str) -> IResult<&str, Expression> {
    delimited(
        space0, 
        delimited(char('('), parse_add_sub, char(')')), 
        space0,
    )(input)
}

fn parse_numeric(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(space0, take_while1(is_numeric_value), space0),
        parse_number
    )(input)
}

fn is_numeric_value(c: char) -> bool {
    c.is_digit(10) || c == '.'
}

fn parse_number(input: &str) -> Expression {
    Expression::Atom(
        Atom::Numeric(
            match input.contains('.') {
                true => Numeric::Decimal(input.parse::<f32>().unwrap()),
                false => Numeric::Integer(input.parse::<i32>().unwrap()),
            }
        )
    )
}

#[cfg(test)]
mod tests {
    use alloc::boxed::Box;

    use super::parse;
    use crate::expression_tree::*;

    #[test]
    fn test_integer() {
        assert_eq!(parse("1"), 
            Expression::Atom(
                Atom::Numeric(
                    Numeric::Integer(1)
                )
            )
        );
        assert_eq!(parse("55"), 
            Expression::Atom(
                Atom::Numeric(
                    Numeric::Integer(55)
                )
            )
        );
    }

    #[test]
    fn test_decimal() {
        assert_eq!(parse("1.0"), 
            Expression::Atom(
                Atom::Numeric(
                    Numeric::Decimal(1.0)
                )
            )
        );
    }

    #[test]
    fn test_add() {
        assert_eq!(parse("1 + 2"),
            Expression::Add(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(2)
                    )
                ))
            )
        );
    }

    #[test]
    fn test_subtract() {
        assert_eq!(parse("1 - 2"),
            Expression::Subtract(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(2)
                    )
                ))
            )
        );
    }

    #[test]
    fn test_modulus() {
        assert_eq!(parse("1 % 2"),
            Expression::Modulus(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(2)
                    )
                ))
            )
        );
    }

    #[test]
    fn test_multiply() {
        assert_eq!(parse("1 * 2"),
            Expression::Multiply(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(2)
                    )
                ))
            )
        );
    }

    #[test]
    fn test_divide() {
        assert_eq!(parse("1 / 2"),
            Expression::Divide(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(2)
                    )
                ))
            )
        );
    }

    #[test]
    fn test_exponent() {
        assert_eq!(parse("1 ^ 2"),
            Expression::Power(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(2)
                    )
                ))
            )
        );
    }

    #[test]
    fn test_parentheses() {
        assert_eq!(parse("(1 + 2) * 5"),
            Expression::Multiply(
                Box::new(Expression::Add(
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(1)
                        )
                    )),
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(2)
                        )
                    ))
                )),
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(5)
                    )
                ))
            )
        );
    }

    #[test]
    fn test_multi_level_expression() {
        assert_eq!(parse("1 * 2 + 3 / 4 ^ 6 % 7"),
            Expression::Add(
                Box::new(Expression::Multiply(
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(1)
                        )
                    )),
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(2)
                        )
                    ))
                )),
                Box::new(Expression::Modulus(
                    Box::new(Expression::Divide(
                        Box::new(Expression::Atom(
                            Atom::Numeric(
                                Numeric::Integer(3)
                            )
                        )),
                        Box::new(Expression::Power(
                            Box::new(Expression::Atom(
                                Atom::Numeric(
                                    Numeric::Integer(4)
                                )
                            )),
                            Box::new(Expression::Atom(
                                Atom::Numeric(
                                    Numeric::Integer(6)
                                )
                            ))
                        ))
                    )),
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(7)
                        )
                    ))
                ))
            )
        );
    }
}