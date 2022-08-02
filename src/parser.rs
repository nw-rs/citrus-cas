use core::str::FromStr;

use alloc::{boxed::Box, vec::Vec};

use nom::{
    IResult, 
    sequence::{delimited, tuple, preceded, terminated,}, 
    character::complete::{space0, char,}, 
    combinator::map, 
    branch::alt, 
    bytes::complete::{take_while1, take},
    multi::many0,
};

use crate::expression_tree::{Expression, Numeric, Atom};

pub fn parse(input: &str) -> Expression {
    parse_add_sub(input).map_err(|_| "failed to parse").unwrap().1
}

fn parse_recursive(input: &str) -> IResult<&str, Expression> {
    alt((parse_parentheses, parse_numeric, parse_function, parse_escape, parse_variable))(input)
}

fn parse_parentheses(input: &str) -> IResult<&str, Expression> {
    delimited(
        space0, 
        delimited(
            char('('), 
            parse_add_sub, 
            char(')')
        ), 
        space0,
    )(input)
}

fn parse_numeric(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0, 
            take_while1(is_numeric_value), 
            space0
        ),
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

fn parse_function(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0,
            tuple((
                preceded(
                    space0, 
                    take_while1(|c: char| {c.is_alphabetic()})
                ), 
                delimited(
                    char('('), 
                    many0(alt((terminated(parse_add_sub, char(',')), parse_add_sub))), //inefficient
                    char(')'),
                )
            )),
            space0,
        ),
        |(name,arg)| Expression::Function {
            name: heapless::String::from_str(name).unwrap(),
            args: arg.into_iter().map(|arg| Box::new(arg)).collect(),
        }
    )(input)
}

fn parse_escape(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0, 
            preceded(
                char('_'), 
                take(1usize)
            ), 
            space0
        ),
        |value: &str| Expression::Atom(
            Atom::Escape(value.chars().next().unwrap())
        )
    )(input)
}

fn parse_variable(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0, 
            take(1usize), 
            space0
        ),
        |value: &str| Expression::Atom(
            Atom::Variable(value.chars().next().unwrap())
        )
    )(input)
}

fn parse_unary(input: &str) -> IResult<&str, Expression> {
    alt((parse_negate, parse_exponents))(input)
}

fn parse_exponents(input: &str) -> IResult<&str, Expression> {
    let (input, num) = parse_recursive(input)?;
    let (input, ops) = many0(tuple((char('^'), parse_exponents)))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn parse_negate(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(space0, tuple((char('-'), parse_unary)), space0),
        parse_unary_op
    )(input)
}

fn parse_mult_div_mod(input: &str) -> IResult<&str, Expression> {
    let (input, num) = parse_unary(input)?;
    let (input, ops) = many0(tuple((alt((char('*'), char('/'), char('%'))), parse_unary)))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn parse_add_sub(input: &str) -> IResult<&str, Expression> {
    let (input, num) = parse_mult_div_mod(input)?;
    let (input, ops) = many0(tuple((alt((char('+'), char('-'))), parse_mult_div_mod)))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn parse_unary_op(operator_pair: (char, Expression)) -> Expression {
    match operator_pair {
        ('-', expr) => Expression::Negate(Box::new(expr)),
        ('!', expr) => Expression::Factorial(Box::new(expr)),
        ('%', expr) => Expression::Percent(Box::new(expr)),
        _ => panic!("Invalid operator"),
    }
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

#[cfg(test)]
mod tests {
    use core::str::FromStr;

    use alloc::{boxed::Box, vec};

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
    fn test_escape() {
        assert_eq!(parse("_A"), 
            Expression::Atom(
                Atom::Escape('A')
            )
        );
    }

    #[test]
    fn test_variable() {
        assert_eq!(parse("x"), 
            Expression::Atom(
                Atom::Variable('x')
            )
        );
    }

    #[test]
    fn test_function() {
        assert_eq!(parse("sin(1)"), 
            Expression::Function {
                name: heapless::String::from_str("sin").unwrap(),
                args: vec![
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(1)
                        )
                    )),
                ].into_iter().collect(),
            }
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
    fn test_add_negative() {
        assert_eq!(parse("1 + -2"),
            Expression::Add(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Negate(
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(2)
                        )
                    ))
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
    fn test_multiply_negative() {
        assert_eq!(parse("1 * -2"),
            Expression::Multiply(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Negate(
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(2)
                        )
                    ))
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
    fn test_spaceless() {
        assert_eq!(parse("1+2*5"),
            Expression::Add(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Multiply(
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(2)
                        )
                    )),
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(5)
                        )
                    ))
                ))
            )
        );
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(parse(" 1    +  2 *   5  "),
            Expression::Add(
                Box::new(Expression::Atom(
                    Atom::Numeric(
                        Numeric::Integer(1)
                    )
                )),
                Box::new(Expression::Multiply(
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(2)
                        )
                    )),
                    Box::new(Expression::Atom(
                        Atom::Numeric(
                            Numeric::Integer(5)
                        )
                    ))
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