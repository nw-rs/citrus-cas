use alloc::{boxed::Box, string::ToString, vec::Vec};

use nom::multi::separated_list0;
use nom::{
    branch::alt,
    bytes::complete::{take, take_while1},
    character::complete::{char, digit1, space0},
    combinator::map,
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

use crate::expression::expression_tree::{Atom, Expression, Numeric};

//TODO: explain parser

pub fn parse(input: &str) -> Expression {
    parse_add_sub(input)
        .map_err(|_| "failed to parse")
        .unwrap()
        .1
}

fn parse_recursive(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_parentheses,
        parse_vector,
        parse_matrix,
        parse_numeric,
        parse_function,
        parse_escape,
        parse_variable,
    ))(input)
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
        parse_number,
    )(input)
}

fn is_numeric_value(c: char) -> bool {
    c.is_ascii_digit() || c == '.'
}

fn parse_number(input: &str) -> Expression {
    Expression::Atom(Atom::Numeric(match input.contains('.') {
        true => Numeric::Decimal(input.parse::<f32>().unwrap()),
        false => Numeric::Integer(input.parse::<i32>().unwrap()),
    }))
}

fn parse_function(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0,
            tuple((
                preceded(space0, take_while1(|c: char| c.is_alphabetic())),
                preceded(
                    char('('),
                    many0(terminated(parse_add_sub, alt((char(','), char(')'))))),
                ),
            )),
            space0,
        ),
        |(name, arg_list)| Expression::Function {
            name: name.to_string(),
            args: arg_list.into_iter().map(Box::new).collect(),
        },
    )(input)
}

fn parse_vector(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0,
            preceded(
                char('<'),
                many0(terminated(parse_add_sub, alt((char(','), char('>'))))),
            ),
            space0,
        ),
        |vector| Expression::Vector {
            size: vector.len() as u8,
            backing: vector.into_iter().map(Box::new).collect(),
        },
    )(input)
}

fn parse_matrix(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0,
            delimited(
                char('['),
                separated_list0(char(';'), separated_list0(char(','), parse_add_sub)),
                char(']'),
            ),
            space0,
        ),
        |flatten_matrix| {
            let row_count = flatten_matrix.len() as u8;
            let col_count = flatten_matrix[0].len() as u8; // assuming every row has the same number of columns

            let backing = flatten_matrix
                .into_iter()
                .flatten()
                .map(Box::new)
                .collect();
            Expression::Matrix {
                backing,
                shape: (row_count, col_count),
            }
        },
    )(input)
}

fn parse_escape(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0,
            tuple((preceded(char('_'), take(1usize)), digit1)),
            space0,
        ),
        |(value, num): (&str, &str)| {
            Expression::Atom(Atom::Escape(
                value.chars().next().unwrap(),
                num.parse::<u8>().unwrap(),
            ))
        },
    )(input)
}

fn parse_variable(input: &str) -> IResult<&str, Expression> {
    map(delimited(space0, take(1usize), space0), |value: &str| {
        Expression::Atom(Atom::Variable(value.chars().next().unwrap()))
    })(input)
}

fn parse_unary(input: &str) -> IResult<&str, Expression> {
    alt((parse_unary_prefix, parse_unary_postfix, parse_exponents))(input)
}

fn parse_exponents(input: &str) -> IResult<&str, Expression> {
    let (input, num) = parse_recursive(input)?;
    let (input, ops) = many0(tuple((char('^'), parse_exponents)))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn parse_unary_prefix(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(space0, tuple((char('-'), parse_unary)), space0),
        parse_unary_prefix_op,
    )(input)
}

fn parse_unary_postfix(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(space0, tuple((parse_exponents, char('!'))), space0),
        parse_unary_postfix_op,
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

fn parse_unary_prefix_op(operator_pair: (char, Expression)) -> Expression {
    let (operator, operand) = operator_pair;
    match operator {
        '-' => Expression::Negate(Box::new(operand)),
        _ => panic!("Invalid operator"),
    }
}

fn parse_unary_postfix_op(operator_pair: (Expression, char)) -> Expression {
    let (operand, operator) = operator_pair;
    match operator {
        '!' => Expression::Factorial(Box::new(operand)),
        _ => panic!("Invalid operator"),
    }
}

fn fold_binary_operators(expr: Expression, ops: Vec<(char, Expression)>) -> Expression {
    ops.into_iter()
        .fold(expr, |acc, val| parse_binary_op(val, acc))
}

fn parse_binary_op(operator_pair: (char, Expression), expr1: Expression) -> Expression {
    let (operator, expr2) = operator_pair;
    match operator {
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
    use alloc::{boxed::Box, string::ToString, vec};

    use super::parse;
    use crate::expression::expression_tree::*;

    #[test]
    fn test_integer() {
        assert_eq!(
            parse("1"),
            Expression::Atom(Atom::Numeric(Numeric::Integer(1)))
        );
        assert_eq!(
            parse("55"),
            Expression::Atom(Atom::Numeric(Numeric::Integer(55)))
        );
    }

    #[test]
    fn test_decimal() {
        assert_eq!(
            parse("1.0"),
            Expression::Atom(Atom::Numeric(Numeric::Decimal(1.0)))
        );
    }

    #[test]
    fn test_escape() {
        assert_eq!(parse("_A2"), Expression::Atom(Atom::Escape('A', 2)));
    }

    #[test]
    fn test_wildcard_escape() {
        assert_eq!(parse("_*0"), Expression::Atom(Atom::Escape('*', 0)));
    }

    #[test]
    fn test_variable() {
        assert_eq!(parse("x"), Expression::Atom(Atom::Variable('x')));
    }

    #[test]
    fn test_unicode_variable() {
        assert_eq!(parse("π"), Expression::Atom(Atom::Variable('π')));
    }

    #[test]
    fn test_function() {
        assert_eq!(
            parse("sin(1 + -2)"),
            Expression::Function {
                name: "sin".to_string(),
                args: vec![Box::new(Expression::Add(
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Negate(Box::new(Expression::Atom(
                        Atom::Numeric(Numeric::Integer(2))
                    ))))
                )),]
                .into_iter()
                .collect(),
            }
        );
    }

    #[test]
    fn test_multiple_arguments() {
        assert_eq!(
            parse("normcdf(0, 1, 2.5, x)"),
            Expression::Function {
                name: "normcdf".to_string(),
                args: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(0)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Decimal(2.5)))),
                    Box::new(Expression::Atom(Atom::Variable('x'))),
                ]
                .into_iter()
                .collect(),
            }
        );
    }

    #[test]
    fn test_advanced_function() {
        assert_eq!(
            parse("5 * log(10, sin(x))"),
            Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Function {
                    name: "log".to_string(),
                    args: vec![
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(10)))),
                        Box::new(Expression::Function {
                            name: "sin".to_string(),
                            args: vec![Box::new(Expression::Atom(Atom::Variable('x'))),]
                                .into_iter()
                                .collect(),
                        }),
                    ]
                    .into_iter()
                    .collect(),
                })
            )
        );
    }

    #[test]
    fn test_add() {
        assert_eq!(
            parse("1 + 2"),
            Expression::Add(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn test_subtract() {
        assert_eq!(
            parse("1 - 2"),
            Expression::Subtract(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn test_add_negative() {
        assert_eq!(
            parse("1 + -2"),
            Expression::Add(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Negate(Box::new(Expression::Atom(
                    Atom::Numeric(Numeric::Integer(2))
                ))))
            )
        );
    }

    #[test]
    fn test_modulus() {
        assert_eq!(
            parse("1 % 2"),
            Expression::Modulus(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn test_multiply() {
        assert_eq!(
            parse("1 * 2"),
            Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn test_divide() {
        assert_eq!(
            parse("1 / 2"),
            Expression::Divide(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn test_multiply_negative() {
        assert_eq!(
            parse("1 * -2"),
            Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Negate(Box::new(Expression::Atom(
                    Atom::Numeric(Numeric::Integer(2))
                ))))
            )
        );
    }

    #[test]
    fn test_exponent() {
        assert_eq!(
            parse("1 ^ 2"),
            Expression::Power(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn test_factorial() {
        assert_eq!(
            parse("5!"),
            Expression::Factorial(Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(
                5
            )))))
        );
    }

    #[test]
    fn test_parentheses() {
        assert_eq!(
            parse("(1 + 2) * 5"),
            Expression::Multiply(
                Box::new(Expression::Add(
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
                )),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5))))
            )
        );
    }

    #[test]
    fn test_nested_parentheses() {
        assert_eq!(
            parse("(5 * (4 + (6 / 3)))"),
            Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Add(
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                    Box::new(Expression::Divide(
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3))))
                    ))
                ))
            )
        );
    }

    #[test]
    fn test_spaceless() {
        assert_eq!(
            parse("1+2*5"),
            Expression::Add(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Multiply(
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5))))
                ))
            )
        );
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(
            parse(" 1    +  2 *   5  "),
            Expression::Add(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Multiply(
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5))))
                ))
            )
        );
    }

    #[test]
    fn test_multi_level_expression() {
        assert_eq!(
            parse("1 * 2 + 3 / 4 ^ 6 % 7"),
            Expression::Add(
                Box::new(Expression::Multiply(
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
                )),
                Box::new(Expression::Modulus(
                    Box::new(Expression::Divide(
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                        Box::new(Expression::Power(
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6))))
                        ))
                    )),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7))))
                ))
            )
        );
    }

    macro_rules! integer_atom {
        ( $i:literal ) => {
            Expression::Atom(Atom::Numeric(Numeric::Integer($i)))
        };
    }

    #[test]
    fn test_vector_literal() {
        assert_eq!(
            parse("<1, 2, 3>"),
            Expression::Vector {
                backing: vec![
                    Box::new(integer_atom!(1)),
                    Box::new(integer_atom!(2)),
                    Box::new(integer_atom!(3)),
                ],
                size: 3 as u8,
            }
        )
    }

    #[test]
    fn test_vector_in_expression() {
        assert_eq!(
            parse("1 + <2, 3, 4>"),
            Expression::Add(
                Box::new(integer_atom!(1)),
                Box::new(Expression::Vector {
                    backing: vec![
                        Box::new(integer_atom!(2)),
                        Box::new(integer_atom!(3)),
                        Box::new(integer_atom!(4)),
                    ],
                    size: 3 as u8,
                })
            )
        )
    }

    #[test]
    fn test_expression_in_vector() {
        assert_eq!(
            parse("<1 + 2, 3 - 4, 5 * 6, 7 / 8, 9 % 10>"),
            Expression::Vector {
                backing: vec![
                    Box::new(Expression::Add(
                        Box::new(integer_atom!(1)),
                        Box::new(integer_atom!(2))
                    )),
                    Box::new(Expression::Subtract(
                        Box::new(integer_atom!(3)),
                        Box::new(integer_atom!(4))
                    )),
                    Box::new(Expression::Multiply(
                        Box::new(integer_atom!(5)),
                        Box::new(integer_atom!(6))
                    )),
                    Box::new(Expression::Divide(
                        Box::new(integer_atom!(7)),
                        Box::new(integer_atom!(8))
                    )),
                    Box::new(Expression::Modulus(
                        Box::new(integer_atom!(9)),
                        Box::new(integer_atom!(10))
                    )),
                ],
                size: 5 as u8,
            }
        )
    }

    macro_rules! variable_atom {
        ( $i:expr ) => {
            Expression::Atom(Atom::Variable($i))
        };
    }

    #[test]
    fn test_function_in_vector() {
        assert_eq!(
            parse("<r*cos(t), r*sin(t), z*t>"),
            Expression::Vector {
                backing: vec![
                    Box::new(Expression::Multiply(
                        Box::new(variable_atom!('r')),
                        Box::new(Expression::Function {
                            name: "cos".to_string(),
                            args: vec![Box::new(variable_atom!('t'))].into_iter().collect()
                        })
                    )),
                    Box::new(Expression::Multiply(
                        Box::new(variable_atom!('r')),
                        Box::new(Expression::Function {
                            name: "sin".to_string(),
                            args: vec![Box::new(variable_atom!('t'))].into_iter().collect()
                        })
                    )),
                    Box::new(Expression::Multiply(
                        Box::new(variable_atom!('z')),
                        Box::new(variable_atom!('t'))
                    ))
                ],
                size: 3 as u8,
            }
        )
    }

    #[test]
    fn test_vector_in_function() {
        assert_eq!(
            parse("dot(<1, 2, 3>, <4, 5, 6>)"),
            Expression::Function {
                name: "dot".to_string(),
                args: vec![
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(integer_atom!(1)),
                            Box::new(integer_atom!(2)),
                            Box::new(integer_atom!(3)),
                        ],
                        size: 3 as u8,
                    }),
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(integer_atom!(4)),
                            Box::new(integer_atom!(5)),
                            Box::new(integer_atom!(6)),
                        ],
                        size: 3 as u8,
                    }),
                ]
                .into_iter()
                .collect()
            }
        )
    }

    #[test]
    fn test_square_matrix() {
        assert_eq!(
            parse("[1, 2; 3, 4]"),
            Expression::Matrix {
                backing: vec![
                    Box::new(integer_atom!(1)),
                    Box::new(integer_atom!(2)),
                    Box::new(integer_atom!(3)),
                    Box::new(integer_atom!(4)),
                ],
                shape: (2, 2),
            }
        )
    }

    #[test]
    fn test_column_rectangular_matrix() {
        assert_eq!(
            parse("[1, 2, 3; 4, 5, 6]"),
            Expression::Matrix {
                backing: vec![
                    Box::new(integer_atom!(1)),
                    Box::new(integer_atom!(2)),
                    Box::new(integer_atom!(3)),
                    Box::new(integer_atom!(4)),
                    Box::new(integer_atom!(5)),
                    Box::new(integer_atom!(6)),
                ],
                shape: (2, 3),
            }
        )
    }

    #[test]
    fn test_row_rectangular_matrix() {
        assert_eq!(
            parse("[1, 2; 3, 4; 5, 6]"),
            Expression::Matrix {
                backing: vec![
                    Box::new(integer_atom!(1)),
                    Box::new(integer_atom!(2)),
                    Box::new(integer_atom!(3)),
                    Box::new(integer_atom!(4)),
                    Box::new(integer_atom!(5)),
                    Box::new(integer_atom!(6)),
                ],
                shape: (3, 2),
            }
        )
    }

    #[test]
    fn test_matrix_in_expression() {
        assert_eq!(
            parse("1 + [2, 3; 4, 5]"),
            Expression::Add(
                Box::new(integer_atom!(1)),
                Box::new(Expression::Matrix {
                    backing: vec![
                        Box::new(integer_atom!(2)),
                        Box::new(integer_atom!(3)),
                        Box::new(integer_atom!(4)),
                        Box::new(integer_atom!(5)),
                    ],
                    shape: (2, 2),
                })
            )
        )
    }

    #[test]
    fn test_expression_in_matrix() {
        assert_eq!(
            parse("[1 + 2 - 3, 4 * 5 / 6; 7 % 8, cos(x)]"),
            Expression::Matrix {
                backing: vec![
                    Box::new(Expression::Subtract(
                        Box::new(Expression::Add(
                            Box::new(integer_atom!(1)),
                            Box::new(integer_atom!(2)),
                        )),
                        Box::new(integer_atom!(3),),
                    )),
                    Box::new(Expression::Divide(
                        Box::new(Expression::Multiply(
                            Box::new(integer_atom!(4)),
                            Box::new(integer_atom!(5)),
                        ),),
                        Box::new(integer_atom!(6),),
                    )),
                    Box::new(Expression::Modulus(
                        Box::new(integer_atom!(7)),
                        Box::new(integer_atom!(8)),
                    )),
                    Box::new(Expression::Function {
                        name: "cos".to_string(),
                        args: vec![Box::new(variable_atom!('x'))].into_iter().collect(),
                    })
                ],
                shape: (2, 2),
            }
        )
    }

    #[test]
    fn test_matrix_in_function() {
        assert_eq!(
            parse("T([1, 2; 3, 4])"),
            Expression::Function {
                name: "T".to_string(),
                args: vec![Box::new(Expression::Matrix {
                    backing: vec![
                        Box::new(integer_atom!(1)),
                        Box::new(integer_atom!(2)),
                        Box::new(integer_atom!(3)),
                        Box::new(integer_atom!(4)),
                    ],
                    shape: (2, 2),
                }),]
                .into_iter()
                .collect(),
            }
        )
    }

    #[test]
    fn test_matrix_paren() {
        assert_eq!(
            parse("([1, 2; 3, 4])"),
            Expression::Matrix {
                backing: vec![
                    Box::new(integer_atom!(1)),
                    Box::new(integer_atom!(2)),
                    Box::new(integer_atom!(3)),
                    Box::new(integer_atom!(4)),
                ],
                shape: (2, 2)
            }
        )
    }

    #[test]
    fn test_vector_paren() {
        assert_eq!(
            parse("(<1, 2, 3>)"),
            Expression::Vector {
                backing: vec![
                    Box::new(integer_atom!(1)),
                    Box::new(integer_atom!(2)),
                    Box::new(integer_atom!(3)),
                ],
                size: 3,
            }
        )
    }
}
