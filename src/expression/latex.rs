use alloc::format;
use alloc::string::String;
use alloc::vec;
use alloc::{boxed::Box, string::ToString, vec::Vec};

use nom::bytes::complete::{tag, take_while};
use nom::multi::separated_list0;
use nom::sequence::pair;
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

use super::expression_tree::Escape;

pub fn parse(input: &str) -> Expression {
    parse_add_sub(input)
        .map_err(|_| "failed to parse")
        .unwrap()
        .1
}

fn parse_recursive(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_parentheses,
        parse_frac,
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
        alt((
            delimited(
                alt((tag("("), tag("\\left("))),
                parse_add_sub,
                alt((tag(")"), tag("\\right)"))),
            ),
            delimited(
                alt((tag("{"), tag("\\left{"))),
                parse_add_sub,
                alt((tag("}"), tag("\\right}"))),
            ),
        )),
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
                preceded(
                    space0,
                    alt((
                        preceded(
                            tag("\\"),
                            pair(
                                take_while1(|c: char| c.is_alphabetic()),
                                take_while(|c: char| c.is_alphanumeric()),
                            ),
                        ),
                        pair(
                            take_while1(|c: char| c.is_alphabetic()),
                            take_while(|c: char| c.is_alphanumeric()),
                        ),
                    )),
                ),
                preceded(
                    alt((tag("("), tag("\\left("))),
                    many0(terminated(
                        parse_add_sub,
                        alt((tag(","), tag(")"), tag("\\right)"))),
                    )),
                ),
            )),
            space0,
        ),
        |(name, arg_list)| Expression::Function {
            name: name.0.to_string() + name.1,
            args: arg_list.into_iter().map(Box::new).collect(),
        },
    )(input)
}

// TODO: fix vector and matrix parsing
fn parse_vector(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0,
            preceded(
                tag("\\begin{vmatrix}"),
                many0(terminated(
                    parse_add_sub,
                    alt((tag("\\\\"), tag("\\end{vmatrix}"))),
                )),
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
            preceded(
                tag("\\begin{bmatrix}"),
                many0(terminated(
                    separated_list0(char('&'), parse_add_sub),
                    alt((tag("\\\\"), tag("\\end{bmatrix}"))),
                )),
            ),
            space0,
        ),
        |flatten_matrix| {
            let row_count = flatten_matrix.len() as u8;
            let col_count = if row_count == 0 {
                0u8
            } else {
                flatten_matrix[0].len() as u8
            }; // assuming every row has the same number of columns

            let backing = flatten_matrix.into_iter().flatten().map(Box::new).collect();
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
                match value.chars().next().unwrap() {
                    'A' => Escape::Atom,
                    'F' => Escape::Function,
                    'V' => Escape::Vector,
                    'M' => Escape::Matrix,
                    '*' => Escape::Everything,
                    _ => unreachable!(),
                },
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
    let (input, ops) = many0(tuple((tag("^"), parse_exponents)))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn parse_unary_prefix(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(space0, tuple((tag("-"), parse_unary)), space0),
        parse_unary_prefix_op,
    )(input)
}

fn parse_unary_postfix(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(space0, tuple((parse_exponents, tag("!"))), space0),
        parse_unary_postfix_op,
    )(input)
}

fn parse_frac(input: &str) -> IResult<&str, Expression> {
    map(
        delimited(
            space0,
            delimited(
                tag("\\frac"),
                tuple((
                    delimited(char('{'), parse_add_sub, char('}')),
                    delimited(char('{'), parse_add_sub, char('}')),
                )),
                space0,
            ),
            space0,
        ),
        |(num, den)| Expression::Divide(Box::new(num), Box::new(den)),
    )(input)
}

fn parse_mult_div_mod(input: &str) -> IResult<&str, Expression> {
    let (input, num) = parse_unary(input)?;
    let (input, ops) = many0(tuple((
        alt((tag("\\cdot"), tag("/"), tag("%"))),
        parse_unary,
    )))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn parse_add_sub(input: &str) -> IResult<&str, Expression> {
    let (input, num) = parse_mult_div_mod(input)?;
    let (input, ops) = many0(tuple((alt((tag("+"), tag("-"))), parse_mult_div_mod)))(input)?;
    Ok((input, fold_binary_operators(num, ops)))
}

fn parse_unary_prefix_op(operator_pair: (&str, Expression)) -> Expression {
    let (operator, operand) = operator_pair;
    match operator {
        "-" => Expression::Negate(Box::new(operand)),
        _ => panic!("Invalid operator"),
    }
}

fn parse_unary_postfix_op(operator_pair: (Expression, &str)) -> Expression {
    let (operand, operator) = operator_pair;
    match operator {
        "!" => Expression::Factorial(Box::new(operand)),
        _ => panic!("Invalid operator"),
    }
}

fn fold_binary_operators(expr: Expression, ops: Vec<(&str, Expression)>) -> Expression {
    ops.into_iter()
        .fold(expr, |acc, val| parse_binary_op(val, acc))
}

fn parse_binary_op(operator_pair: (&str, Expression), expr1: Expression) -> Expression {
    let (operator, expr2) = operator_pair;
    match operator {
        "+" => Expression::Add(Box::new(expr1), Box::new(expr2)),
        "-" => Expression::Subtract(Box::new(expr1), Box::new(expr2)),
        "\\cdot" => Expression::Multiply(Box::new(expr1), Box::new(expr2)),
        "/" => Expression::Divide(Box::new(expr1), Box::new(expr2)),
        "^" => Expression::Power(Box::new(expr1), Box::new(expr2)),
        "%" => Expression::Modulus(Box::new(expr1), Box::new(expr2)),
        _ => panic!("Invalid operator"),
    }
}

pub fn latexify(expr: &Expression) -> String {
    match expr {
        Expression::Atom(a) => a.to_string(),

        Expression::Negate(e) => match **e {
            Expression::Atom(_) => "-".to_string() + &latexify(&e),
            _ => format!("-\\left({}\\right)", &latexify(&e)),
        },
        Expression::Factorial(e) => match **e {
            Expression::Atom(_) => format!("{}!", &latexify(&e)),
            _ => format!("\\left({}\\right)!", &latexify(&e)),
        },
        Expression::Percent(e) => match **e {
            Expression::Atom(_) => format!("{}%", &latexify(&e)),
            _ => format!("\\left({}\\right)%", &latexify(&e)),
        },

        Expression::Add(l, r) => format!("{}+{}", &latexify(&l), &latexify(&r)),
        Expression::Subtract(l, r) => format!("{}-{}", &latexify(&l), &latexify(&r)),
        Expression::Modulus(l, r) => format!("{}%{}", &latexify(&l), &latexify(&r)),

        Expression::Multiply(l, r) => {
            format!(
                "{}\\cdot{}",
                match **l {
                    Expression::Add(_, _)
                    | Expression::Subtract(_, _)
                    | Expression::Modulus(_, _) => format!("\\left({}\\right)", &latexify(&l)),
                    _ => format!("{}", &latexify(&l)),
                },
                match **r {
                    Expression::Add(_, _)
                    | Expression::Subtract(_, _)
                    | Expression::Modulus(_, _) => format!("\\left({}\\right)", &latexify(&r)),
                    _ => format!("{}", &latexify(&r)),
                }
            )
        }

        Expression::Divide(l, r) => {
            format!("\\frac{{{}}}{{{}}}", &latexify(&l), &latexify(&r))
        }

        Expression::Power(l, r) => {
            format!(
                "{}^{}",
                match **l {
                    Expression::Add(_, _)
                    | Expression::Subtract(_, _)
                    | Expression::Modulus(_, _)
                    | Expression::Multiply(_, _)
                    | Expression::Divide(_, _) => format!("\\left({}\\right)", &latexify(&l)),
                    _ => format!("{}", &latexify(&l)),
                },
                match **r {
                    Expression::Atom(_) => format!("{}", &latexify(&r)),
                    _ => format!("{{{}}}", &latexify(&r)),
                }
            )
        }

        Expression::Function { name, args } => {
            let mut out = match name.as_str() {
                "cos" | "csc" | "exp" | "ker" | "limsup" | "min" | "sinh" | "arcsin" | "cosh"
                | "deg" | "gcd" | "lg" | "ln" | "Pr" | "sup" | "arctan" | "cot" | "det" | "hom"
                | "lim" | "log" | "sec" | "tan" | "arg" | "coth" | "dim" | "liminf" | "max"
                | "sin" | "tanh" => format!("\\{}\\left(", name),
                _ => format!("{}\\left(", name),
            };
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out += ",";
                }
                out += &latexify(&arg);
            }
            format!("{}\\right)", out)
        }

        Expression::Vector {
            backing: vec,
            size: _,
        } => {
            format!("\\begin{{vmatrix}}{}\\end{{vmatrix}}", {
                let mut out = String::new();

                for (id, arg) in vec.iter().enumerate() {
                    out = format!("{}{}", out, &latexify(&arg));
                    if id != vec.len() - 1 {
                        out += "\\\\";
                    }
                }

                out
            })
        }

        Expression::Matrix {
            backing: vec,
            shape: (rs, cs),
        } => {
            format!("\\begin{{bmatrix}}{}\\end{{bmatrix}}", {
                let mut out = String::new();

                for r in 0..*rs {
                    if r > 0 {
                        out += "\\\\";
                    }
                    for c in 0..*cs {
                        if c > 0 {
                            out += "&";
                        }
                        out = format!("{}{}", out, &latexify(&vec[(*cs * r + c) as usize]));
                    }
                }

                out
            })
        }
    }
}

mod tests {
    use alloc::{boxed::Box, string::ToString, vec};

    use crate::expression::expression_tree::{Atom, Escape, Expression, Numeric};

    use super::{latexify, parse};

    #[test]
    fn latex_test_add() {
        assert_eq!(
            parse("1 + 2"),
            Expression::Add(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn latex_test_subtract() {
        assert_eq!(
            parse("1 - 2"),
            Expression::Subtract(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn latex_test_add_negative() {
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
    fn latex_test_modulus() {
        assert_eq!(
            parse("1 % 2"),
            Expression::Modulus(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn latex_test_multiply() {
        assert_eq!(
            parse("1\\cdot2"),
            Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn latex_test_divide() {
        assert_eq!(
            parse("1 / 2"),
            Expression::Divide(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn latex_test_multiply_negative() {
        assert_eq!(
            parse("1\\cdot-2"),
            Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Negate(Box::new(Expression::Atom(
                    Atom::Numeric(Numeric::Integer(2))
                ))))
            )
        );
    }

    #[test]
    fn latex_test_exponent() {
        assert_eq!(
            parse("1 ^ 2"),
            Expression::Power(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2))))
            )
        );
    }

    #[test]
    fn latex_test_factorial() {
        assert_eq!(
            parse("5!"),
            Expression::Factorial(Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(
                5
            )))))
        );
    }

    #[test]
    fn latex_test_parentheses() {
        assert_eq!(
            parse("(1 + 2)\\cdot5"),
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
    fn latex_test_nested_parentheses() {
        assert_eq!(
            parse("(5 \\cdot (4 + (6 / 3)))"),
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
    fn latex_test_spaceless() {
        assert_eq!(
            parse("1+2\\cdot5"),
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
    fn latex_test_whitespace() {
        assert_eq!(
            parse(" 1    +  2 \\cdot   5  "),
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
    fn latex_test_multi_level_expression() {
        assert_eq!(
            parse("1 \\cdot 2 + 3 / 4 ^ 6 % 7"),
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

    #[test]
    fn complex_latex() {
        assert_eq!(
            parse("\\frac{5}{6}\\cdot5+\\left(4^{2+x}\\right)-1!+arc\\left(6\\right)"),
            Expression::Add(
                Box::new(Expression::Subtract(
                    Box::new(Expression::Add(
                        Box::new(Expression::Multiply(
                            Box::new(Expression::Divide(
                                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6))))
                            )),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5))))
                        )),
                        Box::new(Expression::Power(
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                            Box::new(Expression::Add(
                                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                                Box::new(Expression::Atom(Atom::Variable('x')))
                            ))
                        ))
                    )),
                    Box::new(Expression::Factorial(Box::new(Expression::Atom(
                        Atom::Numeric(Numeric::Integer(1))
                    ))))
                )),
                Box::new(Expression::Function {
                    name: "arc".to_string(),
                    args: vec![Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(
                        6
                    ))))],
                })
            )
        )
    }

    #[test]
    fn recursive_latex() {
        assert_eq!(
            parse("5+(6+7)+8"),
            Expression::Add(
                Box::new(Expression::Add(
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                    Box::new(Expression::Add(
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7))))
                    ))
                )),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(8))))
            )
        )
    }

    #[test]
    fn latex_test_integer() {
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
    fn latex_test_decimal() {
        assert_eq!(
            parse("1.0"),
            Expression::Atom(Atom::Numeric(Numeric::Decimal(1.0)))
        );
    }

    #[test]
    fn latex_test_escape() {
        assert_eq!(
            parse("_A2"),
            Expression::Atom(Atom::Escape(Escape::Atom, 2))
        );
    }

    #[test]
    fn latex_test_wildcard_escape() {
        assert_eq!(
            parse("_*0"),
            Expression::Atom(Atom::Escape(Escape::Everything, 0))
        );
    }

    #[test]
    fn latex_test_variable() {
        assert_eq!(parse("x"), Expression::Atom(Atom::Variable('x')));
    }

    #[test]
    fn latex_test_unicode_variable() {
        assert_eq!(parse("π"), Expression::Atom(Atom::Variable('π')));
    }

    #[test]
    fn latex_test_function() {
        assert_eq!(
            parse("\\sin(1 + -2)"),
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
    fn latex_test_function_empty() {
        assert_eq!(
            parse("\\sin()"),
            Expression::Function {
                name: "sin".to_string(),
                args: vec![].into_iter().collect(),
            }
        );
    }

    #[test]
    fn latex_test_multiple_arguments() {
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
    fn latex_test_advanced_function() {
        assert_eq!(
            parse("5 \\cdot \\log(10, \\sin(x))"),
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
    fn latex_test_matrix() {
        assert_eq!(
            parse("\\begin{bmatrix} 1 & 2 \\\\ 3 & 4 \\end{bmatrix}"),
            Expression::Matrix {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                ],
                shape: (2, 2),
            }
        )
    }

    #[test]
    fn latex_test_matrix_in_expression() {
        assert_eq!(
            parse("5 \\cdot \\begin{bmatrix} 1 & 2 \\\\ 3 & 4 \\end{bmatrix}"),
            Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Matrix {
                    backing: vec![
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                    ],
                    shape: (2, 2),
                })
            )
        )
    }

    #[test]
    fn latex_test_matrix_in_matrix() {
        assert_eq!(
            parse("\\begin{bmatrix} 1 \\\\ \\begin{bmatrix} 3 & 4 \\end{bmatrix} \\end{bmatrix}"),
            Expression::Matrix {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Matrix {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                        ],
                        shape: (1, 2),
                    }),
                ],
                shape: (2, 1),
            }
        )
    }

    #[test]
    fn latex_test_matrix_in_function() {
        assert_eq!(
            parse("\\sin(\\begin{bmatrix} 1 & 2 \\\\ 3 & 4 \\end{bmatrix})"),
            Expression::Function {
                name: "sin".to_string(),
                args: vec![Box::new(Expression::Matrix {
                    backing: vec![
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                    ],
                    shape: (2, 2),
                }),],
            }
        )
    }

    #[test]
    fn latex_test_expression_in_matrix() {
        assert_eq!(
            parse("\\begin{bmatrix} 1 & 2 \\\\ 3 & 5 \\cdot 4 \\end{bmatrix}"),
            Expression::Matrix {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                    Box::new(Expression::Multiply(
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                    )),
                ],
                shape: (2, 2),
            }
        )
    }

    #[test]
    fn latex_test_vector() {
        assert_eq!(
            parse("\\begin{vmatrix} 1 \\\\ 2 \\\\ 3 \\\\ 4 \\end{vmatrix}"),
            Expression::Vector {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                ],
                size: 4,
            }
        )
    }

    #[test]
    fn latex_test_vector_in_expression() {
        assert_eq!(
            parse("5 \\cdot \\begin{vmatrix} 1 \\\\ 2 \\\\ 3 \\\\ 4 \\end{vmatrix}"),
            Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Vector {
                    backing: vec![
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                    ],
                    size: 4,
                })
            )
        )
    }

    #[test]
    fn latex_test_vector_in_vector() {
        assert_eq!(
            parse(
                "\\begin{vmatrix} 1 \\\\ \\begin{vmatrix} 3 \\\\ 4 \\end{vmatrix} \\end{vmatrix}"
            ),
            Expression::Vector {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                        ],
                        size: 2,
                    }),
                ],
                size: 2,
            }
        )
    }

    #[test]
    fn latex_test_vector_in_function() {
        assert_eq!(
            parse("\\sin(\\begin{vmatrix} 1 \\\\ 2 \\\\ 3 \\\\ 4 \\end{vmatrix})"),
            Expression::Function {
                name: "sin".to_string(),
                args: vec![Box::new(Expression::Vector {
                    backing: vec![
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                    ],
                    size: 4,
                }),],
            }
        );
    }

    #[test]
    fn latex_test_expression_in_vector() {
        assert_eq!(
            parse("\\begin{vmatrix} 1 \\\\ 2 \\\\ 3 \\\\ 5 \\cdot 4 \\end{vmatrix}"),
            Expression::Vector {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                    Box::new(Expression::Multiply(
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                    )),
                ],
                size: 4,
            }
        )
    }

    #[test]
    fn latex_test_vector_in_matrix() {
        assert_eq!(
            parse("\\begin{bmatrix} 1 & 2 \\\\ 3 & \\begin{vmatrix} 5 \\\\ 6 \\end{vmatrix} \\end{bmatrix}"),
            Expression::Matrix {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                        ],
                        size: 2,
                    }),
                ],
                shape: (2, 2),
            }
        )
    }

    #[test]
    fn latex_test_matrix_in_vector() {
        assert_eq!(
            parse("\\begin{vmatrix} 1 \\\\ 2 \\\\ 3 \\\\ \\begin{bmatrix} 5 & 6 \\\\ 7 & 8 \\end{bmatrix} \\end{vmatrix}"),
            Expression::Vector {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(1)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(3)))),
                    Box::new(Expression::Matrix {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(8)))),
                        ],
                        shape: (2, 2),
                    }),
                ],
                size: 4,
            }
        )
    }

    #[test]
    fn integer_string_latex() {
        assert_eq!(
            "5",
            latexify(&Expression::Atom(Atom::Numeric(Numeric::Integer(5))))
        );
    }

    #[test]
    fn decimal_string_latex() {
        assert_eq!(
            "5",
            latexify(&Expression::Atom(Atom::Numeric(Numeric::Decimal(5.0))))
        );
        assert_eq!(
            "5.7",
            latexify(&Expression::Atom(Atom::Numeric(Numeric::Decimal(5.7))))
        );
    }

    #[test]
    fn variable_string_latex() {
        assert_eq!("x", latexify(&Expression::Atom(Atom::Variable('x'))));
    }

    #[test]
    fn unicode_variable_string_latex() {
        assert_eq!("π", latexify(&Expression::Atom(Atom::Variable('π'))));
    }

    #[test]
    fn escape_string_latex() {
        assert_eq!(
            "_A2",
            latexify(&Expression::Atom(Atom::Escape(Escape::Atom, 2)))
        );
    }

    #[test]
    fn wildcard_escape_string_latex() {
        assert_eq!(
            "_*0",
            latexify(&Expression::Atom(Atom::Escape(Escape::Everything, 0)))
        );
    }

    #[test]
    fn negate_string_latex() {
        assert_eq!(
            "-5",
            latexify(&Expression::Negate(Box::new(Expression::Atom(
                Atom::Numeric(Numeric::Integer(5))
            ))))
        );
    }

    #[test]
    fn add_string_latex() {
        assert_eq!(
            "5+6",
            latexify(&Expression::Add(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6))))
            ))
        );
    }

    #[test]
    fn subtract_string_latex() {
        assert_eq!(
            "5-6",
            latexify(&Expression::Subtract(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6))))
            ))
        );
    }

    #[test]
    fn multiply_string_latex() {
        assert_eq!(
            "5\\cdot6",
            latexify(&Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6))))
            ))
        );
    }

    #[test]
    fn divide_string_latex() {
        assert_eq!(
            "\\frac{5}{6}",
            latexify(&Expression::Divide(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6))))
            ))
        );
    }

    #[test]
    fn power_string_latex() {
        assert_eq!(
            "5^6",
            latexify(&Expression::Power(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6))))
            ))
        );
    }

    #[test]
    fn complex_power_string_latex() {
        assert_eq!(
            "5^{6^7}",
            latexify(&Expression::Power(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Power(
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7))))
                ))
            ))
        );
    }

    #[test]
    fn factorial_string_latex() {
        assert_eq!(
            "5!",
            latexify(&Expression::Factorial(Box::new(Expression::Atom(
                Atom::Numeric(Numeric::Integer(5))
            ))))
        );
    }

    #[test]
    fn function_string_latex() {
        assert_eq!(
            "\\sin\\left(5\\right)",
            latexify(&Expression::Function {
                name: "sin".to_string(),
                args: vec![Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(
                    5
                ))))]
                .into_iter()
                .collect(),
            })
        );
    }

    #[test]
    fn function_string_latex_empty() {
        assert_eq!(
            "\\sin\\left(\\right)",
            latexify(&Expression::Function {
                name: "sin".to_string(),
                args: vec![].into_iter().collect(),
            })
        );
    }

    #[test]
    fn function_string_latex_multiple() {
        assert_eq!(
            "\\sin\\left(5,6\\right)",
            latexify(&Expression::Function {
                name: "sin".to_string(),
                args: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                ]
                .into_iter()
                .collect(),
            })
        );
    }

    #[test]
    fn function_string_latex_advanced() {
        assert_eq!(
            "\\sin\\left(5\\cdot\\log\\left(6\\right)\\right)",
            latexify(&Expression::Function {
                name: "sin".to_string(),
                args: vec![Box::new(Expression::Multiply(
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                    Box::new(Expression::Function {
                        name: "log".to_string(),
                        args: vec![Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(
                            6
                        ))))],
                    })
                ))]
                .into_iter()
                .collect(),
            })
        );
    }

    #[test]
    fn vector_string_latex() {
        assert_eq!(
            "\\begin{vmatrix}5\\\\6\\end{vmatrix}",
            latexify(&Expression::Vector {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                ],
                size: 2,
            })
        );
    }

    #[test]
    fn vector_in_expression_string_latex() {
        assert_eq!(
            "5\\cdot\\begin{vmatrix}6\\\\7\\end{vmatrix}",
            latexify(&Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Vector {
                    backing: vec![
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7)))),
                    ],
                    size: 2,
                })
            ))
        );
    }

    #[test]
    fn vector_in_function_string_latex() {
        assert_eq!(
            "\\sin\\left(\\begin{vmatrix}5\\\\6\\end{vmatrix}\\right)",
            latexify(&Expression::Function {
                name: "sin".to_string(),
                args: vec![Box::new(Expression::Vector {
                    backing: vec![
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                    ],
                    size: 2,
                })],
            })
        );
    }

    #[test]
    fn vector_in_vector_string_latex() {
        assert_eq!(
            "\\begin{vmatrix}\\begin{vmatrix}5\\\\6\\end{vmatrix}\\\\\\begin{vmatrix}7\\\\8\\end{vmatrix}\\end{vmatrix}",
            latexify(&Expression::Vector {
                backing: vec![
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                        ],
                        size: 2,
                    }),
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(8)))),
                        ],
                        size: 2,
                    }),
                ],
                size: 2,
            })
        );
    }

    #[test]
    fn matrix_string_latex() {
        assert_eq!(
            "\\begin{bmatrix}5&6\\\\7&8\\end{bmatrix}",
            latexify(&Expression::Matrix {
                backing: vec![
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7)))),
                    Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(8)))),
                ],
                shape: (2, 2),
            })
        );
    }

    #[test]
    fn matrix_in_expression_string_latex() {
        assert_eq!(
            "5\\cdot\\begin{bmatrix}6&7\\\\8&9\\end{bmatrix}",
            latexify(&Expression::Multiply(
                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                Box::new(Expression::Matrix {
                    backing: vec![
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(8)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(9)))),
                    ],
                    shape: (2, 2),
                })
            ))
        );
    }

    #[test]
    fn matrix_in_function_string_latex() {
        assert_eq!(
            "\\sin\\left(\\begin{bmatrix}5&6\\\\7&8\\end{bmatrix}\\right)",
            latexify(&Expression::Function {
                name: "sin".to_string(),
                args: vec![Box::new(Expression::Matrix {
                    backing: vec![
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7)))),
                        Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(8)))),
                    ],
                    shape: (2, 2),
                })],
            })
        );
    }

    #[test]
    fn matrix_in_matrix_string_latex() {
        assert_eq!(
            "\\begin{bmatrix}\\begin{bmatrix}5&6\\\\7&8\\end{bmatrix}&\\begin{bmatrix}9&10\\\\11&12\\end{bmatrix}\\\\\\begin{bmatrix}13&14\\\\15&16\\end{bmatrix}&\\begin{bmatrix}17&18\\\\19&20\\end{bmatrix}\\end{bmatrix}",
            latexify(&Expression::Matrix {
                backing: vec![
                    Box::new(Expression::Matrix {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(8)))),
                        ],
                        shape: (2, 2),
                    }),
                    Box::new(Expression::Matrix {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(9)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(10)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(11)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(12)))),
                        ],
                        shape: (2, 2),
                    }),
                    Box::new(Expression::Matrix {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(13)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(14)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(15)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(16)))),
                        ],
                        shape: (2, 2),
                    }),
                    Box::new(Expression::Matrix {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(17)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(18)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(19)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(20)))),
                        ],
                        shape: (2, 2),
                    }),
                ],
                shape: (2, 2),
            })
        );
    }

    #[test]
    fn vector_in_matrix_string_latex() {
        assert_eq!(
            "\\begin{bmatrix}\\begin{vmatrix}5\\\\6\\end{vmatrix}&\\begin{vmatrix}7\\\\8\\end{vmatrix}\\\\\\begin{vmatrix}9\\\\10\\end{vmatrix}&\\begin{vmatrix}11\\\\12\\end{vmatrix}\\end{bmatrix}",
            latexify(&Expression::Matrix {
                backing: vec![
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                        ],
                        size: 2,
                    }),
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(8)))),
                        ],
                        size: 2,
                    }),
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(9)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(10)))),
                        ],
                        size: 2,
                    }),
                    Box::new(Expression::Vector {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(11)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(12)))),
                        ],
                        size: 2,
                    }),
                ],
                shape: (2, 2),
            })
        );
    }

    #[test]
    fn matrix_in_vector_string_latex() {
        assert_eq!(
            "\\begin{vmatrix}\\begin{bmatrix}5&6\\\\7&8\\end{bmatrix}\\\\\\begin{bmatrix}9&10\\\\11&12\\end{bmatrix}\\end{vmatrix}",
            latexify(&Expression::Vector {
                backing: vec![
                    Box::new(Expression::Matrix {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(7)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(8)))),
                        ],
                        shape: (2, 2),
                    }),
                    Box::new(Expression::Matrix {
                        backing: vec![
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(9)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(10)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(11)))),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(12)))),
                        ],
                        shape: (2, 2),
                    }),
                ],
                size: 2,
            })
        );
    }

    #[test]
    fn complex_string_latex() {
        assert_eq!(
            "\\frac{5}{6}\\cdot5+4^{2+x}-1!+arc\\left(6\\right)",
            latexify(&Expression::Add(
                Box::new(Expression::Subtract(
                    Box::new(Expression::Add(
                        Box::new(Expression::Multiply(
                            Box::new(Expression::Divide(
                                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5)))),
                                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(6))))
                            )),
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(5))))
                        )),
                        Box::new(Expression::Power(
                            Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(4)))),
                            Box::new(Expression::Add(
                                Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(2)))),
                                Box::new(Expression::Atom(Atom::Variable('x')))
                            ))
                        ))
                    )),
                    Box::new(Expression::Factorial(Box::new(Expression::Atom(
                        Atom::Numeric(Numeric::Integer(1))
                    ))))
                )),
                Box::new(Expression::Function {
                    name: "arc".to_string(),
                    args: vec![Box::new(Expression::Atom(Atom::Numeric(Numeric::Integer(
                        6
                    ))))],
                })
            ))
        )
    }
}
