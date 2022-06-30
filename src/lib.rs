#![no_std]

mod expression;
mod parser;
mod token;
mod expression_map;

pub use expression::{Approx, Expression};
pub use expression_map::{ExpressionMap, VariableMap,UserFunctionMap,};
pub use parser::approx as parse_approximation;
pub use parser::math_expr as parse_math_expression;

#[derive(Debug)]
pub enum Error {
    NotEnoughMemory,
}
