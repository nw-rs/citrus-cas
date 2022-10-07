#![no_std]

extern crate alloc;

pub mod expression;
pub mod modifier;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    NotEnoughMemory,
    InvalidSyntax,
    UndefinedSymbol,
}
