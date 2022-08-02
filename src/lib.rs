#![no_std]

extern crate alloc;

mod parser;
mod expression_tree;
mod modifier;

#[derive(Debug)]
pub enum Error {
    NotEnoughMemory,
    InvalidSyntax,
    UndefinedSymbol,
}
