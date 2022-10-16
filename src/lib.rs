#![no_std]

#[cfg(feature = "std")]
extern crate std;

use core::fmt;

extern crate alloc;

pub mod expression;
pub mod modifier;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Error {
    NotEnoughMemory,
    InvalidSyntax,
    UndefinedSymbol,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::NotEnoughMemory => write!(f, "Not enough memory"),
            Error::InvalidSyntax => write!(f, "Invalid syntax"),
            Error::UndefinedSymbol => write!(f, "Undefined symbol"),
        }
    }
}
