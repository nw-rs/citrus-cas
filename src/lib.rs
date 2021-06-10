//#![no_std]

#[macro_use]
extern crate nom;

use core::str::FromStr;

use nom::IResult;

named!(decimal<&str, u32>,
  map!(recognize!(many1!(one_of!("0123456789"))), |s:&str|
    dbg!(u32::from_str_radix(dbg!(s), 10).unwrap()
  )
));

#[derive(Debug)]
pub enum Token {
    Add,
    Subtract,
    Divide,
    Multiply,
    Number(f32),
}

named!(float<&str, f32>,
  alt!(
    // Case one: .42
    map_res!(
      recognize!(tuple!(char!('.'),
        decimal,
        opt!(tuple!(one_of!("eE"), opt!(one_of!("+-")), decimal))
      )), |s:&str| {
        f32::from_str(s)
      }
    )
  | // Case two: 42e42 and 42.42e42
    map_res!(
      recognize!(tuple!(
        decimal,
        opt!(preceded!(char!('.'), decimal)),
        tuple!(one_of!("eE"), opt!(one_of!("+-")), decimal)
      )), |s:&str| {
        f32::from_str(s)
      }
    )
  | // Case three: 42. and 42.42
    map_res!(
      recognize!(tuple!(
        decimal,
        char!('.'),
        opt!(decimal)
      )), |s:&str| {
        f32::from_str(s)
      }
    )
  | // Case four: 42
    map_res!(decimal, |u: u32| {Ok::<_, nom::Err<nom::error::ErrorKind>>(u as f32)}
)));

pub fn test(input: &str) -> IResult<&str, f32> {
    float(dbg!(input))
}
