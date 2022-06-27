use std::{io, str::FromStr};

use rcas::Expression;

fn main() {
    let mut map = heapless::LinearMap::<char, Expression<500>, 52>::new();
    map.insert('x', rcas::Expression::<500>::from_str("30+20").unwrap()).unwrap(); //simple variable test case

    loop {
        println!("Enter a math expression: ");

        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let result = rcas::parse_approximation(&input, &map);

        if result.is_ok() {
            println!("{}", result.unwrap());
        } else {
            println!("{:?}", result.unwrap_err());
        }
    }
}
