use std::{io, str::FromStr};

fn main() {
    let mut map = rcas::ValueMap::new();
    map.insert('x', rcas::Expression::from_str("30+20").unwrap()); //simple variable test case

    loop {
        println!("Enter a math expression: ");
        
        let mut input = String::new();

        io::stdin().read_line(&mut input).expect("Failed to read line");

        println!("{}", rcas::parse_approx(&input, &map));
    }
}
