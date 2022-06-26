use std::{io, str::FromStr};

fn main() {
    let mut map = rcas::ValueMap::<1000, 52>::new();
    map.insert('x', rcas::Expression::<1000>::from_str("30+20").unwrap()); //simple variable test case

    loop {
        println!("Enter a math expression: ");
        
        let mut input = String::new();

        io::stdin().read_line(&mut input).expect("Failed to read line");

        println!("{}", rcas::parse_approx(&input, &map));
    }
}
