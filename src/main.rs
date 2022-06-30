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

        let eval_result = rcas::parse_evaluation::<500>(&input);
        let approx_result = rcas::parse_approximation(&input, &map);

        if eval_result.is_ok() {
            println!("eval: {}", eval_result.unwrap().to_string());
        } else {
            println!("eval: {:?}", eval_result.unwrap_err());
        }

        if approx_result.is_ok() {
            println!("approx: {}", approx_result.unwrap().to_string());
        } else {
            println!("approx: {:?}", approx_result.unwrap_err());
        }
    }
}
