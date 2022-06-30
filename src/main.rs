use std::{io, str::FromStr};

use rcas::{Expression, VariableMap, ExpressionMap,};

fn main() {
    let mut var_map = VariableMap::<500, 52>::new();
    var_map.insert('x', Expression::<500>::from_str("30+20").unwrap()).unwrap(); //simple variable test case
    let mut map_collection: heapless::Vec<&dyn ExpressionMap<500>, 1> = heapless::Vec::new();
    map_collection.push(&var_map as &dyn ExpressionMap<500>);

    loop {
        println!("Enter a math expression: ");

        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let result = rcas::parse_approximation(&input, &map_collection);

        if result.is_ok() {
            println!("{}", result.unwrap());
        } else {
            println!("{:?}", result.unwrap_err());
        }
    }
}
