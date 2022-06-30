use std::{io, str::FromStr};
use heapless::{String, Vec};

use rcas::{Expression, VariableMap, ExpressionMap, UserFunctionMap,Error,};

fn main() {
    let mut var_map = VariableMap::<500, 52>::new();
    var_map.insert('x', Expression::<500>::from_str("30+20").unwrap()).unwrap(); //simple variable test case

    let mut func_map = UserFunctionMap::<500, 52, 16>::new();
    func_map.insert_function(String::<8>::from_str("test").unwrap(), Expression::<500>::from_str("y+z+20").unwrap()).unwrap(); //simple function test case
    func_map.insert_arguments(String::<8>::from_str("test").unwrap(), Vec::from_slice(&['y', 'z']).unwrap())
        .map_err(|_| Error::NotEnoughMemory).unwrap(); //simple argument test case

    let mut map_collection: Vec<&dyn ExpressionMap<500>, 2> = Vec::new();
    map_collection.push(&var_map as &dyn ExpressionMap<500>)
        .map_err(|_| Error::NotEnoughMemory).unwrap();
    map_collection.push(&func_map as &dyn ExpressionMap<500>)
        .map_err(|_| Error::NotEnoughMemory).unwrap();

    loop {
        println!("Enter a math expression: ");

        let mut input = std::string::String::new();

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
