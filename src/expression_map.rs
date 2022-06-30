use heapless::{Vec, LinearMap, String,};

use crate::{token::Token, Expression, Error, Approx};

/// parses tokens which need modifiable definition
pub trait ExpressionMap<const E: usize> {
    fn stack_contains(&self, stack: &Expression<E>) -> bool;
    fn evaluate(&self, stack: &Expression<E>) -> Expression<E>;
    fn approximate(&self, stack: &Expression<E>) -> Expression<E>;
}

pub struct VariableMap<const E: usize, const L: usize> {
    map: LinearMap<char, Expression<E>, L>,
}

impl<const E: usize, const L: usize> ExpressionMap<E> for VariableMap<E, L> {
    fn stack_contains(&self, stack: &Expression<E>) -> bool {
        for token in stack.tokens.iter() {
            match token {
                Token::Var(_) => return true,
                _ => continue,
            }
        }
        return false;
    }

    fn evaluate(&self, stack: &Expression<E>) -> Expression<E> {
        Expression { tokens: stack.tokens.clone() }
    }

    fn approximate(&self, stack: &Expression<E>) -> Expression<E> {
        Expression { tokens: stack.tokens
        .iter()
        .map(|token| {
            match token {
                Token::Var(var) => {
                    if let Some(expr) = self.map.get(&var) {
                        match expr.approximate(&Vec::<&dyn ExpressionMap<E>, 1>::from_slice(&[self]).unwrap()) {
                            Ok(Approx::Num(n)) => Ok(Some(Token::Number(n))),
                            Ok(Approx::Undef) => Ok(None),
                            Err(err) => Err(err),
                        }
                    } else {
                        return Ok(None);
                    }
                }
                _ => Ok(Some(token.clone())),
            }
        })
        .filter_map(Result::transpose)
        .collect::<Result<Vec<Token, E>, Error>>()
        .unwrap() }
    }
}

impl<const E: usize, const L: usize> VariableMap<E, L> {
    pub fn new() -> Self {
        VariableMap {
            map: LinearMap::<char, Expression<E>, L>::new(),
        }
    }

    pub fn insert(&mut self, var: char, expr: Expression<E>) -> Result<Option<Expression<E>>, Error> {
        self.map.insert(var, expr).map_err(|_| Error::NotEnoughMemory)
    }

    pub fn get(&self, var: char) -> Option<&Expression<E>> {
        self.map.get(&var)
    }

    pub fn remove(&mut self, var: char) -> Option<Expression<E>> {
        self.map.remove(&var)
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }
}

pub struct UserFunctionMap<const E: usize, const L: usize, const V: usize> {
    function_map: LinearMap<String<8>, Expression<E>, L>,
    argument_list: LinearMap<String<8>, Vec<char, V>, L>,
}

impl<const E: usize, const L: usize, const V: usize> ExpressionMap<E> for UserFunctionMap<E, L, V> {
    fn stack_contains(&self, stack: &Expression<E>) -> bool {
        for token in stack.tokens.iter() {
            match token {
                Token::Func(_) => return true,
                _ => continue,
            }
        }
        return false;
    }

    fn evaluate(&self, stack: &Expression<E>) -> Expression<E> {
        Expression { tokens: stack.tokens.clone() }
    }

    fn approximate(&self, stack: &Expression<E>) -> Expression<E> {
        let mut new_tokens = Vec::<Token, E>::new();
        
        let mut skip = 0;
        for (i, token) in stack.tokens.iter().enumerate() {
            if skip > 0 { //there has to be a better way to do this
                skip -= 1;
                continue;
            }
            
            skip = 1; //skip the terminator
            match token {
                Token::Func(func) => {
                    let mut arguments: Vec<Vec::<Token, V>, V> = Vec::new();
                    let mut argument_approx: Vec<Approx, E> = Vec::new();
                    let mut argument_count = 0;

                    //fill the arguments vector with Vec::new()
                    for _ in 1..arguments.capacity() {
                        arguments.push(Vec::new()).map_err(|_| Error::NotEnoughMemory).unwrap();
                    }

                    for sub_token in stack.tokens.iter().skip(i + 1) {
                        match sub_token {
                            Token::Divider => argument_count += 1,
                            Token::Terminator => break,
                            _ => arguments[argument_count].push(sub_token.clone()).unwrap(),
                        }
                        skip += 1;
                    }

                    for vector in arguments.iter().take(argument_count + 1) {
                        argument_approx
                            .push(Expression::<V> { tokens: vector.clone() }
                            .approximate(&Vec::<&dyn ExpressionMap<V>, E>::new()).unwrap()).unwrap();
                    }

                    for sub_token in self.function_map.get(&func).unwrap().tokens.iter() {
                        match sub_token {
                            Token::Var(var) => {
                                if let Some(approx_token) = match argument_approx[self.argument_list.get(&func).unwrap().binary_search(var).unwrap()] {
                                    Approx::Num(n) => Some(Token::Number(n)),
                                    _ => unimplemented!(),
                                }{
                                    new_tokens.push(approx_token.clone()).unwrap();
                                } else {
                                    new_tokens.push(sub_token.clone()).unwrap();
                                }
                            }
                            _ => new_tokens.push(sub_token.clone()).unwrap(),
                        }
                    }
                }
                _ => new_tokens.push(token.clone()).unwrap(),
            }
        }

        Expression { tokens: new_tokens }
    }
}

impl<const E: usize, const L: usize, const V: usize> UserFunctionMap<E, L, V> {
    pub fn new() -> Self {
        UserFunctionMap {
            function_map: LinearMap::<String<8>, Expression<E>, L>::new(),
            argument_list: LinearMap::<String<8>, Vec<char, V>, L>::new(),
        }
    }

    pub fn insert_function(&mut self, name: String<8>, expr: Expression<E>) -> Result<Option<Expression<E>>, Error> {
        self.function_map.insert(name, expr).map_err(|_| Error::NotEnoughMemory)
    }

    pub fn insert_arguments(&mut self, name: String<8>, vars: Vec<char, V>) -> Result<Option<Vec<char, V>>, Error> {
        self.argument_list.insert(name, vars).map_err(|_| Error::NotEnoughMemory)
    }

    pub fn get_function(&self, name: String<8>) -> Option<&Expression<E>> {
        self.function_map.get(&name)
    }

    pub fn get_arguments(&self, name: String<8>) -> Option<&Vec<char, V>> {
        self.argument_list.get(&name)
    }

    pub fn remove_function(&mut self, name: String<8>) -> Option<Expression<E>> {
        self.function_map.remove(&name)
    }

    pub fn remove_arguments(&mut self, name: String<8>) -> Option<Vec<char, V>> {
        self.argument_list.remove(&name)
    }

    pub fn clear(&mut self) {
        self.function_map.clear();
        self.argument_list.clear();
    }

    pub fn len(&self) -> usize {
        assert!(self.function_map.len() == self.argument_list.len());
        self.function_map.len()
    }
}
