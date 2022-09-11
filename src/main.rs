#![allow(dead_code)]
#![allow(unused_variables)]

mod ast;
mod lexer;
mod parser;
mod tests;
mod types;

use crate::lexer::*;
use crate::parser::*;

fn main() {
    let stream = "{
        hi;
        let a = 1;
        let b = 2;
        type a b c ( Some, X of a b c )
    }".chars().peekable();

    let mut token = Tokenizer::from(stream);
    println!("{:?}", parse(&mut token));

    let stream = "
        type a b c ( Some, X of a b c )
    ".chars().peekable();

    let mut token = Tokenizer::from(stream);
    println!("{:?}", parse(&mut token));
}
