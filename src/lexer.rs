use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Colon,
    Comma,
    Dot,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,
    Semicolon,
    Arrow,
    Minus,
    Plus,
    Star,
    Slash,
    Equals,
    Ident(String),
    Int(i64),
}

pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
    last: Option<Token>,
}

fn take_while(stream: &mut Peekable<Chars>, f: impl Fn(char) -> bool) -> String {
    let mut res = String::new();

    while let Some(&c) = stream.peek() {
        if f(c) {
            res.push(stream.next().unwrap());
        } else {
            break;
        }
    }

    res
}

fn is_identifier_start(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false,
    }
}

fn is_identifier(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn get_ident(stream: &mut Peekable<Chars>) -> String {
    take_while(stream, |c| is_identifier(c))
}
fn skip_whitespace(stream: &mut Peekable<Chars>) {
    take_while(stream, |c| c.is_whitespace());
}

impl Tokenizer<'_> {
    pub fn from(input: Peekable<Chars<'static>>) -> Tokenizer {
        Tokenizer { input, last: None }
    }

    pub fn take_peek_string(&mut self) -> String {
        self.peek();
        let t = self.last.take();
        match t {
            Some(Token::Ident(s)) => s,
            _ => panic!("Expected ident"),
        }
    }

    pub fn peek(&mut self) -> &Option<Token> {
        if self.last.is_some() {
            return &self.last;
        }

        let token = self.next();
        self.last = token;
        &self.last
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.last.is_some() {
            return self.last.take();
        }

        match self.input.next() {
            Some(c) => {
                if c.is_whitespace() {
                    skip_whitespace(&mut self.input);
                    self.next()
                } else if is_identifier_start(c) {
                    let ident = c.to_string() + &get_ident(&mut self.input);
                    Some(Token::Ident(ident))
                } else if c.is_numeric() {
                    let ident = c.to_string() + &get_ident(&mut self.input);
                    Some(Token::Int(ident.parse().ok()?))
                } else {
                    match c {
                        '=' => Some(Token::Equals),
                        ':' => Some(Token::Colon),
                        ',' => Some(Token::Comma),
                        '.' => Some(Token::Dot),
                        '(' => Some(Token::ParenOpen),
                        ')' => Some(Token::ParenClose),
                        '{' => Some(Token::BraceOpen),
                        '}' => Some(Token::BraceClose),
                        '[' => Some(Token::BracketOpen),
                        ']' => Some(Token::BracketClose),
                        ';' => Some(Token::Semicolon),
                        '-' => {
                            if let Some('>') = self.input.peek() {
                                self.input.next();
                                Some(Token::Arrow)
                            } else {
                                Some(Token::Minus)
                            }
                        }
                        '+' => Some(Token::Plus),
                        '*' => Some(Token::Star),
                        '/' => Some(Token::Slash),
                        _ => None,
                    }
                }
            }
            None => None,
        }
    }
}
