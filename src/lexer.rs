use std::marker::PhantomData;
use std::str::Chars;
use std::iter::Peekable;

enum TypeExpr<K> {
    Generic(Vec<String>),
    Normal(String, Vec<TypeExpr<K>>),
    Func(Vec<TypeExpr<K>>, Box<TypeExpr<K>>),
    Useless(PhantomData<K>),
}

type Any = ();
type Generic = ();
type Normal = ();
type Func = ();

impl TypeExpr<Any> {
    fn try_unpack_generic(self) -> Option<Vec<String>> {
        match self {
            TypeExpr::Generic(traits) => Some(traits),
            _ => None,
        }
    }

    fn try_unpack_normal(self) -> Option<(String, Vec<TypeExpr<Any>>)> {
        match self {
            TypeExpr::Normal(name, args) => Some((name, args)),
            _ => None,
        }
    }

    fn try_unpack_func(self) -> Option<(Vec<TypeExpr<Any>>, Box<TypeExpr<Any>>)> {
        match self {
            TypeExpr::Func(args, ret) => Some((args, ret)),
            _ => None,
        }
    }
}

impl TypeExpr<Generic> {
    fn unpack_generic(self) -> Vec<String> {
        self.try_unpack_generic().unwrap()
    }
}

impl TypeExpr<Normal> {
    fn unpack_normal(self) -> (String, Vec<TypeExpr<Any>>) {
        self.try_unpack_normal().unwrap()
    }
}

impl TypeExpr<Func> {
    fn unpack_func(self) -> (Vec<TypeExpr<Any>>, Box<TypeExpr<Any>>) {
        self.try_unpack_func().unwrap()
    }
}

struct Var<K> {
    name: String,
    type_expr: TypeExpr<K>,
}

enum SumOrProd {
    Sum(Vec<(String, Option<TypeExpr<Any>>)>),
    Prod(Vec<(String, TypeExpr<Any>)>),
}

enum AST {
    Typed(String, Box<AST>),
    DefType {
        params: Vec<String>,
        trait_specs: Vec<(usize, Vec<String>)>,
        fields: SumOrProd,
    },
    DefTrait {
        name: String,
        params: Vec<String>,
        trait_specs: Vec<(usize, Vec<String>)>,
        items: Vec<TypeExpr<Any>>,
    },
    Fn {
        params: Vec<Var<Any>>,
        body: Box<AST>,
    },
    Let {
        ident: Var<Any>,
        body: Box<AST>,
    },
    Scope(Vec<AST>),
    Unit,
    Int(i64),
}

enum LexErr {
    Expected(String, String),
}

fn is_identifier(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn get_ident(stream: &mut Peekable<Chars>) -> String {
    stream.take_while(|&c| is_identifier(c)).collect::<String>()
}

fn get_int(stream: &mut Peekable<Chars>) -> Result<i64, LexErr> {
    let s = stream.take_while(|c| c.is_digit(10)).collect::<String>();
    match s.parse::<i64>() {
        Ok(i) => Ok(i),
        Err(_) => Err(LexErr::Expected("integer".to_string(), s)),
    }
}

fn skip_whitespace(stream: &mut Peekable<Chars>) {
    stream.take_while(|c| c.is_whitespace()).count();
}

fn get_string(s: &str, stream: &mut Peekable<Chars>) -> Result<(), LexErr> {
    for e in s.chars() {
        match stream.next() {
            Some(c) if c == e => (),
            Some(_) => return Err(LexErr::Expected(e.to_string(), s.to_string())),
            None => return Err(LexErr::Expected(e.to_string(), "EOF".to_string())),
        }
    }
    Ok(())
}

fn get_string_ws(s: &str, stream: &mut Peekable<Chars>) -> Result<(), LexErr> {
    skip_whitespace(stream);
    get_string(s, stream)
}

fn get_normal_type_expr(stream: &mut Peekable<Chars>, del: char) -> Result<TypeExpr<Any>, LexErr> {
    let mut res: Vec<TypeExpr<Any>>;
    let mut last: Option<String> = None;

    loop {
        if let Some(s) = last.take() {
            res.push(TypeExpr::Normal(s, vec![]));
        }

        skip_whitespace(stream);
        match stream.peek() {
            Some(&c) if c == del => {
                stream.next();
                break;
            }
            Some('(') => {
                stream.next();
                let inner = get_normal_type_expr(stream, ')')?;
                res.push(inner);
            }
            Some(c) if is_identifier(*c) => {
                let name = get_ident(stream);
                last = Some(name);
            }
            _ => return Err(LexErr::Expected("type_expr or identifier".to_string(), "EOF".to_string())),
        }
    }

    if let Some(last) = last {
        return Ok(TypeExpr::Normal(last, res));
    } else {
        return Err(LexErr::Expected("type name".to_string(), "EOF".to_string()));
    }
}

fn lex_in_type(stream: &mut Peekable<Chars>) -> Result<AST, LexErr> {
    let type_expr = get_normal_type_expr(stream, '=')?;
    skip_whitespace(stream);
    match stream.next() {
        Some('(') => 

        None => Err(LexErr::Expected("type definition".to_string(), "EOF".to_string())),
    }
}

fn lex(stream: &mut Peekable<Chars>) -> Result<AST, LexErr> {
    while let Some(c) = stream.peek() {
        match c {
            '0'..='9' => return Ok(AST::Int(get_int(stream)? as i64)),
            'a'..='z' | 'A'..='Z' | '_' => {
                let s = get_ident(stream);
                match &s {
                    "type" => {

                    }
                }           
            }
        }
    }

    Err(LexErr::Expected("something".to_string(), "EOF".to_string()))
}
