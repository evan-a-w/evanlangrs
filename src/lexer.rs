use std::iter::Peekable;
use std::marker::PhantomData;
use std::str::Chars;

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
        trait_specs: Vec<(String, Vec<String>)>,
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

enum TypeExpr<K> {
    Normal(String, Vec<TypeExpr<K>>),
    Func(Vec<TypeExpr<K>>, Box<TypeExpr<K>>),
    Useless(PhantomData<K>),
}

type Any = ();
type Normal = ();
type Func = ();

impl TypeExpr<Any> {
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

enum LexErr {
    Expected(String, String),
    Unimplemented,
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

fn get_string(s: &str, stream: &mut Peekable<Chars>) -> Result<bool, LexErr> {
    for e in s.chars() {
        match stream.next() {
            Some(c) if c == e => (),
            Some(_) => return Err(LexErr::Expected(e.to_string(), s.to_string())),
            None => return Err(LexErr::Expected(e.to_string(), "EOF".to_string())),
        }
    }
    Ok(true)
}

fn get_string_ws(s: &str, stream: &mut Peekable<Chars>) -> Result<(), LexErr> {
    skip_whitespace(stream);
    get_string(s, stream)
}

fn get_definition_type_expr(
    stream: &mut Peekable<Chars>,
    del: char,
) -> Result<(String, Vec<String>), LexErr> {
    let mut res: Vec<String>;
    let mut last: Option<String> = None;

    loop {
        skip_whitespace(stream);
        match stream.peek() {
            Some(&c) if c == del => {
                stream.next();
                break;
            }
            _ => (),
        }
        if let Some(s) = last.take() {
            res.push(s);
        }

        match stream.peek() {
            Some(c) if is_identifier(*c) => {
                let name = get_ident(stream);
                last = Some(name);
            }
            _ => {
                return Err(LexErr::Expected(
                    "identifier".to_string(),
                    "EOF".to_string(),
                ))
            }
        }
    }

    if let Some(last) = last {
        return Ok((last, res));
    } else {
        return Err(LexErr::Expected(
            "type definer".to_string(),
            "EOF".to_string(),
        ));
    }
}

fn check_func_next(
    stream: &mut Peekable<Chars>,
    del: char,
    func_del: bool,
) -> Result<bool, LexErr> {
    let found_del = stream.peek().expect("Should have gone to del");
    if found_del == &'-' || func_del {
        if !func_del {
            stream.next();
        }
        if stream.peek() != Some(&'>') {
            return Err(LexErr::Expected(format!("->"), found_del.to_string()));
        }
    }

    Ok(found_del != &del && !func_del)
}

fn get_single_type_expr(
    stream: &mut Peekable<Chars>,
    del: char,
    func_del: bool,
) -> Result<TypeExpr<Any>, LexErr> {
    match stream.peek() {
        Some('(') => {
            stream.next();
            let inner = get_type_expr(stream, ')', false)?;
            stream.next();
            Ok(inner)
        }
        Some(c) if is_identifier(*c) => {
            let inner = get_type_expr(stream, del, true)?;
            Ok(inner)
        }
        _ => {
            return Err(LexErr::Expected(
                "type_expr or identifier".to_string(),
                "EOF".to_string(),
            ))
        }
    }
}

fn get_type_expr(
    stream: &mut Peekable<Chars>,
    del: char,
    func_del: bool,
) -> Result<TypeExpr<Any>, LexErr> {
    skip_whitespace(stream);

    let first = get_normal_type_expr(stream, &format!("{del}-"))?;

    if !check_func_next(stream, del, func_del)? {
        return Ok(first);
    }

    stream.next();

    // Func thing

    let mut args = vec![first];
    loop {
        skip_whitespace(stream);

        let next = get_single_type_expr(stream, del, func_del)?;

        if check_func_next(stream, del, func_del)? {
            stream.next();
            args.push(next);
        } else {
            let last = args.pop().expect("Shouldnt be empty func arg");
            break Ok(TypeExpr::Func(args, Box::new(last)));
        }
    }
}

fn get_normal_type_expr(
    stream: &mut Peekable<Chars>,
    dels: &str,
) -> Result<TypeExpr<Normal>, LexErr> {
    let mut res: Vec<TypeExpr<Any>>;
    let mut last: Option<String> = None;

    loop {
        skip_whitespace(stream);
        match stream.peek() {
            Some(&c) if dels.chars().any(|ch| ch == c) => {
                break;
            }
            _ => (),
        }

        if let Some(name) = last.take() {
            res.push(TypeExpr::Normal(name, vec![]));
        }

        match stream.peek() {
            Some('(') => {
                stream.next();
                let inner = get_type_expr(stream, ')', false)?;
                stream.next();
                res.push(inner);
            }
            Some(c) if is_identifier(*c) => {
                let name = get_ident(stream);
                last = Some(name);
            }
            _ => {
                return Err(LexErr::Expected(
                    "type_expr or identifier".to_string(),
                    "EOF".to_string(),
                ))
            }
        }
    }

    if let Some(last) = last {
        return Ok(TypeExpr::Normal(last, res));
    } else {
        return Err(LexErr::Expected("type name".to_string(), "EOF".to_string()));
    }
}

fn is_expr_or(stream: &mut Peekable<Chars>, expr: &str, or: &str) -> Result<bool, LexErr> {
    skip_whitespace(stream);
    match stream.peek() {
        Some(&c) if or.chars().any(|ch| ch == c) => Ok(false),
        Some(&c) if expr.chars().nth(0).unwrap() == c => Ok(get_string(expr, stream)?),
        _ => Err(LexErr::Expected(expr.to_string(), "EOF".to_string())),
    }
}

fn lex_in_type(stream: &mut Peekable<Chars>) -> Result<AST, LexErr> {
    let def_type_expr = get_definition_type_expr(stream, '=')?;
    skip_whitespace(stream);
    let trait_specs: Vec<(String, Vec<String>)> = if is_expr_or(stream, "where", "({")? {
        // todo
        vec![]
    } else {
        vec![]
    };

    Err(LexErr::Unimplemented)
}

fn lex(stream: &mut Peekable<Chars>) -> Result<AST, LexErr> {
    while let Some(c) = stream.peek() {
        match c {
            '0'..='9' => return Ok(AST::Int(get_int(stream)? as i64)),
            'a'..='z' | 'A'..='Z' | '_' => {
                let s = get_ident(stream);
                match s.as_str() {
                    "type" => return Err(LexErr::Unimplemented),
                    _ => return Err(LexErr::Unimplemented),
                }
            }
            _ => return Err(LexErr::Unimplemented),
        }
    }

    Err(LexErr::Expected("something".to_string(), "EOF".to_string()))
}
