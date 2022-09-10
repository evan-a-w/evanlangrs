use std::collections::HashMap;
use std::iter::Peekable;
use std::marker::PhantomData;
use std::str::Chars;

pub type LexResult<T> = Result<T, LexErr>;
pub type TraitSpec = (String, Vec<String>);

#[derive(PartialEq, Debug)]
pub enum AST {
    DefType {
        name: (String, Vec<String>),
        trait_specs: Vec<TraitSpec>,
        fields: SumOrProd,
    },
    DefTrait {
        name: (String, Vec<String>),
        trait_specs: Vec<TraitSpec>,
        items: HashMap<String, Option<TypeExpr<Any>>>,
    },
    Fn {
        params: Vec<Var<Any>>,
        body: Box<AST>,
    },
    Let {
        ident: Var<Any>,
        body: Box<AST>,
    },
    Call {
        name: String,
        args: Vec<AST>,
    },
    Scope(Vec<AST>), // currently unused
    Unit,
    Int(i64),
}

#[derive(Debug, PartialEq)]
pub enum TypeExpr<K> {
    Any,
    Normal(String, Vec<TypeExpr<K>>),
    Func(Vec<TypeExpr<K>>, Box<TypeExpr<K>>),
    Useless(PhantomData<K>),
}

pub type Any = ();
pub type Normal = ();
pub type Func = ();

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

#[derive(Debug, PartialEq)]
pub struct Var<K> {
    pub name: String,
    pub type_expr: TypeExpr<K>,
}

#[derive(Debug, PartialEq)]
pub enum SumOrProd {
    Sum(HashMap<String, Option<TypeExpr<Any>>>),
    Prod(HashMap<String, TypeExpr<Any>>),
}

#[derive(Debug, PartialEq)]
pub enum LexErr {
    Expected(String, String),
    Unimplemented,
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

fn is_identifier(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn get_ident(stream: &mut Peekable<Chars>) -> String {
    take_while(stream, |c| is_identifier(c))
}

fn try_get_ident(stream: &mut Peekable<Chars>) -> LexResult<String> {
    match stream.peek() {
        Some(&c) if is_identifier(c) => Ok(get_ident(stream)),
        Some(&c) => Err(LexErr::Expected("identifier".to_string(), c.to_string())),
        None => Err(LexErr::Expected(
            "identifier".to_string(),
            "EOF".to_string(),
        )),
    }
}

fn get_int(stream: &mut Peekable<Chars>) -> LexResult<i64> {
    let s = take_while(stream, |c| c.is_digit(10));
    match s.parse::<i64>() {
        Ok(i) => Ok(i),
        Err(_) => Err(LexErr::Expected("integer".to_string(), s)),
    }
}

fn skip_whitespace(stream: &mut Peekable<Chars>) {
    take_while(stream, |c| c.is_whitespace());
}

fn get_string(s: &str, stream: &mut Peekable<Chars>) -> LexResult<bool> {
    for e in s.chars() {
        match stream.peek() {
            Some(c) if *c == e => {
                stream.next();
            }
            _ => return Err(LexErr::Expected(e.to_string(), "EOF".to_string())),
        }
    }
    Ok(true)
}

fn get_definition_type_expr(
    stream: &mut Peekable<Chars>,
) -> LexResult<((String, Vec<String>), bool)> {
    let mut res: Vec<String> = vec![];
    let mut last: Option<String> = None;

    let mut r#where = false;

    loop {
        skip_whitespace(stream);
        match stream.peek() {
            Some(&c) if c == '(' || c == '{' => {
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
                if name == "where" {
                    r#where = true;
                    break;
                }
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
        return Ok(((last, res), r#where));
    } else {
        return Err(LexErr::Expected(
            "type definer".to_string(),
            "EOF".to_string(),
        ));
    }
}

fn check_func_next(stream: &mut Peekable<Chars>, dels: &str, func_del: bool) -> LexResult<bool> {
    skip_whitespace(stream);
    let found_del = *stream.peek().expect("Should have gone to del");
    if found_del == '-' {
        stream.next();
        if stream.peek() != Some(&'>') {
            return Err(LexErr::Expected(format!("->"), found_del.to_string()));
        }
        return Ok(!func_del);
    }

    if !dels.chars().any(|x| x == found_del) {
        Err(LexErr::Expected(dels.into(), format!("{found_del}")))
    } else {
        stream.next();
        Ok(false)
    }
}

fn get_multiple<T>(
    stream: &mut Peekable<Chars>,
    continue_sep: impl Fn(&mut Peekable<Chars>) -> LexResult<bool>,
    one: impl Fn(&mut Peekable<Chars>) -> LexResult<T>,
) -> LexResult<Vec<T>> {
    let mut res = Vec::new();
    loop {
        skip_whitespace(stream);
        if !continue_sep(stream)? {
            break;
        }
        res.push(one(stream)?);
    }
    Ok(res)
}

fn get_single_type_expr(
    stream: &mut Peekable<Chars>,
    dels: &str,
    func_del: bool,
) -> LexResult<TypeExpr<Any>> {
    match stream.peek() {
        Some('(') => {
            stream.next();
            let new_dels = if dels.chars().any(|x| x == ')') {
                String::from(dels)
            } else {
                format!("{})", dels)
            };

            let inner = get_type_expr(stream, &new_dels, false)?;
            stream.next();
            Ok(inner)
        }
        Some(c) if is_identifier(*c) => {
            let inner = get_type_expr(stream, dels, true)?;
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
    dels: &str,
    func_del: bool,
) -> LexResult<TypeExpr<Any>> {
    skip_whitespace(stream);

    let first = get_normal_type_expr(stream, &format!("{dels}-"))?;
    println!("{first:?}");

    if !check_func_next(stream, dels, func_del)? {
        return Ok(first);
    }

    stream.next();

    // Func thing

    let mut args = vec![first];
    loop {
        skip_whitespace(stream);

        let next = get_single_type_expr(stream, dels, func_del)?;

        if check_func_next(stream, dels, func_del)? {
            stream.next();
            args.push(next);
        } else {
            let last = args.pop().expect("Shouldnt be empty func arg");
            break Ok(TypeExpr::Func(args, Box::new(last)));
        }
    }
}

fn get_normal_type_expr(stream: &mut Peekable<Chars>, dels: &str) -> LexResult<TypeExpr<Normal>> {
    let mut res: Vec<TypeExpr<Any>> = vec![];
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
                let inner = get_type_expr(stream, ")", false)?;
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


fn is_expr_or(stream: &mut Peekable<Chars>, expr: &str, or: &str) -> LexResult<bool> {
    skip_whitespace(stream);

    let mut expri = expr.chars();
    let mut ori = or.chars();

    let mut left = true;
    let mut right = true;

    loop {
        let ec = expri.next();
        let oc = ori.next();
        let c = stream.peek().cloned();

        if ec.is_some() {
            left = left && ec == c;
        }
        if oc.is_some() {
            right = right && oc == c;
        }

        if (left && ec.is_some()) || (right && oc.is_some()) {
            stream.next();
        } else {
            break;
        }
    }

    if left && expri.next().is_none() {
        Ok(true)
    } else if right && ori.next().is_none() {
        Ok(false)
    } else {
        Err(LexErr::Expected(format!("{expr} or {or}"), "something else".into()))
    }
}

fn one_trait(stream: &mut Peekable<Chars>) -> LexResult<String> {
    skip_whitespace(stream);
    Ok(get_ident(stream))
}

fn next_trait(s: &mut Peekable<Chars>) -> LexResult<bool> {
    skip_whitespace(s);
    match s.next() {
        Some(',') => Ok(true),
        Some(']') => Ok(false),
        _ => Err(LexErr::Expected(
            "trait name".to_string(),
            "EOF".to_string(),
        )),
    }
}

fn get_trait_list(s: &mut Peekable<Chars>) -> LexResult<Vec<String>> {
    skip_whitespace(s);
    match s.peek() {
        Some('[') => {
            s.next();
            get_multiple(s, next_trait, one_trait)
        }
        _ => Err(LexErr::Expected(
            "trait list".to_string(),
            "EOF".to_string(),
        )),
    }
}

fn get_single_trait_spec(s: &mut Peekable<Chars>) -> LexResult<TraitSpec> {
    skip_whitespace(s);
    let name = try_get_ident(s)?;
    skip_whitespace(s);
    get_string("is", s)?;
    skip_whitespace(s);
    let tl = get_trait_list(s)?;
    Ok((name, tl))
}

fn trait_spec_sep(stream: &mut Peekable<Chars>) -> LexResult<bool> {
    skip_whitespace(stream);
    match stream.peek() {
        Some(',') => {
            stream.next();
            Ok(true)
        }
        Some('(') | Some('{') => Ok(false),
        _ => Err(LexErr::Expected(
            "trait spec separator".to_string(),
            "EOF".to_string(),
        )),
    }
}

fn get_trait_specs(stream: &mut Peekable<Chars>) -> LexResult<Vec<TraitSpec>> {
    get_multiple(stream, trait_spec_sep, get_single_trait_spec)
}

fn sep_by_comma(stream: &mut Peekable<Chars>, end_c: char) -> LexResult<bool> {
    skip_whitespace(stream);
    match stream.peek() {
        Some(&c) if c == end_c => {
            stream.next();
            Ok(false)
        }
        Some(',') => {
            stream.next();
            skip_whitespace(stream);
            match stream.peek() {
                Some(&c) if c == end_c => Ok(false),
                _ => Ok(true),
            }
        }
        _ => Err(LexErr::Expected("separator".to_string(), "EOF".to_string())),
    }
}

fn get_sum_or_prod_element(
    stream: &mut Peekable<Chars>,
    sum: bool,
) -> LexResult<(String, Option<TypeExpr<Any>>)> {
    skip_whitespace(stream);
    let name = try_get_ident(stream)?;
    skip_whitespace(stream);
    match stream.peek() {
        Some('o') => {
            get_string("of", stream)?;
            let texpr = get_type_expr(stream, ",)", false)?;
            Ok((name, Some(texpr)))
        }
        Some(',') | Some(')') => {
            if !sum {
                Err(LexErr::Expected("type_expr".to_string(), "EOF".to_string()))
            } else {
                Ok((name, None))
            }
        }
        _ => Err(LexErr::Expected(
            "type_expr or , or )".to_string(),
            "EOF".to_string(),
        )),
    }
}

fn get_sum_element(stream: &mut Peekable<Chars>) -> LexResult<(String, Option<TypeExpr<Any>>)> {
    get_sum_or_prod_element(stream, true)
}

fn get_prod_element(stream: &mut Peekable<Chars>) -> LexResult<(String, Option<TypeExpr<Any>>)> {
    get_sum_or_prod_element(stream, false)
}

fn sum_sep(stream: &mut Peekable<Chars>) -> LexResult<bool> {
    sep_by_comma(stream, ')')
}

fn prod_sep(stream: &mut Peekable<Chars>) -> LexResult<bool> {
    sep_by_comma(stream, '}')
}

fn lex_in_type(stream: &mut Peekable<Chars>) -> LexResult<AST> {
    let (def_type_expr, w) = get_definition_type_expr(stream)?;

    skip_whitespace(stream);
    let trait_specs = if w { get_trait_specs(stream)? } else { vec![] };
    skip_whitespace(stream);
    let fields = match stream.next() {
        Some('(') => SumOrProd::Sum(
            get_multiple(stream, sum_sep, get_sum_element)?
                .into_iter()
                .collect(),
        ),
        Some('{') => SumOrProd::Prod(
            get_multiple(stream, prod_sep, get_prod_element)?
                .into_iter()
                .map(|(name, texpr)| {
                    (
                        name,
                        texpr.expect("product type should always have type along with name"),
                    )
                })
                .collect(),
        ),
        _ => {
            return Err(LexErr::Expected(
                "sum or product".to_string(),
                "EOF".to_string(),
            ))
        }
    };

    Ok(AST::DefType {
        name: def_type_expr,
        trait_specs,
        fields,
    })
}

fn lex_in_trait(stream: &mut Peekable<Chars>) -> LexResult<AST> {
    let (def_type_expr, w) = get_definition_type_expr(stream)?;

    skip_whitespace(stream);
    let trait_specs = if w { get_trait_specs(stream)? } else { vec![] };
    skip_whitespace(stream);
    let paren: bool = match stream.next() {
        Some('(') => Ok(true),
        Some('{') => Ok(false),
        _ => Err(LexErr::Expected(
            "sum or product".to_string(),
            "EOF".to_string(),
        )),
    }?;

    let items = get_multiple(
        stream,
        if paren { sum_sep } else { prod_sep },
        get_sum_element,
    )?
    .into_iter()
    .collect();

    Ok(AST::DefTrait {
        name: def_type_expr,
        trait_specs,
        items,
    })
}

fn debug_print_stream(stream: &mut Peekable<Chars>) {
    println!("{}", stream.collect::<String>())
}

fn lex_var(stream: &mut Peekable<Chars>, dels: &str) -> LexResult<Var<Any>> {
    let name = try_get_ident(stream)?;
    skip_whitespace(stream);
    let type_expr = if is_expr_or(stream, ":", "=")? {
        get_type_expr(stream, dels, false)?
    } else {
        TypeExpr::Any
    };

    Ok(Var { name, type_expr })
}

fn lex_in_fn(stream: &mut Peekable<Chars>) -> LexResult<AST> {
    skip_whitespace(stream);

    match stream.peek() {
        Some('(') => {
            stream.next();
            let params = get_multiple(stream, move |s| sep_by_comma(s, ')'), move |s| lex_var(s, ","))?;
            let body = Box::new(lex(stream)?);
            Ok(AST::Fn {
                params,
                body,
            })
        }
        _ => Err(LexErr::Expected("arguments".into(), "something else".into()))
    }
}

fn lex_in_let(stream: &mut Peekable<Chars>) -> LexResult<AST> {
    skip_whitespace(stream);

    let var = lex_var(stream, "=")?;
    skip_whitespace(stream);
    Ok(AST::Let {
        ident: var,
        body: Box::new(lex(stream)?),
    })
}

fn make_call(stream: &mut Peekable<Chars>, name: String) -> LexResult<AST> {
    match stream.peek() {
        Some('(') => {
            stream.next();
            let args = get_multiple(stream, move |s| sep_by_comma(s, ')'), move |s| lex(s))?;
            Ok(AST::Call {
                name,
                args,
            })
        }
        _ => Err(LexErr::Expected("arguments".into(), "something else".into()))
    }
}

pub fn lex(stream: &mut Peekable<Chars>) -> LexResult<AST> {
    if let Some(c) = stream.peek() {
        match c {
            '0'..='9' => Ok(AST::Int(get_int(stream)? as i64)),
            'a'..='z' | 'A'..='Z' | '_' => {
                let s = get_ident(stream);
                match s.as_str() {
                    "type" => lex_in_type(stream),
                    "trait" => lex_in_trait(stream),
                    "fn" => lex_in_fn(stream),
                    "let" => lex_in_let(stream),
                    _ => make_call(stream, s),
                }
            }
            _ => Err(LexErr::Expected("not that".into(), format!("{c}")))
        }
    } else {
        Err(LexErr::Expected("char".into(), "eof".into()))

    }
}
