use std::collections::HashMap;
use std::iter::Peekable;
use std::marker::PhantomData;
use std::str::Chars;
use crate::lexer::*;

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
        stream.next();
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

fn extend_dels(dels: &str, c: char) -> String {
    if dels.chars().any(|x| x == c) {
        dels.to_string()
    } else {
        format!("{dels}{c}")
    }
}

fn get_single_type_expr(stream: &mut Peekable<Chars>, dels: &str) -> LexResult<(TypeExpr<Any>, bool)> {
    match stream.peek() {
        Some('(') => {
            stream.next();
            Ok((get_type_expr(stream, ")", false)?, false))
        }
        _ => Ok((get_type_expr(stream, dels, true)?, true)),
    }
}

fn continue_pred(stream: &mut Peekable<Chars>, dels: &str, func_del: bool) -> LexResult<bool> {
    match stream.peek().cloned() {
        Some(c) if dels.chars().any(|x| x == c) => {
            stream.next();
            if Some('>') == stream.peek().cloned() && c == '-' {
                stream.next();
                Ok(!func_del)
            } else {
                Ok(false)
            }
        }
        Some(c) if c == '(' || is_identifier_start(c) => Ok(true),
        other => {
            debug_print_stream(stream);
            Err(LexErr::Expected(dels.into(), format!("{other:?} in type conmt")))
        }
    }
}

fn get_type_expr(
    stream: &mut Peekable<Chars>,
    dels: &str,
    func_del: bool,
) -> LexResult<TypeExpr<Any>> {
    skip_whitespace(stream);

    let extended = extend_dels(dels, '-');
    let first = get_normal_type_expr(stream, &extended)?;


    if !continue_pred(stream, &extended, func_del)? {
        return Ok(first);
    }

    // Func thing

    let mut args = vec![first];
    loop {
        skip_whitespace(stream);

        
        let (next, eaten) = get_single_type_expr(stream, &extended)?;
        let cont = if eaten {
            match stream.peek() {
                Some('>') => {
                    stream.next();
                    !func_del
                }
                _ => false,
            }
        } else {
            continue_pred(stream, &extended, func_del)?
        };

        args.push(next);

        skip_whitespace(stream);
        if !cont {
            let last = args.pop().expect("Shouldnt be empty func arg");
            break Ok(TypeExpr::Func(args, Box::new(last)));
        }
    }
}

fn get_normal_type_expr(stream: &mut Peekable<Chars>, dels: &str) -> LexResult<TypeExpr<Normal>> {
    let mut last = None;
    let mut res = vec![];

    loop {
        skip_whitespace(stream);

        if let Some(c) = stream.peek() {
            if dels.chars().any(|x| x == *c) {
                return Ok(TypeExpr::Normal(
                    last.ok_or(LexErr::Expected("normal type expr".into(), "else".into()))?,
                    res,
                ));
            }
        }

        if let Some(s) = last.take() {
            res.push(TypeExpr::Normal(s, vec![]));
        }

        match stream.peek() {
            Some('a'..='z') | Some('A'..='Z') | Some('_') => {
                last = Some(get_ident(stream));
            }
            Some('(') => {
                res.push(get_type_expr(stream, ")", false)?);
            }
            other => {
                return Err(LexErr::Expected(
                    "type definer".to_string(),
                    format!("{other:?}"),
                ))
            }
        }
    }
}

pub fn is_expr_or(stream: &mut Peekable<Chars>, expr: &str, or: &str) -> LexResult<bool> {
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
        Err(LexErr::Expected(
            format!("{expr} or {or}"),
            "something else".into(),
        ))
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
            let params = get_multiple(
                stream,
                move |s| sep_by_comma(s, ')'),
                move |s| lex_var(s, ","),
            )?;
            let body = Box::new(lex(stream)?);
            Ok(AST::Fn { params, body })
        }
        _ => Err(LexErr::Expected(
            "arguments".into(),
            "something else".into(),
        )),
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
            Ok(AST::Call { name, args })
        }
        _ => Err(LexErr::Expected(
            "arguments".into(),
            "something else".into(),
        )),
    }
}

pub fn lex(stream: &mut Peekable<Chars>) -> LexResult<AST> {
    if let Some(c) = stream.peek().cloned() {
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
            _ => {
                debug_print_stream(stream);
                Err(LexErr::Expected("not that".into(), format!("{c}")))
            }
        }
    } else {
        Err(LexErr::Expected("char".into(), "eof".into()))
    }
}
