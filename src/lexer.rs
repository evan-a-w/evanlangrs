use std::collections::HashMap;
use std::iter::Peekable;
use std::marker::PhantomData;
use std::str::Chars;

type LexResult<T> = Result<T, LexErr>;
type TraitSpec = (String, Vec<String>);

enum AST {
    Typed(String, Box<AST>),
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
    Sum(HashMap<String, Option<TypeExpr<Any>>>),
    Prod(HashMap<String, TypeExpr<Any>>),
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
    let s = stream.take_while(|c| c.is_digit(10)).collect::<String>();
    match s.parse::<i64>() {
        Ok(i) => Ok(i),
        Err(_) => Err(LexErr::Expected("integer".to_string(), s)),
    }
}

fn skip_whitespace(stream: &mut Peekable<Chars>) {
    stream.take_while(|c| c.is_whitespace()).count();
}

fn get_string(s: &str, stream: &mut Peekable<Chars>) -> LexResult<bool> {
    for e in s.chars() {
        match stream.next() {
            Some(c) if c == e => (),
            Some(_) => return Err(LexErr::Expected(e.to_string(), s.to_string())),
            None => return Err(LexErr::Expected(e.to_string(), "EOF".to_string())),
        }
    }
    Ok(true)
}

fn get_definition_type_expr(
    stream: &mut Peekable<Chars>
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
    let found_del = *stream.peek().expect("Should have gone to del");
    if found_del == '-' || func_del {
        if !func_del {
            stream.next();
        }
        if stream.peek() != Some(&'>') {
            return Err(LexErr::Expected(format!("->"), found_del.to_string()));
        }
    }

    Ok(dels.chars().any(|x| x == found_del) && !func_del)
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
    match stream.peek() {
        Some(&c) if or.chars().any(|ch| ch == c) => Ok(false),
        Some(&c) if expr.chars().nth(0).unwrap() == c => Ok(get_string(expr, stream)?),
        _ => Err(LexErr::Expected(expr.to_string(), "EOF".to_string())),
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
    let trait_specs = if w {
        get_trait_specs(stream)?
    } else {
        vec![]
    };
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
    let trait_specs = if w {
        get_trait_specs(stream)?
    } else {
        vec![]
    };
    skip_whitespace(stream);
    let paren: bool  = match stream.next() {
        Some('(') => Ok(true),
        Some('{') => Ok(false),
        _ => {
            Err(LexErr::Expected(
                "sum or product".to_string(),
                "EOF".to_string(),
            ))
        }
    }?;

    let items = get_multiple(stream, if paren { sum_sep } else { prod_sep }, get_sum_element)?
                    .into_iter()
                    .collect();

    Ok(AST::DefTrait {
        name: def_type_expr,
        trait_specs,
        items,
    })
}

fn lex(stream: &mut Peekable<Chars>) -> LexResult<AST> {
    while let Some(c) = stream.peek() {
        match c {
            '0'..='9' => return Ok(AST::Int(get_int(stream)? as i64)),
            'a'..='z' | 'A'..='Z' | '_' => {
                let s = get_ident(stream);
                match s.as_str() {
                    "type" => return lex_in_type(stream),
                    "trait" => return lex_in_trait(stream),
                    _ => return Err(LexErr::Unimplemented),
                }
            }
            _ => return Err(LexErr::Unimplemented),
        }
    }

    Err(LexErr::Expected("something".to_string(), "EOF".to_string()))
}
