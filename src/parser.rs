use crate::ast::*;
use crate::lexer::*;

pub type ParseResult<T> = Result<T, ParseErr>;

#[derive(Debug, PartialEq)]
pub enum ParseErr {
    Expected(String, String),
    DuplicateTrait(String),
    TypeNotFound(String),
    Unimplemented,
}

pub fn parse(stream: &mut Tokenizer) -> ParseResult<AST> {
    match stream.next() {
        Some(Token::Int(i)) => {
            Ok(AST::Int(i))
        }
        Some(Token::Ident(s)) if s == "type" => parse_in_type(stream),
        Some(Token::Ident(s)) if s == "trait" => parse_in_trait(stream),
        Some(Token::Ident(s)) if s == "fn" => parse_in_fn(stream),
        Some(Token::Ident(s)) if s == "let" => parse_in_let(stream),
        Some(Token::Ident(s)) if s == "impl" => parse_in_impl(stream),
        Some(Token::Ident(name)) => {
            if stream.peek() == &Some(Token::ParenOpen) {
                stream.next();
                parse_call(stream, name)
            } else {
                Ok(AST::Ident(name))
            }
        }
        Some(Token::BraceOpen) => parse_in_scope(stream),
        Some(Token::ParenOpen) => {
            let expr = parse(stream)?;
            if stream.peek() != &Some(Token::ParenClose) {
                return Err(ParseErr::Expected(
                    ")".to_string(),
                    format!("{:?}", stream.peek()),
                ));
            }
            stream.next();
            Ok(expr)
        }
        other => Err(ParseErr::Expected(
            "expression in parse".to_string(),
            format!("{:?}", other),
        )),
    }
}

fn sep_by_comma(stream: &mut Tokenizer, end_tok: &Token) -> ParseResult<bool> {
    match stream.peek() {
        Some(t) if t == end_tok => Ok(false),
        Some(Token::Comma) => {
            stream.next();
            match stream.peek() {
                Some(t) if t == end_tok => Ok(false),
                _ => Ok(true),
            }
        }
        _ => Err(ParseErr::Expected(
            "separator".to_string(),
            "EOF".to_string(),
        )),
    }
}

fn try_get_ident(stream: &mut Tokenizer) -> ParseResult<String> {
    match stream.next() {
        Some(Token::Ident(s)) => Ok(s),
        other => Err(ParseErr::Expected(
            "identifier".to_string(),
            format!("{:?}", other),
        )),
    }
}

fn get_sum_or_prod_element(
    stream: &mut Tokenizer,
    sum: bool,
) -> ParseResult<(String, Option<TypeExpr>)> {
    let name = try_get_ident(stream)?;

    match stream.peek() {
        Some(Token::Ident(s)) if s == "of" => {
            stream.next();
            let del = if sum {
                Token::ParenClose
            } else {
                Token::BraceClose
            };
            let texpr = get_type_expr(stream, &[Token::Comma, del])?;
            Ok((name, Some(texpr)))
        }
        Some(Token::Comma) | Some(Token::ParenClose) => {
            if !sum {
                Err(ParseErr::Expected(
                    "type_expr".to_string(),
                    "EOF".to_string(),
                ))
            } else {
                Ok((name, None))
            }
        }
        other => Err(ParseErr::Expected(
            "type_expr or , or )".to_string(),
            format!("{:?}", other),
        )),
    }
}

fn get_sum_element(stream: &mut Tokenizer) -> ParseResult<(String, Option<TypeExpr>)> {
    get_sum_or_prod_element(stream, true)
}

fn get_prod_element(stream: &mut Tokenizer) -> ParseResult<(String, Option<TypeExpr>)> {
    get_sum_or_prod_element(stream, false)
}

fn sum_sep(stream: &mut Tokenizer) -> ParseResult<bool> {
    sep_by_comma(stream, &Token::ParenClose)
}

fn prod_sep(stream: &mut Tokenizer) -> ParseResult<bool> {
    sep_by_comma(stream, &Token::BraceClose)
}

fn parse_in_type(stream: &mut Tokenizer) -> ParseResult<AST> {
    let def_type_expr = get_definition_type_expr(stream)?;

    let trait_specs = if stream.peek() == &Some(Token::Ident("where".to_string())) {
        stream.next();
        stream.debug_print();
        get_trait_specs(stream)?
    } else {
        vec![]
    };

    let fields = match stream.next() {
        Some(Token::ParenOpen) => SumOrProd::Sum(
            get_multiple(stream, sum_sep, get_sum_element)?
                .into_iter()
                .collect(),
        ),
        Some(Token::BraceOpen) => SumOrProd::Prod(
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
            return Err(ParseErr::Expected(
                "sum or product".to_string(),
                "EOF".to_string(),
            ))
        }
    };

    stream.next();

    Ok(AST::DefType {
        name: def_type_expr,
        trait_specs,
        fields,
    })
}

fn get_token(tok: Token, stream: &mut Tokenizer) -> ParseResult<()> {
    match stream.next() {
        Some(t) if t == tok => Ok(()),
        other => Err(ParseErr::Expected(
            format!("{:?}", tok),
            format!("{:?}", other),
        )),
    }
}

fn parse_in_impl(stream: &mut Tokenizer) -> ParseResult<AST> {
    let trait_type = get_definition_type_expr(stream)?;
    get_string("for", stream)?;
    let for_name = get_definition_type_expr(stream)?;

    let trait_specs = if stream.peek() == &Some(Token::Ident("where".to_string())) {
        stream.next();
        get_trait_specs(stream)?
    } else {
        vec![]
    };

    match stream.peek() {
        Some(Token::BraceOpen) => {
            stream.next();
        }
        other => Err(ParseErr::Expected(
            "{".to_string(),
            format!("{:?}", other),
        )),
    }

    let fields = match stream.next() {
        Some(Token::ParenOpen) => SumOrProd::Sum(
            get_multiple(stream, sum_sep, get_sum_element)?
                .into_iter()
                .collect(),
        ),
        Some(Token::BraceOpen) => SumOrProd::Prod(
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
            return Err(ParseErr::Expected(
                "sum or product".to_string(),
                "EOF".to_string(),
            ))
        }
    };

    stream.next();

    Ok(AST::DefType {
        name: def_type_expr,
        trait_specs,
        fields,
    })
}

fn get_definition_type_expr(stream: &mut Tokenizer) -> ParseResult<(String, Vec<String>)> {
    let mut res: Vec<String> = vec![];
    let mut last: Option<String> = None;

    loop {
        match stream.peek() {
            Some(Token::ParenOpen) | Some(Token::BraceOpen) => break,
            Some(Token::Ident(s)) if s == "where" => break,
            Some(Token::Ident(name)) => {
                if let Some(s) = last.take() {
                    res.push(s);
                }
                last = Some(stream.take_peek_string());
            }
            _ => {
                return Err(ParseErr::Expected(
                    "identifier".to_string(),
                    "EOF".to_string(),
                ))
            }
        }
    }

    if let Some(last) = last {
        return Ok((last, res));
    } else {
        return Err(ParseErr::Expected(
            "type definer".to_string(),
            "EOF".to_string(),
        ));
    }
}

fn get_trait_specs(stream: &mut Tokenizer) -> ParseResult<Vec<TraitSpec>> {
    get_multiple(stream, trait_spec_sep, get_single_trait_spec)
}

fn get_string(s: &str, stream: &mut Tokenizer) -> ParseResult<String> {
    match stream.peek() {
        Some(Token::Ident(sp)) if sp == s => Ok(stream.take_peek_string()),
        _ => Err(ParseErr::Expected(s.to_string(), "EOF".to_string())),
    }
}

fn get_single_trait_spec(s: &mut Tokenizer) -> ParseResult<TraitSpec> {
    let name = s
        .next()
        .and_then(|t| match t {
            Token::Ident(s) => Some(s),
            _ => None,
        })
        .ok_or(ParseErr::Expected(
            "identifier".to_string(),
            "EOF".to_string(),
        ))?;

    get_string("is", s)?;

    let tl = get_trait_list(s)?;

    Ok((name, tl))
}

fn get_trait_list(s: &mut Tokenizer) -> ParseResult<Vec<String>> {
    match s.peek() {
        Some(Token::BracketOpen) => {
            s.next();
            get_multiple(s, next_trait, one_trait)
        }
        _ => Err(ParseErr::Expected(
            "trait list".to_string(),
            "EOF".to_string(),
        )),
    }
}

fn next_trait(s: &mut Tokenizer) -> ParseResult<bool> {
    match s.next() {
        Some(Token::Comma) => Ok(true),
        Some(Token::BracketClose) => Ok(false),
        other => Err(ParseErr::Expected(
            "trait name".to_string(),
            format!("{:?}", other),
        )),
    }
}

fn one_trait(stream: &mut Tokenizer) -> ParseResult<String> {
    match stream.next() {
        Some(Token::Ident(s)) => Ok(s),
        other => Err(ParseErr::Expected(
            "trait".to_string(),
            format!("{:?}", other),
        )),
    }
}

fn trait_spec_sep(stream: &mut Tokenizer) -> ParseResult<bool> {
    match stream.peek() {
        Some(Token::Comma) => {
            stream.next();
            Ok(true)
        }
        Some(Token::BraceOpen) | Some(Token::ParenOpen) => Ok(false),
        _ => Err(ParseErr::Expected(
            "trait spec separator".to_string(),
            "EOF".to_string(),
        )),
    }
}

fn get_multiple<T>(
    stream: &mut Tokenizer,
    continue_sep: impl Fn(&mut Tokenizer) -> ParseResult<bool>,
    one: impl Fn(&mut Tokenizer) -> ParseResult<T>,
) -> ParseResult<Vec<T>> {
    let mut res = vec![one(stream)?];
    loop {
        if !continue_sep(stream)? {
            break;
        }
        res.push(one(stream)?);
    }
    Ok(res)
}

fn get_multiple0<T>(
    stream: &mut Tokenizer,
    continue_sep: impl Fn(&mut Tokenizer) -> ParseResult<bool>,
    one: impl Fn(&mut Tokenizer) -> ParseResult<T>,
) -> ParseResult<Vec<T>> {
    let mut res = vec![];
    loop {
        if !continue_sep(stream)? {
            break;
        }
        res.push(one(stream)?);
    }
    Ok(res)
}

fn get_single_type_expr(stream: &mut Tokenizer, dels: &[Token]) -> ParseResult<TypeExpr> {
    let mut v = vec![];

    loop {
        match stream.peek() {
            Some(t) if dels.contains(&t) => break,
            Some(Token::Arrow) => break,
            Some(Token::Ident(s)) => {
                let s = if let Some(Token::Ident(s)) = stream.next() {
                    s
                } else {
                    unreachable!()
                };
                v.push(TypeExpr::Ident(s));
            }
            Some(Token::ParenOpen) => {
                stream.next();
                v.push(get_type_expr(stream, &[Token::ParenClose])?);
                stream.next();
            }
            other => {
                return Err(ParseErr::Expected(
                    "type expression".to_string(),
                    format!("{:?}", other),
                ))
            }
        }
    }

    let last = v.pop().ok_or(ParseErr::Expected(
        "type expression".to_string(),
        "EOF".to_string(),
    ))?;

    if v.len() == 0 {
        Ok(last)
    } else if let TypeExpr::Ident(s) = last {
        Ok(TypeExpr::Normal(s, v))
    } else {
        Err(ParseErr::Expected(
            "some type".to_string(),
            format!("{v:?} {last:?}"),
        ))
    }
}

fn get_multiple_prime<T>(
    stream: &mut Tokenizer,
    next: impl Fn(&mut Tokenizer) -> ParseResult<(T, bool)>,
) -> ParseResult<Vec<T>> {
    let mut res = Vec::new();
    loop {
        let (next, cont) = next(stream)?;
        res.push(next);
        if !cont {
            break;
        }
    }
    Ok(res)
}

fn get_type_expr(stream: &mut Tokenizer, dels: &[Token]) -> ParseResult<TypeExpr> {
    let mut multi = get_multiple_prime(stream, move |s| {
        let res = get_single_type_expr(s, dels)?;
        let cont = if s.peek() == &Some(Token::Arrow) {
            s.next();
            true
        } else {
            false
        };
        Ok((res, cont))
    })?;

    let result = multi.pop().ok_or(ParseErr::Expected(
        "non empty list of type exprs".to_string(),
        format!("{:?}", multi),
    ))?;

    if multi.len() == 0 {
        Ok(result)
    } else {
        Ok(TypeExpr::Func(multi, Box::new(result)))
    }
}

fn parse_in_trait(stream: &mut Tokenizer) -> ParseResult<AST> {
    let def_type_expr = get_definition_type_expr(stream)?;

    let items = get_multiple(
        stream,
        prod_sep,
        get_prod_element,
    )?
    .into_iter()
    .map(|(a, b)| (a, b.expect("Prod element should never be None")))
    .collect();

    Ok(AST::DefTrait {
        name: def_type_expr,
        items,
    })
}

fn is_expr_or(stream: &mut Tokenizer, expr: Token, or: Token) -> ParseResult<bool> {
    match stream.next() {
        Some(t) if t == expr => Ok(true),
        Some(t) if t == or => Ok(false),
        other => Err(ParseErr::Expected(
            format!("{expr:?} or {or:?}"),
            format!("{:?}", other),
        )),
    }
}

fn parse_var(stream: &mut Tokenizer, dels: &[Token]) -> ParseResult<Var> {
    let name = try_get_ident(stream)?;

    let type_expr = match stream.peek() {
        Some(Token::Colon) => {
            stream.next();
            let res = get_type_expr(stream, dels)?;
            res
        }
        other if dels.iter().any(|x| Some(x) == other.as_ref()) => TypeExpr::Any,
        other => {
            return Err(ParseErr::Expected(
                "type expression or nothing".to_string(),
                format!("{:?}", other),
            ))
        }
    };

    Ok(Var { name, type_expr })
}

fn parse_in_fn(stream: &mut Tokenizer) -> ParseResult<AST> {
    match stream.peek() {
        Some(Token::ParenOpen) => {
            stream.next();
            let params = get_multiple(
                stream,
                move |s| sep_by_comma(s, &Token::ParenClose),
                move |s| parse_var(s, &[Token::Comma, Token::ParenClose]),
            )?;
            stream.next();
            let next = stream.next();
            if next != Some(Token::Arrow) {
                return Err(ParseErr::Expected("->".to_string(), format!("{:?}", next)));
            }
            let body = Box::new(parse(stream)?);
            Ok(AST::Fn { params, body })
        }
        _ => Err(ParseErr::Expected(
            "arguments".into(),
            "something else".into(),
        )),
    }
}

fn parse_in_let(stream: &mut Tokenizer) -> ParseResult<AST> {
    let var = parse_var(stream, &[Token::Equals])?;

    stream.next();

    Ok(AST::Let {
        ident: var,
        body: Box::new(parse(stream)?),
    })
}

fn parse_call(stream: &mut Tokenizer, name: String) -> ParseResult<AST> {
    let args = get_multiple(
        stream,
        move |s| sep_by_comma(s, &Token::ParenClose),
        move |s| parse(s),
    )?;
    Ok(AST::Call { name, args })
}

fn scope_sep(stream: &mut Tokenizer) -> ParseResult<bool> {
    match stream.next() {
        Some(Token::Semicolon) => {
            if stream.peek() == &Some(Token::BraceClose) {
                stream.next();
                Ok(false)
            } else {
                Ok(true)
            }
        }
        Some(Token::BraceClose) => Ok(false),
        other => Err(ParseErr::Expected(
            "scope separator".to_string(),
            format!("{:?}", other),
        )),
    }
}

fn parse_in_scope(stream: &mut Tokenizer) -> ParseResult<AST> {
    let mut res = match stream.peek() {
        Some(Token::BraceClose) => {
            stream.next();
            return Ok(AST::Scope(vec![], Box::new(AST::Unit)));
        }
        other => vec![parse(stream)?],
    };

    loop {
        match stream.peek() {
            Some(Token::BraceClose) => {
                stream.next();
                let val = res.pop().map(Box::new).unwrap_or_else(|| Box::new(AST::Unit));
                return Ok(AST::Scope(res, val));
            }
            Some(Token::Semicolon) => {
                stream.next();
                if stream.peek() == &Some(Token::BraceClose) {
                    stream.next();
                    return Ok(AST::Scope(res, Box::new(AST::Unit)));
                }

                res.push(parse(stream)?);
            }
            other => {
                return Err(ParseErr::Expected(
                    "scope separator".to_string(),
                    format!("{:?}", other),
                ))
            }
        }
    }
}
