use crate::ast::TypeExpr::*;
use std::collections::HashMap;
use crate::ast::AST::*;
use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::types::*;

fn assert_str(s: &'static str, ast: AST) {
    let stream = s.chars().peekable();
    let mut token = Tokenizer::from(stream);

    assert_eq!(parse(&mut token), Ok(ast));
}

fn make_type(s: String) -> TypeExpr {
    Normal(s, vec![])
}

fn get_abc() -> TypeExpr {
    Normal(
        "c".into(),
        vec![TypeExpr::Ident("a".into()), TypeExpr::Ident("b".into())],
    )
}

fn get_ep_func() -> TypeExpr {
    let res = Func(
        vec![TypeExpr::Ident("a".into()), TypeExpr::Ident("b".into())],
        Box::new(TypeExpr::Ident("c".into())),
    );
    let first = Normal(
        "c".into(),
        vec![TypeExpr::Ident("a".into()), TypeExpr::Ident("b".into())],
    );
    let second = TypeExpr::Ident("a".into());

    Func(vec![first, second], Box::new(res))
}

#[test]
fn test_var_no_type() {
    assert_str(
        "let name = 1",
        Let {
            ident: Var {
                name: "name".into(),
                type_expr: TypeExpr::Any,
            },
            body: Box::new(Int(1)),
        },
    );

    assert_str(
        "let a = 1",
        Let {
            ident: Var {
                name: "a".into(),
                type_expr: TypeExpr::Any,
            },
            body: Box::new(Int(1)),
        },
    );
}

#[test]
fn test_var() {
    assert_str(
        "let name : a b c = 1",
        Let {
            ident: Var {
                name: "name".into(),
                type_expr: get_abc(),
            },
            body: Box::new(Int(1)),
        },
    );
}

#[test]
fn test_func_type() {
    let first = Normal(
        "c".into(),
        vec![TypeExpr::Ident("a".into()), TypeExpr::Ident("b".into())],
    );
    let second = TypeExpr::Ident("a".into());

    assert_str(
        "let name : a b c -> a = 1",
        Let {
            ident: Var {
                name: "name".into(),
                type_expr: Func(vec![first], Box::new(second)),
            },
            body: Box::new(Int(1)),
        },
    );
}

#[test]
fn test_func_type_bra() {

    assert_str(
        "let name : a b c -> a -> (a -> b -> c) = 1",
        Let {
            ident: Var {
                name: "name".into(),
                type_expr: get_ep_func(),
            },
            body: Box::new(Int(1)),
        },
    );
}

#[test]
fn test_type_expr() {
    assert_str(
        "type a b c ( Some, X of a b c )",
        DefType {
            name: ("c".into(), vec!["a".into(), "b".into()]),
            trait_specs: vec![],
            fields: SumOrProd::Sum({
                let mut map = HashMap::new();
                map.insert("Some".into(), None);
                map.insert("X".into(), Some(get_abc()));
                map
            }),
        },
    );
}
