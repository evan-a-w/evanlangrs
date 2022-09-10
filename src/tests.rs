use crate::lexer::Any;
use crate::lexer::TypeExpr::*;
use crate::lexer::AST::*;
use crate::lexer::*;
use crate::types::*;

#[test]
fn is_expr_or_expr_test() {
    let mut s = "funny".chars().peekable();

    assert!(is_expr_or(&mut s, "funny", "=") == Ok(true));
    assert!(s.next().is_none());

    let mut s = "=".chars().peekable();
    assert!(is_expr_or(&mut s, "funny", "=") == Ok(false));
    assert!(s.next().is_none());
}

fn assert_str(s: &str, ast: AST) {
    let mut stream = s.chars().peekable();

    assert_eq!(lex(&mut stream), Ok(ast));
}

fn make_type(s: String) -> TypeExpr<Any> {
    Normal(s, vec![])
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
}

#[test]
fn test_var() {
    assert_str(
        "let name : a b c = 1",
        Let {
            ident: Var {
                name: "name".into(),
                type_expr: Normal(
                    "c".into(),
                    vec![make_type("a".into()), make_type("b".into())],
                ),
            },
            body: Box::new(Int(1)),
        },
    );
}

#[test]
fn test_func_type() {
    let first = Normal(
        "c".into(),
        vec![make_type("a".into()), make_type("b".into())],
    );
    let second = make_type("a".into());

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
    let res = Func(
        vec![make_type("a".into()), make_type("b".into())],
        Box::new(make_type("c".into())),
    );
    let first = Normal(
        "c".into(),
        vec![make_type("a".into()), make_type("b".into())],
    );
    let second = make_type("a".into());

    assert_str(
        "let name : a b c -> a -> (a -> b -> c) = 1",
        Let {
            ident: Var {
                name: "name".into(),
                type_expr: Func(vec![first, second], Box::new(res)),
            },
            body: Box::new(Int(1)),
        },
    );
}
