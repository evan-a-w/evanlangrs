use crate::lexer::*;
use crate::types::*;
use crate::lexer::AST::*;
use crate::lexer::Any;
use crate::lexer::TypeExpr::*;

fn assert_str(s: &str, ast: AST) {
    let mut stream = s.chars().peekable();

    assert_eq!(lex(&mut stream), Ok(ast));
}

fn make_type(s: String) -> TypeExpr<Any> {
    Normal(s, vec![])
}

#[test]
fn test_var() {
    assert_str("let name : a b c = 1", Let {
        ident: Var {
            name: "name".into(),
            type_expr: Normal("c".into(), vec![make_type("a".into()), make_type("b".into())]),
        },
        body: Box::new(Int(1)),
    });

    let res = Func(vec![make_type("a".into()), make_type("b".into())], Box::new(make_type("c".into())));
    let first = Normal("c".into(), vec![make_type("a".into()), make_type("b".into())]);
    let second = make_type("a".into());

    assert_str("let name : a b c -> a -> (a -> b -> c) = 1", Let {
        ident: Var {
            name: "name".into(),
            type_expr: Func(vec![first, second], Box::new(res)),
        },
        body: Box::new(Int(1)),
    });
}
