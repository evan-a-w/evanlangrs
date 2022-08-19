use std::marker::PhantomData;
use std::str::Chars;
use std::iter::Peakable;

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
            TypeExpr::Normal(_, _) => None,
            TypeExpr::Func(_, _) => None,
            TypeExpr::Useless(_) => None,
        }
    }

    fn try_unpack_normal(self) -> Option<(String, Vec<TypeExpr<Any>>)> {
        match self {
            TypeExpr::Generic(_) => None,
            TypeExpr::Normal(name, args) => Some((name, args)),
            TypeExpr::Func(_, _) => None,
            TypeExpr::Useless(_) => None,
        }
    }

    fn try_unpack_func(self) -> Option<(Vec<TypeExpr<Any>>, Box<TypeExpr<Any>>)> {
        match self {
            TypeExpr::Generic(_) => None,
            TypeExpr::Normal(_, _) => None,
            TypeExpr::Func(args, ret) => Some((args, ret)),
            TypeExpr::Useless(_) => None,
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

fn get_ident(stream: &mut Peakable<Chars>) -> String {
    Ok(stream.take_while(|&c| is_identifier(c)).collect::<String>())
}

fn get_int(stream: &mut Peakable<Chars>) -> Result<i64, LexErr> {
    let s = stream.take_while(|c| c.is_digit(10)).collect::<String>();
    match s.parse::<i64>() {
        Ok(i) => Ok(i),
        Err(_) => Err(LexErr::Expected("integer".to_string(), s)),
    }
}

fn skip_whitespace(stream: &mut Peakable<Chars>) {
    stream.take_while(|c| c.is_whitespace()).count();
}

fn lex_in_type(stream: &mut Peakable<Chars>) -> Result<AST, LexErr> {
    
}

fn lex(stream: &mut Peakable<Chars>) -> Result<AST, LexErr> {
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
