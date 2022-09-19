use std::collections::HashMap;

pub type Traits = Vec<String>;
pub type TraitSpec = (String, Traits);

#[derive(PartialEq, Debug, Clone)]
pub enum AST {
    DefType {
        name: (String, Vec<String>),
        trait_specs: Vec<TraitSpec>,
        fields: SumOrProd,
    },
    // TODO: Write Impl
    DefTrait {
        name: (String, Vec<String>),
        items: HashMap<String, TypeExpr>,
    },
    Fn {
        params: Vec<Var>,
        body: Box<AST>,
    },
    Let {
        ident: Var,
        body: Box<AST>,
    },
    Call {
        name: String,
        args: Vec<AST>,
    },
    Scope(Vec<AST>, Box<AST>),
    Unit,
    Int(i64),
    Ident(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
    Any,
    Ident(String),
    Normal(String, Vec<TypeExpr>),
    Func(Vec<TypeExpr>, Box<TypeExpr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub name: String,
    pub type_expr: TypeExpr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SumOrProd {
    Sum(HashMap<String, Option<TypeExpr>>),
    Prod(HashMap<String, TypeExpr>),
}
