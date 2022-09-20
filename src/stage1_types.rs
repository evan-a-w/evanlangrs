use crate::ast::*;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(PartialEq, Debug, Clone)]
pub enum ASTpp {
    Fn {
        params: Vec<Var>,
        body: Box<ASTpp>,
    },
    Let {
        ident: Var,
        body: Box<ASTpp>,
    },
    Impl {
        trait_name: (String, Vec<String>),
        type_name: (String, Vec<String>),
        trait_specs: Vec<TraitSpec>,
        body: Vec<(Var, Box<ASTpp>)>,
    },
    Call {
        name: String,
        args: Vec<ASTpp>,
    },
    Scope(Vec<ASTpp>, Box<ASTpp>),
    Unit,
    Int(i64),
    Ident(String),
}

#[derive(PartialEq, Debug, Clone)]
pub enum ASTp {
    Fn {
        params: Vec<Var>,
        body: Box<ASTp>,
    },
    Let {
        ident: Var,
        body: Box<ASTp>,
    },
    Call {
        name: String,
        args: Vec<ASTp>,
    },
    Scope(Vec<ASTp>, Box<ASTp>),
    Unit,
    Int(i64),
    Ident(String),
}

#[derive(PartialEq, Debug, Clone)]
pub enum A {
    Fn {
        params: Vec<AVar>,
        body: Box<A>,
    },
    Let {
        ident: AVar,
        body: Box<A>,
    },
    Call {
        name: String,
        args: Vec<A>,
    },
    Scope(Vec<A>, Box<A>),
    Unit,
    Int(i64),
    Ident(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AVar {
    pub name: String,
    pub type_expr: AType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AType {
    Any,
    Ident(TypeId),
    Normal(TypeId, Vec<AType>),
    Func(Vec<AType>, Box<AType>),
}

#[derive(Clone, Debug)]
pub struct Type {
    name: String,
    params: Vec<TraitSpec>,
    fields: SumOrProd,
    members: HashMap<String, TypeExpr>,
    traits: HashSet<TraidId>,
}

#[derive(Clone, Debug)]
pub struct Trait {
    name: String,
    params: Vec<String>,
    members: HashMap<String, TypeExpr>,
}

pub type TraidId = usize;
pub type TypeId = usize;

#[derive(Clone, Debug)]
pub struct ParseState {
    types: Vec<Type>,
    traits: Vec<Trait>,
    trait_map: HashMap<String, TraidId>,
    type_map: HashMap<String, Vec<TypeId>>,
}
