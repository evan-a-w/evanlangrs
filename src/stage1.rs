use crate::parser::*;
use crate::ast::*;
use crate::stage1_types::*;
use std::collections::HashMap;
use std::collections::HashSet;

fn make_params(params: Vec<String>, trait_spec: Vec<TraitSpec>) -> Vec<TraitSpec> {
    let mut map = trait_spec.into_iter().collect::<HashMap<_, _>>();
    params
        .into_iter()
        .map(|param| {
            let spec = map.remove(&param).unwrap_or_default();
            (param, spec)
        })
        .collect()
}

impl ParseState {
    fn new() -> Self {
        Self {
            types: Vec::new(),
            traits: Vec::new(),
            trait_map: HashMap::new(),
            type_map: HashMap::new(),
        }
    }

    fn process_impls(&mut self, ast: ASTpp) -> ParseResult<ASTp> {
        match ast {
            ASTpp::Impl { trait_name, type_name, trait_specs, body } => {
                let trait_id = self.trait_map.get(&trait_name.0).ok_or_else(|| {
                    ParseErr::TraitNotFound(trait_name.0.clone())
                })?;
            }
        }
    }

    pub fn process_types_and_traits(&mut self, ast: AST) -> ParseResult<ASTpp> {
        Ok(match ast {
            AST::DefType { name: (name, params), trait_specs, fields } => {
                let id = self.add_type(Type {
                    name: name.clone(),
                    params: make_params(params, trait_specs),
                    fields,
                    members: HashMap::new(),
                    traits: HashSet::new(),
                });

                self.type_map
                    .entry(name)
                    .or_insert(Vec::new())
                    .push(id);

                ASTpp::Unit
            }
            AST::Impl { trait_name, type_name, trait_specs, body } => {
                let mut nb = vec![];
                for (name, expr) in body {
                    nb.push((name, self.process_types_and_traits(*expr)?));
                }

                let body = nb;
                ASTpp::Impl {
                    trait_name,
                    type_name,
                    trait_specs,
                    body,
                }
            }
            AST::DefTrait { name: (name, params), items } => {
                let id = self.add_trait(Trait {
                    name: name.clone(),
                    params,
                    members: items,
                });

                if self.trait_map.contains_key(&name) {
                    return Err(ParseErr::DuplicateTrait(name));
                }

                self.trait_map.insert(name, id);

                ASTpp::Unit
            }
            AST::Fn { params, body } => {
                ASTpp::Fn {
                    params,
                    body: Box::new(self.process_types_and_traits(*body)?),
                }
            }
            AST::Let { ident, body } => {
                ASTpp::Let {
                    ident,
                    body: Box::new(self.process_types_and_traits(*body)?),
                }
            }
            AST::Call { name, args } => {
                ASTpp::Call {
                    name,
                    args: {
                        let mut a = vec![];
                        for arg in args.into_iter() {
                            a.push(self.process_types_and_traits(arg)?);
                        }
                        a
                    }
                }
            }
            AST::Scope(things, res) => {
                ASTpp::Scope(
                    {
                        let mut args = vec![];
                        for arg in things.into_iter() {
                            args.push(self.process_types_and_traits(arg)?);
                        }
                        args
                    },
                    Box::new(self.process_types_and_traits(*res)?),
                )
            }
            AST::Unit => ASTpp::Unit,
            AST::Int(i) => ASTpp::Int(i),
            AST::Ident(s) => ASTpp::Ident(s),
        }).and_then(|ast| self.process_impls(ast))
    }

    fn string_to_typeid(&self, name: &str) -> ParseResult<TypeId> {
        self.type_map.get(name).and_then(|v| v.last().cloned()).ok_or(ParseErr::TypeNotFound(name.to_string()))
    }

    fn typeexpr_to_atype(&self, t: TypeExpr) -> ParseResult<AType> {
        Ok(match t {
            TypeExpr::Any => AType::Any,
            TypeExpr::Ident(s) => AType::Ident(self.string_to_typeid(&s)?),
            TypeExpr::Normal(s, v) => AType::Normal(
                self.string_to_typeid(&s)?,
                {
                    let mut a = vec![];
                    for arg in v.into_iter() {
                        a.push(self.typeexpr_to_atype(arg)?);
                    }
                    a
                },
            ),
            TypeExpr::Func(v, b) => AType::Func(
                {
                    let mut a = vec![];
                    for arg in v.into_iter() {
                        a.push(self.typeexpr_to_atype(arg)?);
                    }
                    a
                },
                Box::new(self.typeexpr_to_atype(*b)?),
            ),
        })
    }

    pub fn var_to_avar(&mut self, var: Var) -> ParseResult<AVar> {
        let Var { name, type_expr } = var;
        let atype = self.typeexpr_to_atype(type_expr)?;

        Ok(AVar { name, type_expr: atype })
    }

    pub fn make_a(&mut self, from: ASTp) -> ParseResult<A> {
        Ok(match from {
            ASTp::Unit => A::Unit,
            ASTp::Fn { params, body } => A::Fn {
                params: {
                    let mut a = vec![];
                    for arg in params.into_iter() {
                        a.push(self.var_to_avar(arg)?);
                    }
                    a
                },
                body: Box::new(self.make_a(*body)?),
            },
            ASTp::Let { ident, body } => A::Let {
                ident: self.var_to_avar(ident)?,
                body: Box::new(self.make_a(*body)?),
            },
            ASTp::Call { name, args } => A::Call {
                name,
                args: {
                    let mut a = vec![];
                    for arg in args.into_iter() {
                        a.push(self.make_a(arg)?);
                    }
                    a
                }
            },
            ASTp::Scope(things, res) => A::Scope(
                {
                    let mut args = vec![];
                    for arg in things.into_iter() {
                        args.push(self.make_a(arg)?);
                    }
                    args
                },
                Box::new(self.make_a(*res)?),
            ),
            ASTp::Int(i) => A::Int(i),
            ASTp::Ident(s) => A::Ident(s),
        })
    }

    fn add_type(&mut self, t: Type) -> usize {
        self.types.push(t);
        self.types.len() - 1
    }

    fn add_trait(&mut self, t: Trait) -> usize {
        self.traits.push(t);
        self.traits.len() - 1
    }
}
