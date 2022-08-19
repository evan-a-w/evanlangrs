enum TypeExpr {
    Generic(Vec<String>),
    Parametrised(String, Vec<TypeExpr>),
    Func(Vec<TypeExpr>, Box<TypeExpr>),
}

enum SumOrProd {
    Sum(Vec<(String, Option<TypeExpr>)>),
    Prod(Vec<(String, TypeExpr)>),
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
        items: Vec<TypeExpr>,
    },
}
