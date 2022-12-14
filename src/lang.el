(typed (<type_name>) <expr>)
expr : type_name

(deftype (<type_params> <name>) (where (<type_param> is [<trait>, ...]) ...)?
  { <field> of <type>, ... })
type a b ... name `where a is [trait1, ...], b is ...`? = { field of a, ... }

(deftype (<type_params> <name>) (where (<type_param> is [<trait>, ...]) ...)?
  ( <variant> of <type>?, ... ))
type a b ... name ( variant of a?, ... )

(deftrait (<trait_params> <name>) (where (<trait_param> is [<trait>, ...]) ...)?
  { <method> of <type>, ... })
trait a b ... name { thingy of type, ... }

(fn (<params>) <expr>)
fn (a, b, c, ...) <expr>

type = a b identifier | type list sep by "->" | [[ <trait name>, ... ]]

let ident = expr

impl a b trait for a b c where a is [trait1, ...], b is ... {
    let method1 = ...
    let method2 = ...
}

{
    expr1;
    expr2
}

type_expr := (type_expr) | func_type_expr | normal_type_expr
func_type_expr := type_expr -> type_expr -> ... -> type_expr
normal_type_expr := a b c ... t
