use std::marker::PhantomData;

type FuncType = ();
type SumType = ();
type AnyType = ();
type ProductType = ();

struct Type<K> {
    name: String,
    fields: Field<K>,
    methods: Vec<Method>,
    traits: Vec<TraidId>,
    kind: PhantomData<K>,
}

enum Field<K> {
    Product(Vec<Type<K>>),
    Sum(Vec<Type<K>>),
    Func(Vec<Type<K>>, Box<Type<K>>),
}

struct Method {
    name: String,
    method: Type<FuncType>,
}

struct Trait<K> {
    name: String,
    items: Vec<Type<K>>,
}

enum CompTime {}

type TraidId = usize;
