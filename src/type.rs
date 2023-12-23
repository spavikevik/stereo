#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Primitive(PrimitiveType),
    TypeVar(String),
    Bottom,
    Function(Box<Type>, Box<Type>),
    ForAll(String, Box<Type>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum PrimitiveType {
    Numeric,
    Int,
    String,
    Bool,
    Star,
}
