#[derive(PartialEq, Debug)]
pub enum Expression {
    IntegerLiteral(i64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Named(String),
    TypeSym(TypeSymEnum),
    Let(String, Box<Expression>, Box<Expression>),
    InfixOperation(String, Box<Expression>, Box<Expression>),
    Lambda(String, ParamList, Box<Expression>, Box<Expression>),
}

#[derive(PartialEq, Debug)]
pub struct Param {
    pub name: String,
    pub type_expr: Expression,
}

#[derive(PartialEq, Debug)]
pub struct ParamList {
    pub params: Vec<Param>,
}

#[derive(PartialEq, Debug)]
pub enum TypeSymEnum {
    Int,
    String,
    Boolean,
    Star,
}

#[derive(PartialEq, Debug)]
pub struct Program {
    imports: Vec<String>,
    expressions: Vec<Expression>,
}
