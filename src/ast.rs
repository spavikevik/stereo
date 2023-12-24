#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expression {
    IntegerLiteral(i64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Named(String),
    Let(String, Box<Expression>, Box<Expression>),
    Lambda(Option<String>, ParamList, Box<Expression>, Box<Expression>),
    // TODO: Drop separate InfixOperation node and unify it under Application
    InfixOperation(String, Box<Expression>, Box<Expression>),
    Application(Box<Expression>, ArgList),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Param {
    pub name: String,
    pub type_expr: Expression,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ArgList {
    pub args: Vec<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ParamList {
    pub params: Vec<Param>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Program {
    imports: Vec<String>,
    expressions: Vec<Expression>,
}
