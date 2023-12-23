#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, Debug)]
pub struct Param {
    pub name: String,
    pub type_expr: Expression,
}

#[derive(PartialEq, Debug)]
pub struct ArgList {
    pub args: Vec<Expression>,
}

#[derive(PartialEq, Debug)]
pub struct ParamList {
    pub params: Vec<Param>,
}

#[derive(PartialEq, Debug)]
pub struct Program {
    imports: Vec<String>,
    expressions: Vec<Expression>,
}
