#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expression {
    IntegerLiteral(i64),
    StringLiteral(String),
    BooleanLiteral(bool),
    Named(String),
    Let(String, Box<Expression>, Box<Expression>),
    Lambda(
        Option<String>,
        ParamList,
        Box<Expression>,
        Box<Expression>,
        Option<OperatorMetadata>,
    ),
    Application(Box<Expression>, ArgList),
}

impl Expression {
    pub fn infix_operation(operator: Expression, lhs: Expression, rhs: Expression) -> Expression {
        Expression::Application(
            Box::new(operator),
            ArgList {
                args: vec![lhs, rhs],
            },
        )
    }

    pub fn prefix_operation(operator: Expression, arg: Expression) -> Expression {
        Expression::Application(Box::new(operator), ArgList { args: vec![arg] })
    }

    pub fn postfix_operation(operator: Expression, arg: Expression) -> Expression {
        Expression::Application(Box::new(operator), ArgList { args: vec![arg] })
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct OperatorMetadata {
    pub position: AffixPosition,
    pub associativity: Associativity,
    pub precedence: i8,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AffixPosition {
    Pre,
    In,
    Post,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(PartialEq, Eq, Debug, Clone)]
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
