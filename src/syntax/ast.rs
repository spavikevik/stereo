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
        Option<Box<Expression>>,
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
pub struct TypeParam {
    pub name: String,
    pub type_expr: Option<Expression>,
}

impl TypeParam {
    pub fn new(name: String) -> Self {
        Self {
            name,
            type_expr: None,
        }
    }

    pub fn new_typed(name: String, type_expr: Expression) -> Self {
        Self {
            name,
            type_expr: Some(type_expr),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_expr: Option<Expression>,
}

impl Param {
    pub fn new(name: String) -> Self {
        Self {
            name,
            type_expr: None,
        }
    }

    pub fn new_typed(name: String, type_expr: Expression) -> Self {
        Self {
            name,
            type_expr: Some(type_expr),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ArgList {
    pub args: Vec<Expression>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ParamList {
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
}

impl ParamList {
    pub fn new() -> Self {
        Self {
            type_params: vec![],
            params: vec![],
        }
    }

    pub fn add_type_param(self, type_param: TypeParam) -> Self {
        let mut type_params = self.type_params.clone();
        type_params.push(type_param);

        Self {
            type_params,
            params: self.params.clone(),
        }
    }

    pub fn add_param(self, param: Param) -> Self {
        let mut params = self.params.clone();
        params.push(param);

        Self {
            type_params: self.type_params.clone(),
            params,
        }
    }

    pub fn set_params(self, params: Vec<Param>) -> Self {
        Self {
            type_params: self.type_params.clone(),
            params,
        }
    }

    pub fn set_type_params(self, type_params: Vec<TypeParam>) -> Self {
        Self {
            type_params,
            params: self.params.clone(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Program {
    imports: Vec<String>,
    expressions: Vec<Expression>,
}
