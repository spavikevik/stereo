use crate::ast::Expression;
use crate::r#type::Type;
use crate::type_environment::TypeEnvironment;
use std::fmt::{Debug, Formatter};

#[derive(Clone, Eq, PartialEq)]
pub enum TypeError {
    UnificationError(Type, Type),
    IsFreeTypeVariableError(String, Type),
    TypeNotInContextError(String),
}

impl TypeError {
    pub fn into_error_report(self) -> TypeErrorReport {
        TypeErrorReport::new().add_error(self)
    }
}

impl Debug for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnificationError(a, b) => {
                write!(f, "Couldn't unify type {:?} with {:?}", a, b)
            }
            TypeError::IsFreeTypeVariableError(a, b) => {
                write!(f, "{:?} is a free variable in {:?}", a, b)
            }
            TypeError::TypeNotInContextError(a) => write!(f, "Type {:?} is not in context", a),
        }
    }
}

#[derive(Eq, PartialEq)]
pub struct TypeErrorReport {
    errors: Vec<TypeError>,
    ast: Option<Expression>,
    env: Option<TypeEnvironment>,
}

impl TypeErrorReport {
    pub fn new() -> Self {
        Self {
            errors: vec![],
            ast: None,
            env: None,
        }
    }

    pub fn add_error(self, type_error: TypeError) -> Self {
        Self {
            errors: self.errors.into_iter().chain(vec![type_error]).collect(),
            ast: self.ast.clone(),
            env: self.env.clone(),
        }
    }

    pub fn add_context(self, ast: Expression, env: TypeEnvironment) -> Self {
        Self {
            errors: self.errors.clone(),
            ast: Some(ast),
            env: Some(env),
        }
    }

    pub fn get_errors(self) -> Vec<TypeError> {
        self.errors.clone()
    }
}

impl Debug for TypeErrorReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Couldn't infer type for {:?};\n", &self.ast)?;
        Ok(for error in &self.errors {
            write!(f, "{:?}\n", error)?;
        })
    }
}
