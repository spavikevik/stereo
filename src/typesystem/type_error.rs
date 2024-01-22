use std::fmt::{Debug, Formatter};

use crate::syntax::ast::Expression;
use crate::typesystem::r#type::Type;
use crate::typesystem::type_environment::TypeEnvironment;

#[derive(Clone, Eq, PartialEq)]
pub enum TypeError {
    UnificationError(Type, Type),
    IsFreeTypeVariableError(String, Type),
    TypeNotInContextError(String),
}

impl TypeError {
    pub fn into_error_report(self) -> TypeErrorReport {
        let mut report = TypeErrorReport::new();
        report.add_error(self);

        report
    }
}

impl Debug for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnificationError(a, b) => {
                write!(f, "Couldn't unify types {:?} with {:?}", a, b)
            }
            TypeError::IsFreeTypeVariableError(a, b) => {
                write!(f, "{:?} is a free variable in {:?}", a, b)
            }
            TypeError::TypeNotInContextError(a) => write!(f, "Type {:?} is not in context", a),
        }
    }
}

#[derive(Eq, PartialEq, Clone)]
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

    pub fn add_error(&mut self, type_error: TypeError) -> () {
        self.errors.push(type_error)
    }

    pub fn add_context(&mut self, ast: Expression, env: TypeEnvironment) -> () {
        self.ast = Some(ast);
        self.env = Some(env);
    }

    pub fn get_errors(&self) -> &Vec<TypeError> {
        &self.errors
    }
}

impl Debug for TypeErrorReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Couldn't infer types for {:?};\n", &self.ast)?;
        Ok(for error in &self.errors {
            write!(f, "{:?}\n", error)?;
        })
    }
}
