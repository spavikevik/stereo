use crate::r#type::Type;
use std::fmt::{Debug, Formatter};

#[derive(Eq, PartialEq)]
pub enum TypeError {
    UnificationError(Type, Type),
    IsFreeTypeVariableError(String, Type),
    TypeNotInContextError(String),
}

impl TypeError {
    pub fn into_error_report(self) -> TypeErrorReport {
        TypeErrorReport(vec![self])
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
pub struct TypeErrorReport(Vec<TypeError>);

impl TypeErrorReport {
    pub fn new() -> Self {
        TypeErrorReport(vec![])
    }

    pub fn add_error(self, type_error: TypeError) -> Self {
        TypeErrorReport(self.0.into_iter().chain(vec![type_error]).collect())
    }

    pub fn combine(self, other: Self) -> Self {
        TypeErrorReport(self.0.into_iter().chain(other.0).collect())
    }
}

impl Debug for TypeErrorReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(for error in &self.0 {
            write!(f, "{:?}\n", error)?;
        })
    }
}
