use std::collections::HashSet;
use std::fmt::{Debug, Formatter};

use crate::substitution::{Substitutable, Substitution};

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
    TypeVar(String),
    Bottom,
    Function(Box<Type>, Box<Type>),
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Primitive(p) => write!(f, "{:?}", p),
            Type::TypeVar(s) => write!(f, "{}", s),
            Type::Bottom => write!(f, "⊥"),
            Type::Function(t1, t2) => write!(f, "({:?}) -> {:?}", t1, t2),
        }
    }
}

impl Substitutable for Type {
    fn free_type_vars(&self) -> Vec<String> {
        match self {
            Type::TypeVar(s) => vec![s.clone()],
            Type::Function(t1, t2) => {
                let mut free_vars = t1.free_type_vars();
                free_vars.append(&mut t2.free_type_vars());
                free_vars
            }
            _ => vec![],
        }
    }

    fn apply_substitution(&self, substitution: &Substitution) -> Type {
        match self {
            Type::TypeVar(s) => match substitution.type_var_map.get(s) {
                Some(t) => t.clone(),
                None => self.clone(),
            },
            Type::Function(t1, t2) => Type::Function(
                Box::new(t1.apply_substitution(substitution)),
                Box::new(t2.apply_substitution(substitution)),
            ),
            _ => self.clone(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PrimitiveType {
    Numeric,
    Int,
    String,
    Bool,
    Star,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeScheme {
    pub type_vars: Vec<String>,
    pub tpe: Type,
}

impl TypeScheme {
    pub fn from_type(tpe: Type) -> TypeScheme {
        TypeScheme {
            type_vars: vec![],
            tpe,
        }
    }

    pub fn add_type_vars(&mut self, type_vars: &mut Vec<String>) -> () {
        self.type_vars.append(type_vars)
    }
}

impl Substitutable for TypeScheme {
    fn free_type_vars(&self) -> Vec<String> {
        let type_fv: HashSet<String> = HashSet::from_iter(self.tpe.free_type_vars());
        let vars: HashSet<String> = HashSet::from_iter(self.type_vars.clone().into_iter());

        type_fv
            .symmetric_difference(&vars)
            .into_iter()
            .map(|s| s.to_owned())
            .collect()
    }

    fn apply_substitution(&self, substitution: &Substitution) -> TypeScheme {
        let filtered_substitution = substitution.filter_type_vars(&self.type_vars);
        let tpe = self.tpe.apply_substitution(&filtered_substitution);

        TypeScheme {
            type_vars: self.type_vars.clone(),
            tpe,
        }
    }
}
