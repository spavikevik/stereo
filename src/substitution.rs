use crate::r#type::Type;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Substitution {
    pub type_var_map: HashMap<String, Type>,
}

impl Substitution {
    pub fn new() -> Self {
        Self {
            type_var_map: HashMap::new(),
        }
    }

    pub fn from_iter<I: Iterator<Item = (String, Type)>>(it: I) -> Self {
        Self {
            type_var_map: HashMap::from_iter(it),
        }
    }

    pub fn compose(&self, other: &Substitution) -> Self {
        let new_type_var_map: HashMap<String, Type> = other
            .type_var_map
            .clone()
            .iter()
            .map(|(k, v)| (k.clone(), v.apply_substitution(other)))
            .collect();

        Substitution {
            type_var_map: self
                .type_var_map
                .clone()
                .into_iter()
                .chain(new_type_var_map)
                .collect(),
        }
    }
}

pub trait Substitutable {
    fn free_type_vars(&self) -> Vec<String>;
    fn apply_substitution(&self, substitution: &Substitution) -> Self;
}
