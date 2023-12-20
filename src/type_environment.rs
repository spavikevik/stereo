use crate::r#type::{Type, TypeScheme};
use crate::substitution::{Substitutable, Substitution};
use std::collections::HashMap;

#[derive(Clone)]
pub struct TypeEnvironment {
    pub bindings: HashMap<String, TypeScheme>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        TypeEnvironment {
            bindings: HashMap::new(),
        }
    }

    pub fn add_type_binding(self, name: String, tpe: Type) -> Self {
        let mut bindings = self.bindings.clone();
        bindings.insert(name, TypeScheme::from_type(tpe));
        Self { bindings }
    }

    pub fn add_binding(self, name: String, tpe: TypeScheme) -> Self {
        let mut bindings = self.bindings.clone();
        bindings.insert(name.clone(), tpe);
        Self { bindings }
    }

    pub fn remove_binding(self, name: &str) -> Self {
        let mut bindings = self.bindings.clone();
        bindings.remove(name);
        Self { bindings }
    }
}

impl Substitutable for TypeEnvironment {
    fn free_type_vars(&self) -> Vec<String> {
        self.bindings
            .values()
            .map(|t| t.free_type_vars())
            .flatten()
            .collect()
    }
    fn apply_substitution(&self, substitution: &Substitution) -> Self {
        let new_bindings = self
            .bindings
            .iter()
            .map(|(k, v)| (k.clone(), v.apply_substitution(substitution)))
            .collect();
        TypeEnvironment {
            bindings: new_bindings,
        }
    }
}
