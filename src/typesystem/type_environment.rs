use std::collections::HashMap;

use crate::typesystem::r#type::{Type, TypeScheme};
use crate::typesystem::substitution::{Substitutable, Substitution};

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TypeEnvironment {
    pub bindings: HashMap<String, TypeScheme>,
    pub aliases: HashMap<String, Type>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        TypeEnvironment {
            bindings: HashMap::new(),
            aliases: HashMap::new(),
        }
    }

    pub fn add_type_alias(&mut self, name: String, tpe: Type) -> () {
        self.aliases.insert(name, tpe);
    }

    pub fn add_type_binding(&mut self, name: String, tpe: Type) -> () {
        self.bindings.insert(name, TypeScheme::from_type(tpe));
    }

    pub fn add_binding(&mut self, name: String, tpe: TypeScheme) -> () {
        self.bindings.insert(name.clone(), tpe);
    }

    pub fn remove_binding(&mut self, name: &str) -> () {
        self.bindings.remove(name);
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
            aliases: self.aliases.clone(),
        }
    }
}
