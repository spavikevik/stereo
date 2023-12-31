use crate::r#type::Type;
use crate::substitution::Substitution;

pub enum Inference {
    Complete(Type),
    Partial(Type, Substitution),
}

impl Inference {
    pub fn as_tuple(&self) -> (Type, Substitution) {
        match self {
            Inference::Complete(ty) => (ty.clone(), Substitution::new()),
            Inference::Partial(ty, subst) => (ty.clone(), subst.clone()),
        }
    }
}
