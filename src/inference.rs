use crate::r#type::Type;
use crate::substitution::Substitution;

pub enum Inference {
    Complete(Type),
    Partial(Type, Substitution),
}

impl Inference {
    pub fn into_tuple(self) -> (Type, Substitution) {
        match self {
            Inference::Complete(ty) => (ty, Substitution::new()),
            Inference::Partial(ty, subst) => (ty, subst),
        }
    }
}
