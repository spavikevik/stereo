use std::cell::Cell;
use std::collections::HashMap;

use crate::syntax::ast::{ArgList, Expression, Param, ParamList, TypeParam};
use crate::typesystem::inference::Inference;
use crate::typesystem::r#type::{PrimitiveType, Type, TypeScheme};
use crate::typesystem::substitution::{Substitutable, Substitution};
use crate::typesystem::type_environment::TypeEnvironment;
use crate::typesystem::type_error::{TypeError, TypeErrorReport};

type InferenceResult = Result<Inference, TypeErrorReport>;

struct Typer<'a> {
    builtins: &'a TypeEnvironment,
    ti_supply: Cell<i32>,
}

impl<'a> Typer<'a> {
    pub fn new(builtins: &'a TypeEnvironment) -> Self {
        Self {
            builtins,
            ti_supply: Cell::new(0),
        }
    }

    pub fn infer_and_panic(&self, expression: Expression, env: TypeEnvironment) -> Type {
        match Typer::infer(self, expression, env) {
            Ok(tpe) => tpe,
            Err(err_report) => panic!("Type inference error:\n{:?}", err_report),
        }
    }
    pub fn infer(
        &self,
        expression: Expression,
        env: TypeEnvironment,
    ) -> Result<Type, TypeErrorReport> {
        let (tpe, subst) = Typer::ti(self, expression, env)?.into_tuple();
        Ok(tpe.apply_substitution(&subst))
    }

    #[inline]
    fn ti(&self, expression: Expression, env: TypeEnvironment) -> InferenceResult {
        let expr_clone = expression.clone();
        match expression.clone() {
            Expression::IntegerLiteral(_) => {
                Ok(Inference::Complete(Type::Primitive(PrimitiveType::Numeric)))
            }
            Expression::StringLiteral(_) => {
                Ok(Inference::Complete(Type::Primitive(PrimitiveType::String)))
            }
            Expression::BooleanLiteral(_) => {
                Ok(Inference::Complete(Type::Primitive(PrimitiveType::Bool)))
            }
            Expression::Named(name) => {
                let bot = TypeScheme::from_type(Type::Bottom);

                let scheme = env
                    .bindings
                    .get(name.as_str())
                    .or(self.builtins.bindings.get(name.as_str()))
                    .unwrap_or(&bot);

                Ok(Inference::Complete(scheme.tpe.clone()))
            }
            Expression::Let(_, _, expr) => Typer::ti(self, *expr, env),
            Expression::Lambda(
                _,
                ParamList {
                    type_params,
                    params,
                },
                _,
                body,
                _,
            ) => {
                let (res, param_types) = Typer::collect_param_env(self, type_params, params, env);
                let new_env = res?;
                let (return_type, substitution) = Typer::ti(self, *body, new_env)?.into_tuple();

                Ok(Inference::Partial(
                    param_types.iter().fold(return_type, |acc, tpe| {
                        Type::Function(
                            Box::new(tpe.apply_substitution(&substitution).clone()),
                            Box::new(acc.clone()),
                        )
                    }),
                    substitution,
                ))
            }
            Expression::Application(applicable, ArgList { args }) => {
                let (fn_type, fn_subst) = Typer::ti(self, *applicable, env.clone())?.into_tuple();

                args.iter().fold(
                    Ok(Inference::Partial(fn_type.clone(), fn_subst.clone())),
                    |acc, next_arg_expr| {
                        let (acc_tpe, acc_subst) = acc?.into_tuple();
                        let ret_tpe = Typer::new_type_var(self, "return".to_string());

                        let (arg_tpe, arg_subst) = Typer::ti(
                            self,
                            next_arg_expr.clone(),
                            env.apply_substitution(&fn_subst),
                        )?
                        .into_tuple();

                        let subst = Typer::unify(
                            self,
                            acc_tpe.clone(),
                            Type::Function(Box::new(arg_tpe), Box::new(ret_tpe.clone())),
                        )
                        .map_err(|err| {
                            let mut err_with_ctx = err.clone();
                            err_with_ctx.add_context(expr_clone.clone(), env.clone());
                            err_with_ctx
                        })?;

                        Ok(Inference::Partial(
                            ret_tpe.clone().apply_substitution(&subst),
                            subst.compose(&arg_subst).compose(&acc_subst),
                        ))
                    },
                )
            }
        }
    }

    fn unify(&self, tpe1: Type, tpe2: Type) -> Result<Substitution, TypeErrorReport> {
        match (tpe1, tpe2) {
            (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => Ok(Substitution::new()),
            (Type::TypeVar(name), tpe) => Ok(Typer::unify_bind_type_var(self, name, tpe))?,
            (tpe, Type::TypeVar(name)) => Ok(Typer::unify_bind_type_var(self, name, tpe))?,
            (Type::Bottom, _) => Ok(Substitution::new()),
            (Type::Function(left1, right1), Type::Function(left2, right2)) => {
                let sub1 = self.unify(*left1, *left2)?;
                let sub2 = self.unify(
                    right1.apply_substitution(&sub1),
                    right2.apply_substitution(&sub1),
                )?;
                Ok(sub1.compose(&sub2))
            }
            (tpe1, tpe2) => Err(TypeError::UnificationError(tpe1, tpe2).into_error_report()),
        }
    }

    #[inline]
    fn instantiate(&self, scheme: TypeScheme) -> Type {
        let new_vars = scheme
            .type_vars
            .iter()
            .map(|name| self.new_type_var(name.clone()));

        let substitution =
            Substitution::from_iter(scheme.clone().type_vars.into_iter().zip(new_vars));

        scheme.tpe.apply_substitution(&substitution)
    }

    #[inline]
    fn new_type_var(&self, prefix: String) -> Type {
        let ti_supply = self.ti_supply.get();
        self.ti_supply.set(ti_supply + 1);
        Type::TypeVar(format!("{}{}", prefix, ti_supply))
    }

    #[inline]
    fn unify_bind_type_var(
        &self,
        name: String,
        tpe: Type,
    ) -> Result<Substitution, TypeErrorReport> {
        match tpe {
            Type::TypeVar(name2) if name2 == name => Ok(Substitution::new()),
            tpe if tpe.free_type_vars().contains(&name) => {
                Err(TypeError::IsFreeTypeVariableError(name, tpe).into_error_report())
            }
            _ => Ok(Substitution {
                type_var_map: HashMap::from_iter([(name, tpe)]),
            }),
        }
    }

    // TODO: This should probably rely on proper expression evaluation
    #[inline]
    fn eval_type_expr(
        &self,
        expression: Expression,
        type_params: Vec<TypeParam>,
    ) -> Result<Type, TypeError> {
        match expression {
            Expression::Named(name) => match Typer::get_builtin_type(self, name.as_str()) {
                Some(tpe) => Ok(tpe),
                None => {
                    let type_var = Type::TypeVar(name.clone());

                    if type_params
                        .iter()
                        .find(|type_param| type_param.name == name.clone())
                        .is_some()
                    {
                        Ok(type_var)
                    } else {
                        Err(TypeError::IsFreeTypeVariableError(name.clone(), type_var))
                    }
                }
            },
            _ => Ok(Type::Bottom),
        }
    }

    #[inline]
    fn get_builtin_type(&self, name: &str) -> Option<Type> {
        self.builtins.aliases.get(name).map(|tpe| tpe.clone())
    }

    #[inline]
    fn collect_param_env(
        &self,
        type_params_list: Vec<TypeParam>,
        params_list: Vec<Param>,
        env: TypeEnvironment,
    ) -> (Result<TypeEnvironment, TypeErrorReport>, Vec<Type>) {
        let param_env = env.clone();
        let mut types: Vec<Type> = vec![];

        (
            params_list.iter().fold(Ok(param_env), |acc, param| {
                // let env = acc?;
                let param_clone = param.clone();

                let param_type = match param_clone.type_expr {
                    None => Ok(Typer::new_type_var(self, param_clone.name.clone())),
                    Some(type_expr) => {
                        Typer::eval_type_expr(self, type_expr, type_params_list.clone())
                    }
                };

                match (acc, param_type) {
                    (Err(errs), Err(type_err)) => {
                        let mut new_errs = errs.clone();
                        new_errs.add_error(type_err);

                        Err(new_errs)
                    }
                    (Err(errs), _) => Err(errs),
                    (Ok(env), param_type_result) => {
                        let param_type = param_type_result.map_err(TypeError::into_error_report)?;

                        types.push(param_type.clone());

                        let mut param_type_scheme = TypeScheme::from_type(param_type);
                        param_type_scheme.add_type_vars(
                            &mut type_params_list
                                .iter()
                                .map(|type_param| type_param.name.clone())
                                .collect::<Vec<_>>()
                                .clone(),
                        );

                        let mut new_env = env.clone();
                        new_env.remove_binding(param_clone.name.as_str());
                        new_env.add_binding(param_clone.name, param_type_scheme.to_owned());
                        Ok(new_env)
                    }
                }
            }),
            types,
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::ast::{ArgList, Expression, Param, ParamList, TypeParam};
    use crate::typesystem::r#type::{PrimitiveType, Type};
    use crate::typesystem::type_error::TypeError;
    use crate::typesystem::typer::{TypeEnvironment, Typer};
    use crate::{
        application, bool_lit, infix, int_lit, lambda, let_expr, named, param, string_lit,
        type_param,
    };

    #[test]
    fn test_literal() {
        let env = TypeEnvironment::new();

        let builtins = &TypeEnvironment::new();

        let typer = Typer::new(builtins);

        assert_eq!(
            typer.infer_and_panic(int_lit!(1), env.clone()),
            Type::Primitive(PrimitiveType::Numeric)
        );

        assert_eq!(
            typer.infer_and_panic(string_lit!("Hello"), env.clone()),
            Type::Primitive(PrimitiveType::String)
        );

        assert_eq!(
            typer.infer_and_panic(bool_lit!(true), env.clone()),
            Type::Primitive(PrimitiveType::Bool)
        );
    }

    #[test]
    fn test_named() {
        let mut env = TypeEnvironment::new();

        env.add_type_binding("x".to_string(), Type::Primitive(PrimitiveType::Int));
        env.add_type_binding("X".to_string(), Type::Primitive(PrimitiveType::Star));

        let builtins = &TypeEnvironment::new();

        let typer = Typer::new(builtins);

        assert_eq!(
            typer.infer_and_panic(named! { "x" }, env.clone()),
            Type::Primitive(PrimitiveType::Int)
        );

        assert_eq!(
            typer.infer_and_panic(named! { "X" }, env.clone()),
            Type::Primitive(PrimitiveType::Star)
        )
    }

    #[test]
    fn test_let() {
        let env = TypeEnvironment::new();
        let builtins = &TypeEnvironment::new();

        let typer = Typer::new(builtins);

        assert_eq!(
            typer.infer_and_panic(let_expr! {"x" : named!("int") => int_lit!(3) }, env.clone()),
            Type::Primitive(PrimitiveType::Numeric)
        );
    }

    #[test]
    fn test_infix_operation() {
        let env = TypeEnvironment::new();
        let builtins = &mut TypeEnvironment::new();

        builtins.add_type_binding(
            "+".to_string(),
            Type::Function(
                Box::new(Type::Primitive(PrimitiveType::Numeric)),
                Box::new(Type::Function(
                    Box::new(Type::Primitive(PrimitiveType::Numeric)),
                    Box::new(Type::Primitive(PrimitiveType::Numeric)),
                )),
            ),
        );

        let typer = Typer::new(builtins);
        assert_eq!(
            typer.infer_and_panic(infix! { "+", int_lit!(3), int_lit!(4) }, env),
            Type::Primitive(PrimitiveType::Numeric)
        )
    }

    #[test]
    fn test_lambda() {
        let env = TypeEnvironment::new();
        let builtins = &mut TypeEnvironment::new();

        builtins.add_type_binding(
            "==".to_string(),
            Type::Function(
                Box::new(Type::Primitive(PrimitiveType::Int)),
                Box::new(Type::Function(
                    Box::new(Type::Primitive(PrimitiveType::Int)),
                    Box::new(Type::Primitive(PrimitiveType::Bool)),
                )),
            ),
        );

        builtins.add_type_alias("int".to_string(), Type::Primitive(PrimitiveType::Int));

        let typer = Typer::new(builtins);

        assert_eq!(
            typer.infer_and_panic(
                lambda! { "identity", { param!("x": named!("int")) } -> named!("int"), body: named!("x") },
                env.clone()
            ),
            Type::Function(
                Box::new(Type::Primitive(PrimitiveType::Int)),
                Box::new(Type::Primitive(PrimitiveType::Int))
            )
        );

        assert_eq!(
            typer.infer(
                lambda! { "identity", { param!("x": named!("a")) } -> named!("a"), body: named!("x") },
                env.clone()
            )
                .unwrap_err()
                .get_errors(),
            &vec![TypeError::IsFreeTypeVariableError(
                "a".to_string(),
                Type::TypeVar("a".to_string()),
            )]
        );

        assert_eq!(
            typer.infer_and_panic(
                lambda! { "identity", types: type_param!("a"), { param!("x": named!("a")) } -> named!("a"), body: named!("x") },
                env.clone()
            ),
            Type::Function(
                Box::new(Type::TypeVar("a".to_string())),
                Box::new(Type::TypeVar("a".to_string()))
            )
        );

        assert_eq!(
            typer.infer_and_panic(
                lambda! { "const", {param!("x": named!("int")); param!("y": named!("int"))} -> named!("int"), body: named!("x") },
                env.clone()
            ),
            Type::Function(
                Box::new(Type::Primitive(PrimitiveType::Int)),
                Box::new(Type::Function(
                    Box::new(Type::Primitive(PrimitiveType::Int)),
                    Box::new(Type::Primitive(PrimitiveType::Int))
                ))
            )
        );

        assert_eq!(
            typer.infer_and_panic(
                lambda! { "isEqual", {param!("x": named!("int")); param!("y": named!("int"))} -> named!("bool"), body: infix!("==", named!("x"), named!("y")) },
                env.clone()
            ),
            Type::Function(
                Box::new(Type::Primitive(PrimitiveType::Int)),
                Box::new(Type::Function(
                    Box::new(Type::Primitive(PrimitiveType::Int)),
                    Box::new(Type::Primitive(PrimitiveType::Bool))
                ))
            )
        )
    }

    #[test]
    fn test_application() {
        let mut env = TypeEnvironment::new();

        env.add_type_binding(
            "combine".to_string(),
            Type::Function(
                Box::new(Type::Primitive(PrimitiveType::String)),
                Box::new(Type::Function(
                    Box::new(Type::Primitive(PrimitiveType::String)),
                    Box::new(Type::Primitive(PrimitiveType::String)),
                )),
            ),
        );
        env.add_type_binding(
            "identity".to_string(),
            Type::Function(
                Box::new(Type::TypeVar("a".to_string())),
                Box::new(Type::TypeVar("a".to_string())),
            ),
        );
        env.add_type_binding(
            "combine2".to_string(),
            Type::Function(
                Box::new(Type::TypeVar("b".to_string())),
                Box::new(Type::Function(
                    Box::new(Type::TypeVar("b".to_string())),
                    Box::new(Type::TypeVar("b".to_string())),
                )),
            ),
        );
        env.add_type_binding(
            "combine3".to_string(),
            Type::Function(
                Box::new(Type::Primitive(PrimitiveType::String)),
                Box::new(Type::Function(
                    Box::new(Type::Primitive(PrimitiveType::Bool)),
                    Box::new(Type::Primitive(PrimitiveType::Bool)),
                )),
            ),
        );

        let builtins = &TypeEnvironment::new();

        let typer = Typer::new(builtins);

        assert_eq!(
            typer.infer_and_panic(
                application! { named!("combine"), { string_lit!("Hello, "), string_lit!("World!") } },
                env.clone()
            ),
            Type::Primitive(PrimitiveType::String)
        );

        assert_eq!(
            typer.infer_and_panic(
                application! { named!("identity"), { string_lit!("hello") } },
                env.clone()
            ),
            Type::Primitive(PrimitiveType::String)
        );

        assert_eq!(
            typer.infer_and_panic(
                application! { named!("combine2"), { named!("identity") } },
                env.clone()
            ),
            Type::Function(
                Box::new(Type::Function(
                    Box::new(Type::TypeVar("a".to_string())),
                    Box::new(Type::TypeVar("a".to_string()))
                )),
                Box::new(Type::Function(
                    Box::new(Type::TypeVar("a".to_string())),
                    Box::new(Type::TypeVar("a".to_string()))
                ))
            )
        );

        assert_eq!(
            typer
                .infer(
                    application! { named!("combine"), { int_lit!(3), int_lit!(2) } },
                    env.clone()
                )
                .unwrap_err()
                .get_errors(),
            &vec![TypeError::UnificationError(
                Type::Primitive(PrimitiveType::String),
                Type::Primitive(PrimitiveType::Numeric)
            )]
        );

        assert_eq!(
            typer.infer(
                application! { named!("combine3"), { string_lit!("hello"), string_lit!("hello") } },
                env.clone()
            )
                .unwrap_err()
                .get_errors(),
            &vec![TypeError::UnificationError(
                Type::Primitive(PrimitiveType::Bool),
                Type::Primitive(PrimitiveType::String)
            )]
        );
    }
}
