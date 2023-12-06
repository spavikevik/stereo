use crate::ast::{ArgList, Expression, Param, ParamList};
use crate::r#type::{PrimitiveType, Type};
use std::collections::HashMap;

#[derive(Clone)]
struct TypeContext {
    bindings: HashMap<String, Type>,
}

struct Typer {
    builtins: TypeContext,
}

impl Typer {
    pub fn infer(&self, expression: Expression, ctx: TypeContext) -> Type {
        match expression {
            Expression::IntegerLiteral(_) => Type::Primitive(PrimitiveType::Numeric),
            Expression::StringLiteral(_) => Type::Primitive(PrimitiveType::String),
            Expression::BooleanLiteral(_) => Type::Primitive(PrimitiveType::Bool),
            Expression::Named(name) => ctx
                .bindings
                .get(name.as_str())
                .or(self.builtins.bindings.get(name.as_str()))
                .unwrap_or(&Type::Bottom)
                .clone(),
            Expression::Let(_, _, expr) => Typer::infer(self, *expr, ctx),
            Expression::Lambda(_, ParamList { params }, _, body) => {
                let (param_types, ctx) = Typer::collect_param_types(self, params, ctx);

                let return_type = Typer::infer(self, *body, ctx.clone());

                param_types.iter().fold(return_type, |acc, tpe| {
                    Type::Function(Box::new((*tpe).clone()), Box::new(acc.clone()))
                })
            }
            Expression::InfixOperation(name, lhs, rhs) => {
                let op_fn = ctx
                    .bindings
                    .get(name.as_str())
                    .or(self.builtins.bindings.get(name.as_str()))
                    .unwrap_or(&Type::Bottom)
                    .clone();
                let lhs_type = Typer::infer(&self, *lhs, ctx.clone());
                let rhs_type = Typer::infer(&self, *rhs, ctx.clone());

                let first_reduction = Typer::reduce_type(op_fn, lhs_type);

                Typer::reduce_type(first_reduction, rhs_type)
            }
            // TODO: Unify type reduction for Applications and Infix operations
            Expression::Application(applicable, ArgList { args }) => {
                let fn_type = Typer::infer(self, *applicable, ctx.clone());
                let arg_types: Vec<Type> = args
                    .iter()
                    .map(|arg| Typer::infer(self, arg.clone(), ctx.clone()))
                    .collect();

                arg_types.iter().fold(fn_type, |return_type, arg| {
                    Typer::reduce_type(return_type, arg.clone())
                })
            }
        }
    }

    #[inline]
    fn lift_type_expr(&self, expression: Expression, ctx: TypeContext) -> Type {
        match expression {
            Expression::IntegerLiteral(_) => Type::Bottom,
            Expression::StringLiteral(_) => Type::Bottom,
            Expression::BooleanLiteral(_) => Type::Bottom,
            Expression::Named(name) => {
                Typer::get_builtin_type(self, name.as_str()).unwrap_or(Type::TypeVar(name))
            }
            Expression::Let(_, _, _) => Type::Bottom,
            Expression::Lambda(_, _, _, _) => Type::Bottom,
            Expression::InfixOperation(_, _, _) => Type::Bottom,
            Expression::Application(_, _) => Type::Bottom,
        }
    }

    #[inline]
    fn reduce_type(tpe: Type, arg: Type) -> Type {
        match tpe {
            Type::Primitive(_) => Type::Bottom,
            Type::TypeVar(_) => Type::Bottom,
            Type::Bottom => Type::Bottom,
            Type::Function(param, return_tpe) => {
                if *param == arg {
                    *return_tpe
                } else {
                    Type::Bottom
                }
            }
            Type::ForAll(_, _) => Type::Bottom,
        }
    }

    #[inline]
    fn get_builtin_type(&self, name: &str) -> Option<Type> {
        self.builtins.bindings.get(name).map(|tpe| tpe.clone())
    }

    #[inline]
    fn collect_param_types(
        &self,
        params_list: Vec<Param>,
        ctx: TypeContext,
    ) -> (Vec<Type>, TypeContext) {
        let mut inner_ctx = ctx.clone();

        let types = params_list
            .iter()
            .map(|param| {
                let param_clone = param.clone();
                let param_type = Typer::lift_type_expr(self, param_clone.type_expr, ctx.clone());
                inner_ctx
                    .bindings
                    .insert(param_clone.name, param_type.clone());
                param_type
            })
            .collect();

        (types, inner_ctx)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{ArgList, Expression, Param, ParamList};
    use crate::r#type::{PrimitiveType, Type};
    use crate::typer::{TypeContext, Typer};
    use std::collections::HashMap;

    #[test]
    fn test_literal() {
        let ctx =
            TypeContext {
                bindings: HashMap::new(),
            };

        let typer = Typer {
            builtins: TypeContext {
                bindings: HashMap::new(),
            },
        };

        assert_eq!(
            typer.infer(Expression::IntegerLiteral(1), ctx.clone()),
            Type::Primitive(PrimitiveType::Numeric)
        );

        assert_eq!(
            typer.infer(Expression::StringLiteral("Hello".to_string()), ctx.clone()),
            Type::Primitive(PrimitiveType::String)
        );

        assert_eq!(
            typer.infer(Expression::BooleanLiteral(true), ctx.clone()),
            Type::Primitive(PrimitiveType::Bool)
        );
    }

    #[test]
    fn test_named() {
        let ctx = TypeContext {
            bindings: HashMap::from_iter([
                ("x".to_string(), Type::Primitive(PrimitiveType::Int)),
                ("X".to_string(), Type::Primitive(PrimitiveType::Star)),
            ]),
        };

        let mut typer = Typer {
            builtins: TypeContext {
                bindings: HashMap::new(),
            },
        };

        assert_eq!(
            typer.infer(Expression::Named("x".to_string()), ctx.clone()),
            Type::Primitive(PrimitiveType::Int)
        );

        assert_eq!(
            typer.infer(Expression::Named("X".to_string()), ctx.clone()),
            Type::Primitive(PrimitiveType::Star)
        )
    }

    #[test]
    fn test_let() {
        let ctx =
            TypeContext {
                bindings: HashMap::new(),
            };

        let mut typer = Typer {
            builtins: TypeContext {
                bindings: HashMap::new(),
            },
        };

        assert_eq!(
            typer.infer(
                Expression::Let(
                    "x".to_string(),
                    Box::new(Expression::Named("int".to_string())),
                    Box::new(Expression::IntegerLiteral(3))
                ),
                ctx.clone()
            ),
            Type::Primitive(PrimitiveType::Numeric)
        );
    }

    #[test]
    fn test_infix_operation() {
        let ctx =
            TypeContext {
                bindings: HashMap::new(),
            };

        let typer = Typer {
            builtins: TypeContext {
                bindings: HashMap::from_iter([
                    ("int".to_string(), Type::Primitive(PrimitiveType::Int)),
                    (
                        "+".to_string(),
                        Type::Function(
                            Box::new(Type::Primitive(PrimitiveType::Numeric)),
                            Box::new(Type::Function(
                                Box::new(Type::Primitive(PrimitiveType::Numeric)),
                                Box::new(Type::Primitive(PrimitiveType::Numeric)),
                            )),
                        ),
                    ),
                ]),
            },
        };
        assert_eq!(
            typer.infer(
                Expression::InfixOperation(
                    "+".to_string(),
                    Box::new(Expression::IntegerLiteral(3)),
                    Box::new(Expression::IntegerLiteral(4))
                ),
                ctx
            ),
            Type::Primitive(PrimitiveType::Numeric)
        )
    }

    #[test]
    fn test_lambda() {
        let ctx =
            TypeContext {
                bindings: HashMap::new(),
            };

        let typer = Typer {
            builtins: TypeContext {
                bindings: HashMap::from_iter([
                    ("int".to_string(), Type::Primitive(PrimitiveType::Int)),
                    (
                        "==".to_string(),
                        Type::Function(
                            Box::new(Type::Primitive(PrimitiveType::Int)),
                            Box::new(Type::Function(
                                Box::new(Type::Primitive(PrimitiveType::Int)),
                                Box::new(Type::Primitive(PrimitiveType::Bool)),
                            )),
                        ),
                    ),
                ]),
            },
        };

        assert_eq!(
            typer.infer(
                Expression::Lambda(
                    "identity".to_string(),
                    ParamList {
                        params: vec![Param {
                            name: "x".to_string(),
                            type_expr: Expression::Named("int".to_string())
                        }]
                    },
                    Box::new(Expression::Named("int".to_string())),
                    Box::new(Expression::Named("x".to_string()))
                ),
                ctx.clone()
            ),
            Type::Function(
                Box::new(Type::Primitive(PrimitiveType::Int)),
                Box::new(Type::Primitive(PrimitiveType::Int))
            )
        );

        assert_eq!(
            typer.infer(
                Expression::Lambda(
                    "const".to_string(),
                    ParamList {
                        params: vec![
                            Param {
                                name: "x".to_string(),
                                type_expr: Expression::Named("int".to_string())
                            },
                            Param {
                                name: "y".to_string(),
                                type_expr: Expression::Named("int".to_string())
                            }
                        ]
                    },
                    Box::new(Expression::Named("int".to_string())),
                    Box::new(Expression::Named("x".to_string()))
                ),
                ctx.clone()
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
            typer.infer(
                Expression::Lambda(
                    "isEqual".to_string(),
                    ParamList {
                        params: vec![
                            Param {
                                name: "x".to_string(),
                                type_expr: Expression::Named("int".to_string())
                            },
                            Param {
                                name: "y".to_string(),
                                type_expr: Expression::Named("int".to_string())
                            }
                        ]
                    },
                    Box::new(Expression::Named("bool".to_string())),
                    Box::new(Expression::InfixOperation(
                        "==".to_string(),
                        Box::new(Expression::Named("x".to_string())),
                        Box::new(Expression::Named("y".to_string()))
                    ))
                ),
                ctx.clone()
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
        let ctx = TypeContext {
            bindings: HashMap::from_iter([(
                "combine".to_string(),
                Type::Function(
                    Box::new(Type::Primitive(PrimitiveType::String)),
                    Box::new(Type::Function(
                        Box::new(Type::Primitive(PrimitiveType::String)),
                        Box::new(Type::Primitive(PrimitiveType::String)),
                    )),
                ),
            )]),
        };

        let typer =
            Typer {
                builtins: TypeContext {
                    bindings: HashMap::from_iter([(
                        "string".to_string(),
                        Type::Primitive(PrimitiveType::String),
                    )]),
                },
            };

        assert_eq!(
            typer.infer(
                Expression::Application(
                    Box::new(Expression::Named("combine".to_string())),
                    ArgList {
                        args: vec![
                            Expression::StringLiteral("Hello, ".to_string()),
                            Expression::StringLiteral("World!".to_string())
                        ]
                    }
                ),
                ctx
            ),
            Type::Primitive(PrimitiveType::String)
        )
    }
}
