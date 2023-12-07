use crate::ast::{ArgList, Expression, Param, ParamList};
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct PestParser;

impl PestParser {
    fn build_ast_from_expr(pair: Pair<Rule>) -> Expression {
        let mut pairs = pair.into_inner();
        let lhs = pairs.next().unwrap();
        let remainder_opt = pairs.next();

        let lhs_expr = match lhs.as_rule() {
            Rule::integer => Expression::IntegerLiteral(lhs.as_str().parse::<i64>().unwrap()),
            Rule::string => {
                Expression::StringLiteral(lhs.into_inner().next().unwrap().as_str().to_string())
            }
            Rule::boolean => Expression::BooleanLiteral(lhs.as_str().parse::<bool>().unwrap()),
            Rule::expr => PestParser::build_ast_from_expr(lhs.into_inner().next().unwrap()),
            Rule::let_expr => {
                let mut pairs = lhs.into_inner();

                let identifier = pairs.next().unwrap();
                let type_expr = pairs.next().unwrap();
                let expr = pairs.next().unwrap();

                PestParser::build_let_expr(identifier, type_expr, expr)
            }
            Rule::lambda_expr => {
                let mut pairs = lhs.into_inner();

                let identifier = pairs.next().unwrap();
                let param_list = pairs.next().unwrap();
                let type_expr = pairs.next().unwrap();
                let body_expr = pairs.next().unwrap();

                PestParser::build_lambda_expr(identifier, param_list, type_expr, body_expr)
            }
            Rule::invocation => {
                let mut pairs = lhs.into_inner();

                let named = pairs.next().unwrap();

                let call_opt = pairs.next();

                match call_opt {
                    None => Expression::Named(named.as_str().to_string()),
                    Some(call_with_args) => {
                        PestParser::build_application_expr(named, call_with_args)
                    }
                }
            }
            unknown_term => panic!("Unexpected term: {:?}", unknown_term),
        };

        match remainder_opt {
            None => lhs_expr,
            Some(remainder) => {
                let mut pairs = remainder.into_inner();

                let operator = pairs.next().unwrap().as_str().to_string();
                let rhs_expr = PestParser::build_ast_from_expr(pairs.next().unwrap());

                PestParser::combine_infix_operation_expr(lhs_expr, operator, rhs_expr)
            }
        }
    }

    fn combine_infix_operation_expr(
        lhs: Expression,
        operator: String,
        rhs: Expression,
    ) -> Expression {
        Expression::InfixOperation(operator, Box::new(lhs), Box::new(rhs))
    }

    fn build_let_expr(
        identifier: Pair<Rule>,
        type_expr: Pair<Rule>,
        expr: Pair<Rule>,
    ) -> Expression {
        match (identifier.as_rule(), type_expr.as_rule(), expr.as_rule()) {
            (Rule::identifier, Rule::expr, Rule::expr) => Expression::Let(
                identifier.as_str().to_string(),
                Box::new(PestParser::build_ast_from_expr(type_expr)),
                Box::new(PestParser::build_ast_from_expr(expr)),
            ),
            (_, _, _) => {
                panic!(
                    "Invalid let expr {:?}: {:?} = {:?}",
                    identifier.as_str(),
                    type_expr.as_str(),
                    expr.as_str()
                )
            }
        }
    }

    fn build_lambda_expr(
        identifier: Pair<Rule>,
        param_list: Pair<Rule>,
        type_expr: Pair<Rule>,
        body_expr: Pair<Rule>,
    ) -> Expression {
        match (
            identifier.as_rule(),
            param_list.as_rule(),
            type_expr.as_rule(),
            body_expr.as_rule(),
        ) {
            (Rule::identifier, Rule::param_list, Rule::expr, Rule::expr) => Expression::Lambda(
                identifier.as_str().to_string(),
                ParamList {
                    params: param_list
                        .into_inner()
                        .map(PestParser::build_param)
                        .collect(),
                },
                Box::new(PestParser::build_ast_from_expr(type_expr)),
                Box::new(PestParser::build_ast_from_expr(body_expr)),
            ),
            (_, _, _, _) => {
                panic!(
                    "Invalid lambda expr {:?}({:?}): {:?} = {:?}",
                    identifier.as_str(),
                    param_list.as_str(),
                    type_expr.as_str(),
                    body_expr.as_str()
                )
            }
        }
    }

    fn build_param(pair: Pair<Rule>) -> Param {
        match pair.as_rule() {
            Rule::param => {
                let mut inner = pair.into_inner();

                Param {
                    name: inner.next().unwrap().as_str().to_string(),
                    type_expr: PestParser::build_ast_from_expr(inner.next().unwrap()),
                }
            }
            _ => panic!("Invalid parameter {:?}", pair.as_str()),
        }
    }

    fn build_application_expr(identifier: Pair<Rule>, call: Pair<Rule>) -> Expression {
        match (identifier.as_rule(), call.as_rule()) {
            (Rule::named, Rule::call) => Expression::Application(
                Box::new(Expression::Named(identifier.as_str().to_string())),
                ArgList {
                    args: PestParser::build_arg_list(call),
                },
            ),
            (_, _) => panic!("Invalid application {:?}({:?})", identifier, call),
        }
    }

    fn build_arg_list(call: Pair<Rule>) -> Vec<Expression> {
        let args_opt = call.into_inner().next();

        match args_opt {
            None => vec![],
            Some(args) => args.into_inner().map(PestParser::build_arg).collect(),
        }
    }

    fn build_arg(pair: Pair<Rule>) -> Expression {
        match pair.as_rule() {
            Rule::arg => PestParser::build_ast_from_expr(pair.into_inner().next().unwrap()),
            _ => panic!("Invalid argument {:?}", pair.as_str()),
        }
    }
}

pub fn parse(input: &str) -> Result<Vec<Expression>, Error<Rule>> {
    let mut ast = vec![];

    let pairs = PestParser::parse(Rule::expr, input)?;

    for pair in pairs {
        ast.push(PestParser::build_ast_from_expr(pair));
    }

    Ok(ast)
}

#[cfg(test)]
mod tests {
    use crate::ast::{ArgList, Expression, Param, ParamList};
    use crate::pest_parser::{parse, PestParser};

    #[test]
    fn test_integer_literal() {
        assert_eq!(parse("42"), Ok(vec![Expression::IntegerLiteral(42)]));
    }

    #[test]
    fn test_string_literal() {
        assert_eq!(
            parse("\"42\""),
            Ok(vec![Expression::StringLiteral("42".to_string())])
        );
    }

    #[test]
    fn test_boolean_literal() {
        assert_eq!(parse("true"), Ok(vec![Expression::BooleanLiteral(true)]));
    }

    #[test]
    fn test_named_expression() {
        assert_eq!(
            parse("hello"),
            Ok(vec![Expression::Named("hello".to_string())])
        );

        assert_eq!(parse("*"), Ok(vec![Expression::Named("*".to_string())]));

        assert_eq!(parse("int"), Ok(vec![Expression::Named("int".to_string())]));

        assert_eq!(
            parse("bool"),
            Ok(vec![Expression::Named("bool".to_string())])
        );

        assert_eq!(
            parse("string"),
            Ok(vec![Expression::Named("string".to_string())])
        )
    }

    #[test]
    fn test_let_expression() {
        assert_eq!(
            parse("let three: 3 = 3"),
            Ok(vec![Expression::Let(
                "three".to_string(),
                Box::new(Expression::IntegerLiteral(3)),
                Box::new(Expression::IntegerLiteral(3))
            )])
        )
    }

    #[test]
    fn test_lambda_expression() {
        assert_eq!(
            parse("fn id(x: X) -> X { x } "),
            Ok(vec![Expression::Lambda(
                "id".to_string(),
                ParamList {
                    params: vec![Param {
                        name: "x".to_string(),
                        type_expr: Expression::Named("X".to_string())
                    }]
                },
                Box::new(Expression::Named("X".to_string())),
                Box::new(Expression::Named("x".to_string()))
            )])
        );

        assert_eq!(
            parse("fn combine(x: X, y: Y) -> Z { x } "),
            Ok(vec![Expression::Lambda(
                "combine".to_string(),
                ParamList {
                    params: vec![
                        Param {
                            name: "x".to_string(),
                            type_expr: Expression::Named("X".to_string())
                        },
                        Param {
                            name: "y".to_string(),
                            type_expr: Expression::Named("Y".to_string())
                        }
                    ]
                },
                Box::new(Expression::Named("Z".to_string())),
                Box::new(Expression::Named("x".to_string()))
            )])
        );

        assert_eq!(
            parse("fn Id(X: *) -> * { X } "),
            Ok(vec![Expression::Lambda(
                "Id".to_string(),
                ParamList {
                    params: vec![Param {
                        name: "X".to_string(),
                        type_expr: Expression::Named("*".to_string())
                    }]
                },
                Box::new(Expression::Named("*".to_string())),
                Box::new(Expression::Named("X".to_string()))
            )])
        );

        assert_eq!(
            parse("fn addTwo(x: int) -> int { x + 2 } "),
            Ok(vec![Expression::Lambda(
                "addTwo".to_string(),
                ParamList {
                    params: vec![Param {
                        name: "x".to_string(),
                        type_expr: Expression::Named("int".to_string())
                    }]
                },
                Box::new(Expression::Named("int".to_string())),
                Box::new(Expression::InfixOperation(
                    "+".to_string(),
                    Box::new(Expression::Named("x".to_string())),
                    Box::new(Expression::IntegerLiteral(2))
                ))
            )])
        )
    }

    #[test]
    fn test_application_expression() {
        assert_eq!(
            parse("x()"),
            Ok(vec![Expression::Application(
                Box::new(Expression::Named("x".to_string())),
                ArgList { args: vec![] }
            )])
        );

        assert_eq!(
            parse("x(3)"),
            Ok(vec![Expression::Application(
                Box::new(Expression::Named("x".to_string())),
                ArgList {
                    args: vec![Expression::IntegerLiteral(3)]
                }
            )])
        );

        assert_eq!(
            parse("x(a)"),
            Ok(vec![Expression::Application(
                Box::new(Expression::Named("x".to_string())),
                ArgList {
                    args: vec![Expression::Named("a".to_string())]
                }
            )])
        );

        assert_eq!(
            parse("x(a, b)"),
            Ok(vec![Expression::Application(
                Box::new(Expression::Named("x".to_string())),
                ArgList {
                    args: vec![
                        Expression::Named("a".to_string()),
                        Expression::Named("b".to_string())
                    ]
                }
            )])
        );

        assert_eq!(
            parse("x(3, 4)"),
            Ok(vec![Expression::Application(
                Box::new(Expression::Named("x".to_string())),
                ArgList {
                    args: vec![Expression::IntegerLiteral(3), Expression::IntegerLiteral(4)]
                }
            )])
        )
    }

    #[test]
    fn test_infix_expression() {
        assert_eq!(
            parse("a+b"),
            Ok(vec![Expression::InfixOperation(
                "+".to_string(),
                Box::new(Expression::Named("a".to_string())),
                Box::new(Expression::Named("b".to_string()))
            )])
        );

        assert_eq!(
            parse("a + b"),
            Ok(vec![Expression::InfixOperation(
                "+".to_string(),
                Box::new(Expression::Named("a".to_string())),
                Box::new(Expression::Named("b".to_string()))
            )])
        );

        assert_eq!(
            parse("a add b"),
            Ok(vec![Expression::InfixOperation(
                "add".to_string(),
                Box::new(Expression::Named("a".to_string())),
                Box::new(Expression::Named("b".to_string()))
            )])
        )
    }
}
