use crate::ast::{
    AffixPosition, ArgList, Associativity, Expression, OperatorMetadata, Param, ParamList,
    TypeParam,
};
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct PestParser<'a> {
    operator_metadata_mapping: HashMap<&'a str, OperatorMetadata>,
}

impl<'a> PestParser<'a> {
    fn build_ast_from_expr(&self, pair: Pair<Rule>) -> Expression {
        PestParser::pratt(self, Pairs::single(pair), 0).0
    }

    #[inline]
    fn pratt(
        &self,
        mut pairs: Pairs<'a, Rule>,
        binding_power_limit: i8,
    ) -> (Expression, Pairs<Rule>) {
        let mut inner = pairs.next().unwrap().into_inner();
        let lhs_pair = inner.next().unwrap();
        let lhs_expr = PestParser::build_primary_expr(self, lhs_pair);

        PestParser::pratt_loop(self, binding_power_limit, lhs_expr, inner)
    }

    #[inline]
    fn pratt_loop(
        &'a self,
        binding_power_limit: i8,
        lhs_expr: Expression,
        mut pairs: Pairs<'a, Rule>,
    ) -> (Expression, Pairs<'a, Rule>) {
        match pairs.peek() {
            None => (lhs_expr, pairs),
            Some(pair) => {
                let operator_metadata =
                    PestParser::get_op_metadata(self, pair.into_inner().peek().unwrap());

                let next_binding_power = operator_metadata.precedence;

                if next_binding_power > binding_power_limit {
                    let mut inner = pairs.next().unwrap().into_inner();

                    let operator = PestParser::extract_operator(
                        self,
                        inner.next().expect("Operator pair should be Some"),
                    );

                    let right_binding_power = if operator_metadata.associativity
                        == Associativity::Right
                    {
                        next_binding_power - 1
                    } else {
                        next_binding_power
                    };

                    let (rhs_expr, remainder) = PestParser::pratt(self, inner, right_binding_power);
                    let new_lhs_expr = Expression::infix_operation(operator, lhs_expr, rhs_expr);

                    PestParser::pratt_loop(self, binding_power_limit, new_lhs_expr, remainder)
                } else {
                    (lhs_expr, pairs)
                }
            }
        }
    }

    #[inline]
    fn get_op_metadata(&self, pair: Pair<Rule>) -> OperatorMetadata {
        let default_operator_metadata = OperatorMetadata {
            position: AffixPosition::In,
            associativity: Associativity::Left,
            precedence: 20,
        };

        match pair.as_rule() {
            Rule::infix_operation => {
                let pairs = &mut pair.into_inner();
                let operator = pairs.next().unwrap().as_str();
                self.operator_metadata_mapping
                    .get(operator)
                    .unwrap_or(&default_operator_metadata)
                    .clone()
            }
            _ => default_operator_metadata.clone(),
        }
    }

    fn build_primary_expr(&self, pair: Pair<Rule>) -> Expression {
        match pair.as_rule() {
            Rule::parenthesized_expr => {
                PestParser::build_ast_from_expr(self, pair.into_inner().next().unwrap())
            }
            Rule::integer => Expression::IntegerLiteral(pair.as_str().parse::<i64>().unwrap()),
            Rule::string => {
                Expression::StringLiteral(pair.into_inner().next().unwrap().as_str().to_string())
            }
            Rule::boolean => Expression::BooleanLiteral(pair.as_str().parse::<bool>().unwrap()),
            Rule::let_expr => {
                let pairs = &mut pair.into_inner();

                let identifier = pairs.next().unwrap();
                let type_expr = pairs.next().unwrap();
                let expr = pairs.next().unwrap();

                PestParser::build_let_expr(self, identifier, type_expr, expr)
            }
            Rule::lambda_expr => {
                let pairs = &mut pair.into_inner();

                let lambda_name = pairs.next().unwrap();
                let type_params = pairs.next();
                let params = pairs.next().unwrap();
                let type_expr = pairs.next().unwrap();
                let body_expr = pairs.next().unwrap();

                PestParser::build_lambda_expr(
                    self,
                    lambda_name,
                    type_params,
                    params,
                    type_expr,
                    body_expr,
                )
            }
            Rule::invocation => {
                let pairs = &mut pair.into_inner();

                let named = pairs.next().unwrap();

                let call_opt = pairs.next();

                match call_opt {
                    None => Expression::Named(named.as_str().to_string()),
                    Some(call_with_args) => {
                        PestParser::build_application_expr(self, named, call_with_args)
                    }
                }
            }
            unknown_term => panic!("Unexpected term: {:?}", unknown_term),
        }
    }

    fn extract_operator(&self, pair: Pair<Rule>) -> Expression {
        match pair.as_rule() {
            Rule::infix_operation => {
                let pairs = &mut pair.into_inner();
                let operator_pair = pairs.next().unwrap();
                match operator_pair.as_rule() {
                    Rule::infix_op => Expression::Named(operator_pair.as_str().to_string()),
                    Rule::invocation => PestParser::build_primary_expr(self, operator_pair),
                    unknown_term => panic!("Unexpected term: {:?}", unknown_term),
                }
            }
            unknown_term => panic!("Unexpected term: {:?}", unknown_term),
        }
    }

    fn build_let_expr(
        &self,
        identifier: Pair<Rule>,
        type_expr: Pair<Rule>,
        expr: Pair<Rule>,
    ) -> Expression {
        match (identifier.as_rule(), type_expr.as_rule(), expr.as_rule()) {
            (Rule::identifier, Rule::expr, Rule::expr) => Expression::Let(
                identifier.as_str().to_string(),
                Box::new(PestParser::build_ast_from_expr(self, type_expr)),
                Box::new(PestParser::build_ast_from_expr(self, expr)),
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
        &self,
        lambda_name: Pair<Rule>,
        type_params: Option<Pair<Rule>>,
        params: Pair<Rule>,
        type_expr: Pair<Rule>,
        body_expr: Pair<Rule>,
    ) -> Expression {
        match (
            lambda_name.as_rule(),
            params.as_rule(),
            type_expr.as_rule(),
            body_expr.as_rule(),
        ) {
            (Rule::lambda_name, Rule::params, Rule::expr, Rule::expr) => Expression::Lambda(
                lambda_name
                    .into_inner()
                    .next()
                    .map(|identifier| identifier.as_str().to_string()),
                ParamList::new()
                    .set_params(params.into_inner().map(|it| self.build_param(it)).collect())
                    .set_type_params(self.build_type_params(type_params)),
                Box::new(PestParser::build_ast_from_expr(self, type_expr)),
                Box::new(PestParser::build_ast_from_expr(self, body_expr)),
                None,
            ),
            (_, _, _, _) => {
                panic!(
                    "Invalid lambda expr {:?}({:?}) -> {:?} = {:?}",
                    lambda_name.as_str(),
                    params.as_str(),
                    type_expr.as_str(),
                    body_expr.as_str()
                )
            }
        }
    }

    fn build_type_params(&self, type_params_opt: Option<Pair<Rule>>) -> Vec<TypeParam> {
        match type_params_opt.map(|pair| (pair.clone(), pair.clone().as_rule())) {
            None => Vec::new(),
            Some((pair, Rule::type_params)) => pair
                .into_inner()
                .map(|it| self.build_type_param(it))
                .collect(),
            Some((pair, _)) => panic!("Invalid type params {:?}", pair),
        }
    }

    fn build_type_param(&self, pair: Pair<Rule>) -> TypeParam {
        match pair.as_rule() {
            Rule::type_param => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().to_string();

                match inner.next() {
                    None => TypeParam::new(name),
                    Some(pair) => {
                        let type_expr = PestParser::build_ast_from_expr(self, pair);
                        TypeParam::new_typed(name, type_expr)
                    }
                }
            }
            _ => panic!("Invalid type param {:?}", pair.as_str()),
        }
    }

    fn build_param(&self, pair: Pair<Rule>) -> Param {
        match pair.as_rule() {
            Rule::param => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().to_string();

                match inner.next() {
                    None => Param::new(name),
                    Some(pair) => {
                        let type_expr = PestParser::build_ast_from_expr(self, pair);
                        Param::new_typed(name, type_expr)
                    }
                }
            }
            _ => panic!("Invalid parameter {:?}", pair.as_str()),
        }
    }

    fn build_application_expr(&self, identifier: Pair<Rule>, call: Pair<Rule>) -> Expression {
        match (identifier.as_rule(), call.as_rule()) {
            (Rule::named, Rule::call) => Expression::Application(
                Box::new(Expression::Named(identifier.as_str().to_string())),
                ArgList {
                    args: PestParser::build_arg_list(self, call),
                },
            ),
            (_, _) => panic!("Invalid application {:?}({:?})", identifier, call),
        }
    }

    fn build_arg_list(&self, call: Pair<Rule>) -> Vec<Expression> {
        let args_opt = call.into_inner().next();

        match args_opt {
            None => vec![],
            Some(args) => args.into_inner().map(|it| self.build_arg(it)).collect(),
        }
    }

    fn build_arg(&self, pair: Pair<Rule>) -> Expression {
        match pair.as_rule() {
            Rule::arg => PestParser::build_ast_from_expr(&self, pair.into_inner().next().unwrap()),
            _ => panic!("Invalid argument {:?}", pair.as_str()),
        }
    }

    pub fn parse_expr(&self, input: &str) -> Result<Vec<Expression>, Error<Rule>> {
        let mut ast = vec![];

        let pairs = PestParser::parse(Rule::expr, input)?;

        for pair in pairs {
            ast.push(PestParser::build_ast_from_expr(self, pair));
        }

        Ok(ast)
    }

    pub fn new() -> Self {
        Self {
            operator_metadata_mapping: HashMap::new(),
        }
    }

    pub fn new_with_operator_metadata(
        operator_metadata_map: HashMap<&'a str, OperatorMetadata>,
    ) -> PestParser<'a> {
        Self {
            operator_metadata_mapping: operator_metadata_map,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        AffixPosition, ArgList, Associativity, Expression, OperatorMetadata, Param, ParamList,
        TypeParam,
    };
    use crate::pest_parser::PestParser;
    use std::collections::HashMap;

    #[test]
    fn test_integer_literal() {
        let parser = PestParser::new();

        assert_eq!(
            parser.parse_expr("42"),
            Ok(vec![Expression::IntegerLiteral(42)])
        );
    }

    #[test]
    fn test_string_literal() {
        let parser = PestParser::new();

        assert_eq!(
            parser.parse_expr("\"42\""),
            Ok(vec![Expression::StringLiteral("42".to_string())])
        );
    }

    #[test]
    fn test_boolean_literal() {
        let parser = PestParser::new();

        assert_eq!(
            parser.parse_expr("true"),
            Ok(vec![Expression::BooleanLiteral(true)])
        );
    }

    #[test]
    fn test_named_expression() {
        let parser = PestParser::new();

        assert_eq!(
            parser.parse_expr("hello"),
            Ok(vec![Expression::Named("hello".to_string())])
        );

        assert_eq!(
            parser.parse_expr("*"),
            Ok(vec![Expression::Named("*".to_string())])
        );

        assert_eq!(
            parser.parse_expr("int"),
            Ok(vec![Expression::Named("int".to_string())])
        );

        assert_eq!(
            parser.parse_expr("bool"),
            Ok(vec![Expression::Named("bool".to_string())])
        );

        assert_eq!(
            parser.parse_expr("string"),
            Ok(vec![Expression::Named("string".to_string())])
        )
    }

    #[test]
    fn test_let_expression() {
        let parser = PestParser::new();

        assert_eq!(
            parser.parse_expr("let three: 3 = 3"),
            Ok(vec![Expression::Let(
                "three".to_string(),
                Box::new(Expression::IntegerLiteral(3)),
                Box::new(Expression::IntegerLiteral(3))
            )])
        )
    }

    #[test]
    fn test_lambda_expression() {
        let parser = PestParser::new();

        assert_eq!(
            parser.parse_expr("fn id<X>(x: X) -> X { x } "),
            Ok(vec![Expression::Lambda(
                Some("id".to_string()),
                ParamList::new()
                    .add_type_param(TypeParam::new("X".to_string()))
                    .add_param(Param::new_typed(
                        "x".to_string(),
                        Expression::Named("X".to_string())
                    )),
                Box::new(Expression::Named("X".to_string())),
                Box::new(Expression::Named("x".to_string())),
                None
            )])
        );

        assert_eq!(
            parser.parse_expr("fn id<X: Numeric>(x: X) -> X { x } "),
            Ok(vec![Expression::Lambda(
                Some("id".to_string()),
                ParamList::new()
                    .add_type_param(TypeParam::new_typed(
                        "X".to_string(),
                        Expression::Named("Numeric".to_string())
                    ))
                    .add_param(Param::new_typed(
                        "x".to_string(),
                        Expression::Named("X".to_string())
                    )),
                Box::new(Expression::Named("X".to_string())),
                Box::new(Expression::Named("x".to_string())),
                None
            )])
        );

        assert_eq!(
            parser.parse_expr("fn (x: X) -> X { x } "),
            Ok(vec![Expression::Lambda(
                None,
                ParamList::new().add_param(
                    Param::new_typed("x".to_string(), Expression::Named("X".to_string()))
                ),
                Box::new(Expression::Named("X".to_string())),
                Box::new(Expression::Named("x".to_string())),
                None
            )])
        );

        assert_eq!(
            parser.parse_expr("fn (x) -> Int { x } "),
            Ok(vec![Expression::Lambda(
                None,
                ParamList::new().add_param(Param::new("x".to_string())),
                Box::new(Expression::Named("Int".to_string())),
                Box::new(Expression::Named("x".to_string())),
                None
            )])
        );

        assert_eq!(
            parser.parse_expr("fn combine(x: X, y: Y) -> Z { x } "),
            Ok(vec![Expression::Lambda(
                Some("combine".to_string()),
                ParamList::new()
                    .add_param(Param::new_typed(
                        "x".to_string(),
                        Expression::Named("X".to_string())
                    ))
                    .add_param(Param::new_typed(
                        "y".to_string(),
                        Expression::Named("Y".to_string())
                    )),
                Box::new(Expression::Named("Z".to_string())),
                Box::new(Expression::Named("x".to_string())),
                None
            )])
        );

        assert_eq!(
            parser.parse_expr("fn Id(X: *) -> * { X } "),
            Ok(vec![Expression::Lambda(
                Some("Id".to_string()),
                ParamList::new().add_param(
                    Param::new_typed("X".to_string(), Expression::Named("*".to_string()))
                ),
                Box::new(Expression::Named("*".to_string())),
                Box::new(Expression::Named("X".to_string())),
                None
            )])
        );

        assert_eq!(
            parser.parse_expr("fn addTwo(x: int) -> int { x + 2 } "),
            Ok(vec![Expression::Lambda(
                Some("addTwo".to_string()),
                ParamList::new().add_param(Param::new_typed(
                    "x".to_string(),
                    Expression::Named("int".to_string())
                )),
                Box::new(Expression::Named("int".to_string())),
                Box::new(Expression::infix_operation(
                    Expression::Named("+".to_string()),
                    Expression::Named("x".to_string()),
                    Expression::IntegerLiteral(2)
                )),
                None
            )])
        )
    }

    #[test]
    fn test_application_expression() {
        let parser = PestParser::new();

        assert_eq!(
            parser.parse_expr("x()"),
            Ok(vec![Expression::Application(
                Box::new(Expression::Named("x".to_string())),
                ArgList { args: vec![] }
            )])
        );

        assert_eq!(
            parser.parse_expr("x(3)"),
            Ok(vec![Expression::Application(
                Box::new(Expression::Named("x".to_string())),
                ArgList {
                    args: vec![Expression::IntegerLiteral(3)]
                }
            )])
        );

        assert_eq!(
            parser.parse_expr("x(a)"),
            Ok(vec![Expression::Application(
                Box::new(Expression::Named("x".to_string())),
                ArgList {
                    args: vec![Expression::Named("a".to_string())]
                }
            )])
        );

        assert_eq!(
            parser.parse_expr("x(a, b)"),
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
            parser.parse_expr("x(3, 4)"),
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
        let parser =
            PestParser::new_with_operator_metadata(HashMap::from_iter([
                (
                    "/",
                    OperatorMetadata {
                        position: AffixPosition::In,
                        associativity: Associativity::Right,
                        precedence: 30,
                    },
                ),
                (
                    "*",
                    OperatorMetadata {
                        position: AffixPosition::In,
                        associativity: Associativity::Right,
                        precedence: 35,
                    },
                ),
                (
                    "^",
                    OperatorMetadata {
                        position: AffixPosition::In,
                        associativity: Associativity::Right,
                        precedence: 40,
                    },
                ),
            ]));

        assert_eq!(
            parser.parse_expr("a+b"),
            Ok(vec![Expression::infix_operation(
                Expression::Named("+".to_string()),
                Expression::Named("a".to_string()),
                Expression::Named("b".to_string())
            )])
        );

        assert_eq!(
            parser.parse_expr("a + b"),
            Ok(vec![Expression::infix_operation(
                Expression::Named("+".to_string()),
                Expression::Named("a".to_string()),
                Expression::Named("b".to_string())
            )])
        );

        assert_eq!(
            parser.parse_expr("(a + b) * c"),
            Ok(vec![Expression::infix_operation(
                Expression::Named("*".to_string()),
                Expression::infix_operation(
                    Expression::Named("+".to_string()),
                    Expression::Named("a".to_string()),
                    Expression::Named("b".to_string())
                ),
                Expression::Named("c".to_string()),
            )])
        );

        assert_eq!(
            parser.parse_expr("a * (b + c)"),
            Ok(vec![Expression::infix_operation(
                Expression::Named("*".to_string()),
                Expression::Named("a".to_string()),
                Expression::infix_operation(
                    Expression::Named("+".to_string()),
                    Expression::Named("b".to_string()),
                    Expression::Named("c".to_string())
                ),
            )])
        );

        assert_eq!(
            parser.parse_expr("a ^ b ^ c"),
            Ok(vec![Expression::infix_operation(
                Expression::Named("^".to_string()),
                Expression::Named("a".to_string()),
                Expression::infix_operation(
                    Expression::Named("^".to_string()),
                    Expression::Named("b".to_string()),
                    Expression::Named("c".to_string())
                )
            )])
        );

        assert_eq!(
            parser.parse_expr("a + b + c"),
            Ok(vec![Expression::infix_operation(
                Expression::Named("+".to_string()),
                Expression::infix_operation(
                    Expression::Named("+".to_string()),
                    Expression::Named("a".to_string()),
                    Expression::Named("b".to_string())
                ),
                Expression::Named("c".to_string()),
            )])
        );

        assert_eq!(
            parser.parse_expr("a * b + c"),
            Ok(vec![Expression::infix_operation(
                Expression::Named("+".to_string()),
                Expression::infix_operation(
                    Expression::Named("*".to_string()),
                    Expression::Named("a".to_string()),
                    Expression::Named("b".to_string())
                ),
                Expression::Named("c".to_string())
            )])
        );

        assert_eq!(
            parser.parse_expr("a * b / c + d"),
            Ok(vec![Expression::infix_operation(
                Expression::Named("+".to_string()),
                Expression::infix_operation(
                    Expression::Named("/".to_string()),
                    Expression::infix_operation(
                        Expression::Named("*".to_string()),
                        Expression::Named("a".to_string()),
                        Expression::Named("b".to_string())
                    ),
                    Expression::Named("c".to_string())
                ),
                Expression::Named("d".to_string())
            )])
        );

        assert_eq!(
            parser.parse_expr("a add b"),
            Ok(vec![Expression::infix_operation(
                Expression::Named("add".to_string()),
                Expression::Named("a".to_string()),
                Expression::Named("b".to_string())
            )])
        );

        assert_eq!(
            parser.parse_expr("a add(int) b"),
            Ok(vec![Expression::infix_operation(
                Expression::Application(
                    Box::new(Expression::Named("add".to_string())),
                    ArgList {
                        args: vec![Expression::Named("int".to_string())]
                    }
                ),
                Expression::Named("a".to_string()),
                Expression::Named("b".to_string())
            )])
        );

        assert_eq!(
            parser.parse_expr("x(3 + 2 + 5, 4 + 4 * 5 - 5)"),
            Ok(vec![Expression::Application(
                Box::new(Expression::Named("x".to_string())),
                ArgList {
                    args: vec![
                        Expression::Application(
                            Box::new(Expression::Named("+".to_string())),
                            ArgList {
                                args: vec![
                                    Expression::Application(
                                        Box::new(Expression::Named("+".to_string())),
                                        ArgList {
                                            args: vec![
                                                Expression::IntegerLiteral(3),
                                                Expression::IntegerLiteral(2)
                                            ]
                                        }
                                    ),
                                    Expression::IntegerLiteral(5)
                                ]
                            }
                        ),
                        Expression::Application(
                            Box::new(Expression::Named("-".to_string())),
                            ArgList {
                                args: vec![
                                    Expression::Application(
                                        Box::new(Expression::Named("+".to_string())),
                                        ArgList {
                                            args: vec![
                                                Expression::IntegerLiteral(4),
                                                Expression::Application(
                                                    Box::new(Expression::Named("*".to_string())),
                                                    ArgList {
                                                        args: vec![
                                                            Expression::IntegerLiteral(4),
                                                            Expression::IntegerLiteral(5)
                                                        ]
                                                    }
                                                )
                                            ]
                                        }
                                    ),
                                    Expression::IntegerLiteral(5)
                                ]
                            }
                        )
                    ]
                }
            )])
        )
    }
}
