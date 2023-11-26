const RESERVED_WORDS: &[&str] = &["fn", "let", "true", "false", "int", "bool", "string", "*"];

mod common_parsers {
    use nom::branch::alt;
    use nom::bytes::complete::take_until1;
    use nom::character::complete::{alphanumeric1, char, digit1, space1};
    use nom::combinator::{map, map_res};
    use nom::sequence::{delimited, preceded, terminated};
    use nom::{bytes::complete::tag, IResult, Parser};

    use crate::parsers::RESERVED_WORDS;

    pub(in crate::parsers) fn identifier(input: &str) -> IResult<&str, String> {
        let (input, id) = map(alphanumeric1, |s: &str| s.to_string())(input)?;

        if RESERVED_WORDS.contains(&id.as_str()) {
            Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
        } else {
            Ok((input, id))
        }
    }

    pub(in crate::parsers) fn boolean(input: &str) -> IResult<&str, bool> {
        alt((tag("true").map(|_| true), tag("false").map(|_| false)))(input)
    }

    pub(in crate::parsers) fn integer(input: &str) -> IResult<&str, i64> {
        map_res(digit1, str::parse)(input)
    }

    pub(in crate::parsers) fn assignment_eq(input: &str) -> IResult<&str, ()> {
        map(terminated(preceded(space1, char('=')), space1), |_| ())(input)
    }

    pub(in crate::parsers) fn lift_braces(input: &str) -> IResult<&str, &str> {
        delimited(tag("{"), take_until1("}"), tag("}"))(input)
    }
}

mod expression_parsers {
    use nom::branch::alt;
    use nom::bytes::complete::{is_not, take_till};
    use nom::character::complete::{char, space0, space1};
    use nom::combinator::{all_consuming, map, map_parser};
    use nom::multi::separated_list0;
    use nom::sequence::{preceded, terminated};
    use nom::{bytes::complete::tag, IResult};

    use crate::ast::{Expression, Param, ParamList, TypeSymEnum};
    use crate::parsers::common_parsers;
    use crate::parsers::common_parsers::lift_braces;

    fn identifier_and_type_expression(input: &str) -> IResult<&str, (String, Expression)> {
        let (input, name) = (common_parsers::identifier)(input)?;

        let (input, _) = tag(":")(input)?;
        let (input, _) = space0(input)?;

        let (input, type_expr) = expression(input)?;

        Ok((input, (name, type_expr)))
    }

    pub fn integer_literal_expression(input: &str) -> IResult<&str, Expression> {
        map(common_parsers::integer, |i| Expression::IntegerLiteral(i))(input)
    }
    pub fn string_literal_expression(input: &str) -> IResult<&str, Expression> {
        map(
            terminated(preceded(tag("\""), take_till(|c| c == '"')), tag("\"")),
            |s: &str| Expression::StringLiteral(s.to_string()),
        )(input)
    }
    pub fn boolean_literal_expression(input: &str) -> IResult<&str, Expression> {
        map(common_parsers::boolean, |b| Expression::BooleanLiteral(b))(input)
    }
    pub fn named_expression(input: &str) -> IResult<&str, Expression> {
        map(common_parsers::identifier, |s| Expression::Named(s))(input)
    }
    pub fn let_expression(input: &str) -> IResult<&str, Expression> {
        let (input, _) = tag("let")(input)?;
        let (input, _) = space1(input)?;

        let (input, (name, type_expr)) = identifier_and_type_expression(input)?;

        let (input, _) = (common_parsers::assignment_eq)(input)?;

        let (input, expression) = expression(input)?;
        Ok((
            input,
            Expression::Let(name, Box::from(type_expr), Box::from(expression)),
        ))
    }
    pub fn infix_operation_expression(input: &str) -> IResult<&str, Expression> {
        let (input, lhs) = map_parser(is_not(" \t\r\n"), expression)(input)?;
        let (input, _) = space1(input)?;

        let (input, op) = (take_till(|c| c == ' '))(input)?;
        let parsed_op = op.to_string();

        let (input, _) = space1(input)?;

        let (input, rhs) = map_parser(take_till(|c| c == ' '), expression)(input)?;

        Ok((
            input,
            Expression::InfixOperation(parsed_op, Box::from(lhs), Box::from(rhs)),
        ))
    }
    pub fn lambda_expression(input: &str) -> IResult<&str, Expression> {
        let (input, _) = tag("fn")(input)?;
        let (input, _) = space1(input)?;

        let (input, name) = common_parsers::identifier(input)?;

        let (input, _) = char('(')(input)?;
        let (input, params) = separated_list0(
            tag(", "),
            map(identifier_and_type_expression, |(name, type_expr)| Param {
                name,
                type_expr,
            }),
        )(input)?;
        let (input, _) = char(')')(input)?;
        let (input, _) = space1(input)?;

        let (input, _) = tag("->")(input)?;
        let (input, _) = space1(input)?;
        let (input, type_expr) = expression(input)?;

        let (input, _) = space1(input)?;

        let (input, body) = map_parser(lift_braces, expression)(input)?;

        Ok((
            input,
            Expression::Lambda(
                name,
                ParamList { params },
                Box::from(type_expr),
                Box::from(body),
            ),
        ))
    }
    pub fn type_sym_expression(input: &str) -> IResult<&str, Expression> {
        let int_sym = map(tag("int"), |_| TypeSymEnum::Int);
        let bool_sym = map(tag("bool"), |_| TypeSymEnum::Boolean);
        let string_sym = map(tag("string"), |_| TypeSymEnum::String);
        let star_sym = map(tag("*"), |_| TypeSymEnum::Star);

        let (input, sym) = alt((int_sym, bool_sym, string_sym, star_sym))(input)?;
        Ok((input, Expression::TypeSym(sym)))
    }
    pub fn expression(input: &str) -> IResult<&str, Expression> {
        let (input, expr) = alt((
            integer_literal_expression,
            string_literal_expression,
            boolean_literal_expression,
            // The call to all_consuming here ensures that named_expression! doesn't backtrack
            all_consuming(named_expression),
            type_sym_expression,
            let_expression,
            infix_operation_expression,
            lambda_expression,
        ))(input.trim())?;
        Ok((input, expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Param, ParamList, TypeSymEnum};
    use crate::parsers::expression_parsers::*;

    #[test]
    fn test_integer_literal_expression() {
        assert_eq!(
            integer_literal_expression("123"),
            Ok(("", Expression::IntegerLiteral(123)))
        );
    }

    #[test]
    fn test_string_literal_expression() {
        assert_eq!(
            string_literal_expression("\"abc\""),
            Ok(("", Expression::StringLiteral("abc".to_string())))
        );
    }

    #[test]
    fn test_boolean_literal_expression() {
        assert_eq!(
            boolean_literal_expression("true"),
            Ok(("", Expression::BooleanLiteral(true)))
        );
        assert_eq!(
            boolean_literal_expression("false"),
            Ok(("", Expression::BooleanLiteral(false)))
        );
    }

    #[test]
    fn test_named_expression() {
        assert_eq!(
            named_expression("abc"),
            Ok(("", Expression::Named("abc".to_string())))
        );
    }

    #[test]
    fn test_type_sym_expression() {
        assert_eq!(
            type_sym_expression("int"),
            Ok(("", Expression::TypeSym(TypeSymEnum::Int)))
        );

        assert_eq!(
            type_sym_expression("bool"),
            Ok(("", Expression::TypeSym(TypeSymEnum::Boolean)))
        );

        assert_eq!(
            type_sym_expression("string"),
            Ok(("", Expression::TypeSym(TypeSymEnum::String)))
        );

        assert_eq!(
            type_sym_expression("*"),
            Ok(("", Expression::TypeSym(TypeSymEnum::Star)))
        );
    }

    #[test]
    fn test_let_expression() {
        assert_eq!(
            let_expression("let abc: int = 123"),
            Ok((
                "",
                Expression::Let(
                    "abc".to_string(),
                    Box::from(Expression::TypeSym(TypeSymEnum::Int)),
                    Box::from(Expression::IntegerLiteral(123))
                )
            ))
        );

        assert_eq!(
            let_expression("let abc: int = a"),
            Ok((
                "",
                Expression::Let(
                    "abc".to_string(),
                    Box::from(Expression::TypeSym(TypeSymEnum::Int)),
                    Box::from(Expression::Named("a".to_string()))
                )
            ))
        )
    }

    #[test]
    fn test_infix_operation_expression() {
        assert_eq!(
            infix_operation_expression("1 + 2"),
            Ok((
                "",
                Expression::InfixOperation(
                    "+".to_string(),
                    Box::from(Expression::IntegerLiteral(1)),
                    Box::from(Expression::IntegerLiteral(2))
                )
            ))
        );

        assert_eq!(
            infix_operation_expression("1 add 2"),
            Ok((
                "",
                Expression::InfixOperation(
                    "add".to_string(),
                    Box::from(Expression::IntegerLiteral(1)),
                    Box::from(Expression::IntegerLiteral(2))
                )
            ))
        );

        assert_eq!(
            infix_operation_expression("a + b"),
            Ok((
                "",
                Expression::InfixOperation(
                    "+".to_string(),
                    Box::from(Expression::Named("a".to_string())),
                    Box::from(Expression::Named("b".to_string()))
                )
            ))
        )
    }

    #[test]
    fn test_lambda_expression() {
        assert_eq!(
            lambda_expression("fn abc(x: int, y: int) -> int { x + y }"),
            Ok((
                "",
                Expression::Lambda(
                    "abc".to_string(),
                    ParamList {
                        params: vec![
                            Param {
                                name: "x".to_string(),
                                type_expr: Expression::TypeSym(TypeSymEnum::Int)
                            },
                            Param {
                                name: "y".to_string(),
                                type_expr: Expression::TypeSym(TypeSymEnum::Int)
                            }
                        ]
                    },
                    Box::from(Expression::TypeSym(TypeSymEnum::Int)),
                    Box::from(Expression::InfixOperation(
                        "+".to_string(),
                        Box::from(Expression::Named("x".to_string())),
                        Box::from(Expression::Named("y".to_string()))
                    ))
                )
            ))
        );

        assert_eq!(
            lambda_expression("fn X(Y: *) -> * { Y }"),
            Ok((
                "",
                Expression::Lambda(
                    "X".to_string(),
                    ParamList {
                        params: vec![Param {
                            name: "Y".to_string(),
                            type_expr: Expression::TypeSym(TypeSymEnum::Star)
                        }]
                    },
                    Box::from(Expression::TypeSym(TypeSymEnum::Star)),
                    Box::from(Expression::Named("Y".to_string()))
                )
            ))
        )
    }
}
