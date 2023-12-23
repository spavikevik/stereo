use crate::ast::{ArgList, Expression, Param, ParamList};

#[macro_export]
macro_rules! int_lit {
    ($x:literal) => {
        Expression::IntegerLiteral($x)
    };
}

#[macro_export]
macro_rules! string_lit {
    ($x:literal) => {
        Expression::StringLiteral($x.to_string())
    };
}

#[macro_export]
macro_rules! bool_lit {
    ($x:literal) => {
        Expression::BooleanLiteral($x)
    };
}

#[macro_export]
macro_rules! named {
    ($name:literal) => {
        Expression::Named($name.to_string())
    };
}

#[macro_export]
macro_rules! let_expr {
    ($name:literal : $tpe:expr => $val:expr) => {
        Expression::Let($name.to_string(), Box::new($tpe), Box::new($val))
    };
}

#[macro_export]
macro_rules! p {
    ($name:literal : $tpe:expr) => {
        Param {
            name: $name.to_string(),
            type_expr: $tpe,
        }
    };
}

#[macro_export]
macro_rules! lambda {
    ($name:literal, {$( $param:expr );*} -> $tpe:expr , body: $body:expr) => {
        {
            let mut params = Vec::new();
            $(
                params.push($param);
            )*
            Expression::Lambda($name.to_string(), ParamList { params }, Box::new($tpe), Box::new($body))
        }
    };
}

#[macro_export]
macro_rules! infix {
    ($op:literal, $lhs:expr, $rhs:expr) => {
        Expression::InfixOperation($op.to_string(), Box::new($lhs), Box::new($rhs))
    };
}

#[macro_export]
macro_rules! application {
    ($function:expr, {$( $arg:expr ),*}) => {
        {
            let mut args = Vec::new();
            $(
                args.push($arg);
            )*
            Expression::Application(Box::new($function), ArgList { args })
        }
    };
}