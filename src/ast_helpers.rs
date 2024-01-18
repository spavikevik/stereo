use crate::ast::{ArgList, Expression, Param, ParamList, TypeParam};

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
macro_rules! param {
    ($name:literal $(: $tpe:expr)?) => {
        {
            let mut type_expr = None;
            $( type_expr = Some($tpe); )?

            match type_expr {
                None => Param::new($name.to_string()),
                Some(expr) => Param::new_typed($name.to_string(), expr)
            }
        }
    };
}

#[macro_export]
macro_rules! type_param {
    ($name:literal $(: $tpe:expr)?) => {
        {
            let mut type_expr = None;
            $( type_expr = Some($tpe); )?

            match type_expr {
                None => TypeParam::new($name.to_string()),
                Some(expr) => TypeParam::new_typed($name.to_string(), expr)
            }
        }
    };
}

#[macro_export]
macro_rules! lambda {
    ($($name:literal)?, $(types: $($type_param:expr);+,)? {$( $param:expr );*} -> $tpe:expr , body: $body:expr $(,$op_metadata:expr,)?) => {
        {
            let mut type_params = Vec::new();
            let mut params = Vec::new();
            let mut name = None;
            let mut op_metadata = None;
            $( params.push($param); )*
            $( name = Some($name.to_string()); )?
            $( op_metadata = Some($op_metadata); )?
            $( $(
                type_params.push($type_param);
            )+ )?
            Expression::Lambda(name, ParamList { type_params, params }, Box::new($tpe), Box::new($body), op_metadata)
        }
    };
}

#[macro_export]
macro_rules! infix {
    ($op:literal, $lhs:expr, $rhs:expr) => {
        Expression::infix_operation(Expression::Named($op.to_string()), $lhs, $rhs)
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
