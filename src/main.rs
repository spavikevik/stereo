#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]

mod ast;
mod ast_helpers;
mod inference;
mod pest_parser;
mod substitution;
mod r#type;
mod type_environment;
mod type_error;
mod typer;
fn main() {
    println!("Hello, world!");
}
