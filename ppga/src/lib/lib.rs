//! # PPGA Script
//! PPGA Script is a scripting language that transpiles to Lua. It
//! provides a more familiar C-style syntax and syntactic sugar,
//! designed to reduce the amount of boilerplate when writing scripting commands in lua.
//!
//! ## Usage
//! ```rust,ignore
//! extern crate ppga;
//!
//! fn main() {
//!     let source = load_file(...);
//!     let result = ppga::ppga_to_lua(&source, ppga::PPGAConfig::default());
//!     match result {
//!         Ok(lua) => println!("{}", lua),
//!         Err(ex) => eprintln!("{}", ex.report_to_string()),
//!     }
//! }
//! ```
#![feature(box_patterns)]
extern crate logos;

#[macro_use]
mod macros;
pub mod codegen;
pub mod config;
pub mod errors;
pub mod frontend;

use errors::ErrCtx;
use frontend::{lexer, Parser};

pub use codegen::code_builder::DEFAULT_INDENT_SIZE;
pub use codegen::emit_lua;
pub use config::PPGAConfig;

/// Parses the given PPGA source to an AST. The return values is either the AST or an [`ErrCtx`] with the errors.
///
/// ```rust
/// # extern crate ppga;
/// # use ppga::{parse, emit_lua, PPGAConfig};
/// assert_eq!(
///     emit_lua(&parse("let a = 2;", PPGAConfig::default().disable_std()).unwrap()),
///     "local a = 2"
/// );
/// ```
///
/// [`ErrCtx`]: crate::errors::ErrCtx
pub fn parse(source: &str, config: PPGAConfig) -> Result<frontend::ast::AST<'_>, ErrCtx<'_>> {
    Parser::with_config(config, lexer(source.trim_end())).parse()
}

/// Transpiles the given PPGA source to Lua. The returned value is either the resulting Lua code or an [`ErrCtx`] with the errors.
///
/// ```rust
/// # extern crate ppga;
/// # use ppga::{ppga_to_lua, PPGAConfig};
/// assert_eq!(
///     ppga_to_lua("let a = 2;", PPGAConfig::default().disable_std()).unwrap(),
///     "local a = 2"
/// );
/// ```
///
///[`ErrCtx`]: crate::errors::ErrCtx
pub fn ppga_to_lua(source: &str, config: PPGAConfig) -> Result<String, ErrCtx<'_>> {
    parse(source, config).map(|ast| emit_lua(&ast))
}
