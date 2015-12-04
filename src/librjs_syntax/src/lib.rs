#![deny(warnings)]

extern crate itertools;
extern crate phf;
extern crate unicode_categories;
extern crate num;

mod lexer;
mod char_classes;
mod parser;
mod visitor;
pub mod ast;


pub use self::lexer::Lexer;
pub use self::lexer::Token;
pub use self::lexer::TokenKind;
pub use self::lexer::Span;
pub use self::parser::Parser;
pub use self::parser::ParseError;
pub use self::parser::ParseResult;
pub use self::visitor::Visitor;
