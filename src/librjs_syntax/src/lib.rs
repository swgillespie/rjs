extern crate itertools;
extern crate phf;
extern crate unicode_categories;

mod lexer;
mod char_classes;

pub use self::lexer::Lexer;
pub use self::lexer::Token;
pub use self::lexer::TokenKind;
pub use self::lexer::Span;

