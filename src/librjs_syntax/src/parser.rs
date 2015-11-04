use ast::*;
use lexer::{Lexer, Token, TokenKind, Span, Position};

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
    pub more_input: bool,
}

macro_rules! span_err {
    ($span:expr, $($fmt:tt)*) => {
        return Err(ParseError {
            span: $span,
            message: format!($($fmt)*),
            more_input: false
        })
    }
}

macro_rules! span_unexpected_eof {
    () => {
        return Err(ParseError {
            span: Span {
                start: Position {
                    col: 0,
                    line: 1
                },
                stop: Position {
                    col: 0,
                    line: 1
                }
            },
            message: "unexpected EOF".to_string(),
            more_input: true
        })
    }
}

macro_rules! next_token_is {
    ($lexer:expr, $div:expr, $($token:pat),+) => {
        match $lexer.peek($div) {
            $(
                Some(&Token { kind: $token, .. }) => true,
             )*
             _ => false
        }
    }
}

macro_rules! match_peek {
    ($this:ident, $div:expr, $($token:pat => $arm:expr),+) => {
        match $this.peek($div) {
            $(
                Some(&Token { kind: $token, .. }) => $arm,
             )*
             None => span_unexpected_eof!()
        }
    }
}

pub struct Parser<I: Iterator<Item=char>> {
    lexer: Lexer<I>,
    strict_mode: bool,
    peek_stack: Vec<Token>,
    in_directive_prologue: bool,
    last_span: Span
}

impl<I: Iterator<Item=char>> Parser<I> {
    pub fn new(lexer: Lexer<I>) -> Parser<I> {
        Parser {
            lexer: lexer,
            strict_mode: false,
            peek_stack: vec![],
            in_directive_prologue: false,
            last_span: Default::default()
        }
    }

    pub fn parse_expression(&mut self) -> ParseResult<SpannedExpression> {
        unimplemented!()
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut statements = vec![];
        let mut prolog = vec![];
        self.in_directive_prologue = true;
        loop {
            if self.peek(true).is_none() {
                break;
            }

            let statement = try!(self.statement());

            // section 14.1 - "Directive Prologues and the Use Strict Directive"
            // the "prologue" of a program consists of the longest
            // sequence of ExpressionStatements. These can have
            // implementation-defined meanings, but in the case of
            // the standard, the only required one is "use strict",
            // which puts the interpreter into strict mode.
            if let Statement::Expression(_) = statement.data {
                if self.in_directive_prologue {
                    prolog.push(statement);
                } else {
                    statements.push(statement);
                }
            } else {
                if self.in_directive_prologue {
                    self.in_directive_prologue = false;
                }

                statements.push(statement);
            }
        }

        Ok(Program {
            directive_prologue: prolog,
            statements: statements
        })
    }

    pub fn parse_statement(&mut self) -> ParseResult<SpannedStatement> {
        // SPEC_NOTE: the statement production is not allowed to have
        // prologue directives (namely 'use strict'),
        // and v8 doesn't allow it either.
        self.statement()
    }

    fn next_token(&mut self, leading_div: bool) -> Option<Token> {
        let tok = self.peek_stack.pop()
            .or_else(|| if leading_div {
                self.lexer.next_token_leading_div()
            } else {
                self.lexer.next_token()
            });

        println!("next token yields: {:?}", tok);
        tok
    }

    fn insert_token(&mut self, token: Token) {
        self.peek_stack.push(token);
    }

    fn peek(&mut self, leading_div: bool) -> Option<&Token> {
        if self.peek_stack.is_empty() {
            let token = if leading_div {
                self.lexer.next_token_leading_div()
            } else {
                self.lexer.next_token()
            };

            match token {
                Some(t) => self.peek_stack.push(t),
                None => return None
            }
        }

        let peeked = self.peek_stack.first();
        println!("peeking: {:?}", peeked);
        println!("peek stack is now: {:?}", self.peek_stack);
        peeked
    }

    fn eat_semicolon(&mut self) -> ParseResult<Span> {
        let mut inserted_semicolon_already = false;
        loop {
            match self.next_token(false) {
                Some(Token { kind: TokenKind::Semicolon, span, .. }) => {
                    self.last_span = span;
                    return Ok(span)
                },
                Some(Token { kind: ref other_kind, span, ..}) if inserted_semicolon_already => {
                    span_err!(span, "unexpected token: \
                                     expected `{:?}`, got `{:?}`", TokenKind::Semicolon, other_kind)
                },
                Some(token) => {
                    // first time around, try to stick a semicolon in there.
                    // if it's legal to do so, this will stick a semicolon onto
                    // the input stream.
                    // If it's not legal to do so, nothing will change and we'll go to the
                    // above match arm.
                    //
                    // The idea is that if we see someting like { 1 2 },
                    // we will attempt to eat a semicolon after the expression
                    // statement "1", fail (no newline after 1), see "2" instead of a
                    // semicolon and report an error.
                    self.insert_token(token.clone());
                    self.try_insert_semicolon(&token);
                    inserted_semicolon_already = true;
                },
                _ => {
                    self.insert_semicolon();
                    inserted_semicolon_already = true;
                }
            }
        }
    }

    fn eat_token(&mut self, kind: TokenKind, leading_div: bool) -> ParseResult<Span> {
        match self.next_token(leading_div) {
            Some(Token { kind: ref other_kind, span, ..}) if other_kind == &kind => {
                self.last_span = span;
                return Ok(span)
            }
            Some(Token { kind: ref other_kind, span, ..}) => {
                span_err!(span, "unexpected token: \
                                 expected `{:?}`, got `{:?}`", kind, other_kind)
            },
            _ => span_unexpected_eof!()
        }
    }

    fn identifier(&mut self) -> ParseResult<Identifier> {
        match self.next_token(false) {
            Some(Token { kind: TokenKind::Identifier(ref name), span, .. }) => {
                self.last_span = span;
                Ok(Spanned {
                    span: span,
                    data: name.clone()
                })
            },
            // some words are reserved only in strict mode and can otherwise be used as identifiers
            Some(Token { kind: TokenKind::FutureReservedWordStrict(ref name), span, .. }) if !self.strict_mode => {
                self.last_span = span;
                Ok(Spanned {
                    span: span,
                    data: name.to_string()
                })
            },
            Some(Token { kind: ref other_kind, span, ..}) => {
                span_err!(span, "unexpected token: \
                                 expected an identifier, got `{:?}`", other_kind)
            },
            _ => span_unexpected_eof!()
        }
    }

    fn next_token_preceded_by_newline(&mut self) -> bool {
        match self.peek(false) {
            Some(tok) => tok.preceded_by_newline,
            _ => false
        }
    }

    fn try_insert_semicolon(&mut self, offending_token: &Token) {
        // section 7.9.1 automatic semicolon insertion
        // only rule 1 is handled here. Rules 2 and 3 are handled
        // elsewhere.
        println!("trying to insert a semicolon before offending token {:?}", offending_token);
        if offending_token.preceded_by_newline || offending_token.kind == TokenKind::RightBrace {
            // rule 1 - if the token is preceded by a newline or
            // it's a right brace, insert a semicolon.
            self.insert_semicolon();
        }
    }

    fn insert_semicolon(&mut self) {
        println!("inserting semicolon");
        let last_span = self.last_span;
        self.insert_token(Token {
            span: last_span,
            kind: TokenKind::Semicolon,
            preceded_by_newline: true
        });
        println!("  peek stack now {:?}", self.peek_stack);
    }

    // begin grammar productions
    fn statement(&mut self) -> ParseResult<SpannedStatement> {
        match_peek! {
            self, true,
            TokenKind::LeftBrace => self.block(),
            TokenKind::Var => self.variable_statement(),
            TokenKind::Semicolon => self.empty_statement(),
            TokenKind::If => self.if_statement(),
            TokenKind::Do => self.do_while_statement(),
            TokenKind::While => self.while_statement(),
            TokenKind::For => self.for_statement(),
            TokenKind::Continue => self.continue_statement(),
            TokenKind::Break => self.break_statement(),
            TokenKind::Return => self.return_statement(),
            TokenKind::With => self.with_statement(),
            TokenKind::Switch => self.switch_statement(),
            // SPEC_NOTE: unfortunately the ECMA grammar is not LL(1)
            // because of the label rule. A statement that begins
            // with an identifier can be either a variety of expressions
            // or it can be a label, depending on what the token after
            // it is.
            TokenKind::Identifier(_) => self.identifier_or_label(),
            TokenKind::Throw => self.throw_statement(),
            TokenKind::Try => self.try_statement(),
            TokenKind::Debugger => self.debugger_statement(),
            TokenKind::Function => self.function_definition(),
            _ => self.expression_statement()
        }
    }

    fn block(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::LeftBrace, true));
        let mut statements = vec![];
        loop {
            if next_token_is!(self, false, TokenKind::RightBrace) {
                break;
            }

            statements.push(try!(self.statement()));
        }

        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBrace, false));
        Ok(Spanned::new(Span::new(start, stop), Statement::Block(statements)))
    }

    fn variable_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::Var, true));
        let mut declarations = vec![];
        // first declaration is not optional.
        declarations.push(try!(self.variable_declaration()));
        // all other ones are.
        loop {
            if !next_token_is!(self, false, TokenKind::Comma) {
                break;
            }

            let _ = try!(self.eat_token(TokenKind::Comma, false));
            declarations.push(try!(self.variable_declaration()))
        }

        let Span { stop, .. } = try!(self.eat_semicolon());
        Ok(Spanned::new(Span::new(start, stop), Statement::Declaration(Declaration::Variable(declarations))))
    }

    fn variable_declaration(&mut self) -> ParseResult<VariableDeclarator> {
        let ident = try!(self.identifier());
        let initialiser = if next_token_is!(self, false, TokenKind::Equal) {
            let _ = try!(self.eat_token(TokenKind::Equal, false));
            Some(try!(self.assignment_expression()))
        } else {
            None
        };

        Ok(VariableDeclarator {
            id: Spanned::new(ident.span, Pattern::Identifier(ident)),
            initial_value: initialiser
        })
    }

    fn empty_statement(&mut self) -> ParseResult<SpannedStatement> {
        let span = try!(self.eat_semicolon());
        Ok(Spanned::new(span, Statement::Empty))
    }

    fn if_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::If, true));
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let cond = try!(self.expression());
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let true_stmt = try!(self.statement());
        let false_stmt = if next_token_is!(self, false, TokenKind::Else) {
            let _ = try!(self.eat_token(TokenKind::Else, false));
            Some(Box::new(try!(self.statement())))
        } else {
            None
        };

        let stop = false_stmt
            .as_ref()
            .map(|x| x.span.stop)
            .unwrap_or(true_stmt.span.stop);
        Ok(Spanned::new(Span::new(start, stop), Statement::If(cond, Box::new(true_stmt), false_stmt)))
    }

    fn do_while_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::Do, true));
        let stmt = try!(self.statement());
        let _ = try!(self.eat_token(TokenKind::While, false));
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let expr = try!(self.expression());
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let Span { stop, .. } = try!(self.eat_semicolon());

        Ok(Spanned::new(Span::new(start, stop), Statement::DoWhile(expr, Box::new(stmt))))
    }

    fn while_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::While, true));
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let expr = try!(self.expression());
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let stmt = try!(self.statement());

        Ok(Spanned::new(Span::new(start, stmt.span.stop), Statement::While(expr, Box::new(stmt))))
    }

    fn for_statement(&mut self) -> ParseResult<SpannedStatement> {
        unimplemented!();
    }

    // in the ECMAScript 5.1 grammar, there are five "restricted" productions that place
    // restrictions on whether or not line terminators can appear. These productions are:
    //
    //   PostfixExpression ::= LeftHandSideExpression [restricted] (++ | --)
    //   ContinueStatement ::= "continue" [restricted] Identifier ;
    //   BreakStatement    ::= "break" [restricted] Identifier ;
    //   ReturnStatement   ::= "return" [restricted] Expression ;
    //   ThrowStatement    ::= "throw" [restricted] Expression ;
    //
    // The restricted production enforces that the next token is not preceded by a newline. If the next token
    // is preceded by a newline, it inserts a semicolon into the stream. (Section 7.9.1)
    fn continue_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::Continue, true));
        let identifier = if next_token_is!(self, false, TokenKind::Identifier(_)) {
            // this is a restricted production. if the next token is a newline,
            // we have to insert a semicolon and end this statement.
            if self.next_token_preceded_by_newline() {
                self.insert_semicolon();
                None
            } else {
                Some(try!(self.identifier()))
            }
        } else {
            None
        };

        let Span { stop, .. } = try!(self.eat_semicolon());
        Ok(Spanned::new(Span::new(start, stop), Statement::Continue(identifier)))
    }

    fn break_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::Break, true));
        let identifier = if next_token_is!(self, false, TokenKind::Identifier(_)) {
            if self.next_token_preceded_by_newline() {
                self.insert_semicolon();
                None
            } else {
                Some(try!(self.identifier()))
            }
        } else {
            None
        };

        let Span { stop, .. } = try!(self.eat_semicolon());
        Ok(Spanned::new(Span::new(start, stop), Statement::Break(identifier)))
    }

    fn return_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::Return, true));
        let expr = if !next_token_is!(self, false, TokenKind::Semicolon) {
            if self.next_token_preceded_by_newline() {
                self.insert_semicolon();
                None
            } else {
                Some(try!(self.expression()))
            }
        } else {
            None
        };

        let Span { stop, .. } = try!(self.eat_semicolon());
        Ok(Spanned::new(Span::new(start, stop), Statement::Return(expr)))
    }

    fn with_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::With, true));
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let expr = try!(self.expression());
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let stmt = try!(self.statement());

        if self.strict_mode {
            span_err!(Span::new(start, stmt.span.stop), "`with` statements are not permitted in strict mode")
        }

        Ok(Spanned::new(Span::new(start, stmt.span.stop), Statement::With(expr, Box::new(stmt))))
    }

    fn switch_statement(&mut self) -> ParseResult<SpannedStatement> {
        unimplemented!();
    }

    fn identifier_or_label(&mut self) -> ParseResult<SpannedStatement> {
        unimplemented!();
    }

    fn throw_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::Throw, true));
        let expr = if self.next_token_preceded_by_newline() {
            span_err!(self.peek(true).unwrap().span, "illegal newline after throw");
        } else {
            try!(self.expression())
        };

        let Span { stop, .. } = try!(self.eat_semicolon());
        Ok(Spanned::new(Span::new(start, stop), Statement::Throw(expr)))
    }

    fn try_statement(&mut self) -> ParseResult<SpannedStatement> {
        unimplemented!()
    }

    fn debugger_statement(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::Debugger, true));
        let Span { stop, .. } = try!(self.eat_semicolon());
        Ok(Spanned::new(Span::new(start, stop), Statement::Debugger))
    }

    fn function_definition(&mut self) -> ParseResult<SpannedStatement> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::Function, false));
        let ident = try!(self.identifier());
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let params = if next_token_is!(self, false, TokenKind::RightParen) {
            vec![]
        } else {
            try!(self.formal_parameters())
        };
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let _ = try!(self.eat_token(TokenKind::LeftBrace, false));
        let body = try!(self.function_body());
        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBrace, false));

        Ok(Spanned::new(Span::new(start, stop), Statement::Declaration(Declaration::Function(ident, params, Box::new(body)))))
    }

    fn formal_parameters(&mut self) -> ParseResult<Vec<SpannedPattern>> {
        unimplemented!()
    }

    fn function_body(&mut self) -> ParseResult<SpannedStatement> {
        unimplemented!()
    }

    fn expression_statement(&mut self) -> ParseResult<SpannedStatement> {
        let expr = try!(self.expression());
        let Span { stop, ..} = try!(self.eat_semicolon());
        if self.in_directive_prologue && expr.data == Expression::Literal(Literal::String("use strict".to_string())) {
            self.strict_mode = true;
        }

        Ok(Spanned::new(Span::new(expr.span.start, stop), Statement::Expression(expr)))
    }

    fn assignment_expression(&mut self) -> ParseResult<SpannedExpression> {
        unimplemented!()
    }

    fn expression(&mut self) -> ParseResult<SpannedExpression> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    macro_rules! single_statement_test {
        ($name:ident, $ast:pat, $input:expr) => {
            #[test]
            fn $name() {
                let mut parser = Parser::new(Lexer::new($input.chars()));
                match parser.parse_statement() {
                    Ok($ast) => (), /* success */
                    Ok(non_matching_ast) => panic!("unexpected AST: {:#?}", non_matching_ast),
                    Err(e) => panic!("unexpected error: {:?}", e)
                }
            }
        };
        ($name:ident, $ast:pat, $guard:expr, $input:expr) => {
            #[test]
            fn $name() {
                let mut parser = Parser::new(Lexer::new($input.chars()));
                match parser.parse_statement() {
                    Ok($ast) if $guard => (), /* success */
                    Ok(non_matching_ast) => panic!("unexpected AST: {:#?}", non_matching_ast),
                    Err(e) => panic!("unexpected error: {:?}", e)
                }
            }
        };
    }

    mod statements {
        use super::super::*;
        use super::super::super::ast::*;
        use super::super::super::lexer::*;

        single_statement_test!(test_break_semi, Spanned { data: Statement::Break(None), .. }, "break;");
        single_statement_test!(test_break_no_semi, Spanned { data: Statement::Break(None), .. }, "break");
        single_statement_test!(test_break_ident_semi,
                               Spanned {
                                   data: Statement::Break(Some(Spanned {
                                       ref data,
                                       ..
                                   })),
                                   ..
                               },
                               data == "asdf",
                               "break asdf;");
        single_statement_test!(test_break_ident_no_semi,
                               Spanned {
                                   data: Statement::Break(Some(Spanned {
                                       ref data,
                                       ..
                                   })),
                                   ..
                               },
                               data == "asdf",
                               "break asdf");

        single_statement_test!(test_continue_semi, Spanned { data: Statement::Continue(None), .. }, "continue;");
        single_statement_test!(test_continue_no_semi, Spanned { data: Statement::Continue(None), .. }, "continue");
        single_statement_test!(test_continue_ident_semi,
                               Spanned {
                                   data: Statement::Continue(Some(Spanned {
                                       ref data,
                                       ..
                                   })),
                                   ..
                               },
                               data == "asdf",
                               "continue asdf;");
        single_statement_test!(test_continue_ident_no_semi,
                               Spanned {
                                   data: Statement::Continue(Some(Spanned {
                                       ref data,
                                       ..
                                   })),
                                   ..
                               },
                               data == "asdf",
                               "continue asdf");

        single_statement_test!(test_debugger_semi, Spanned { data: Statement::Debugger, .. }, "debugger;");
        single_statement_test!(test_debugger_no_semi, Spanned { data: Statement::Debugger, .. }, "debugger");
    }
}
