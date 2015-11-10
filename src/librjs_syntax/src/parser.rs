use std::{f64, i64};
use num::FromPrimitive;
use ast::*;
use lexer::{Lexer, Token, TokenKind, Span};

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
            span: Default::default(),
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
    last_span: Span,
    in_disallowed: bool
}

impl<I: Iterator<Item=char>> Parser<I> {
    pub fn new(lexer: Lexer<I>) -> Parser<I> {
        Parser {
            lexer: lexer,
            strict_mode: false,
            peek_stack: vec![],
            in_directive_prologue: false,
            last_span: Default::default(),
            in_disallowed: false
        }
    }

    pub fn parse_expression(&mut self) -> ParseResult<SpannedExpression> {
        self.expression()
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
        self.peek_stack.pop()
            .or_else(|| if leading_div {
                self.lexer.next_token_leading_div()
            } else {
                self.lexer.next_token()
            })
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

        self.peek_stack.last()
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
        if offending_token.preceded_by_newline || offending_token.kind == TokenKind::RightBrace {
            // rule 1 - if the token is preceded by a newline or
            // it's a right brace, insert a semicolon.
            self.insert_semicolon();
        }
    }

    fn insert_semicolon(&mut self) {
        let last_span = self.last_span;
        self.insert_token(Token {
            span: last_span,
            kind: TokenKind::Semicolon,
            preceded_by_newline: true
        });
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
        let Span { start, .. } = try!(self.eat_token(TokenKind::For, true));
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        if next_token_is!(self, false, TokenKind::Var) {
            let Span { start: dec_start, .. } = try!(self.eat_token(TokenKind::Var, false));
            let mut declarations = vec![];
            // `in` is not allowed in this production because of
            // a potential ambiguity with the `for-in` statement
            self.in_disallowed = true;
            declarations.push(try!(self.variable_declaration()));
            loop {
                if !next_token_is!(self, false, TokenKind::Comma) {
                    break;
                }

                let _ = try!(self.eat_token(TokenKind::Comma, false));
                declarations.push(try!(self.variable_declaration()));
            }

            self.in_disallowed = false;

            let dec_end = match declarations.last() {
                Some(ref vardec) => match vardec.initial_value {
                    Some(ref s) => s.span.stop,
                    _ => vardec.id.span.stop
                },
                _ => unreachable!()
            };

            let dec_span = Span::new(dec_start, dec_end);

            // important crossroads here - if we parsed exactly one
            // variable declaration above, we can enter a `ForIn`
            // production if an `in` is used instead of a semicolon.
            // If we parsed more than one declaration, we cannot
            // allow `in` as the next token.
            if declarations.len() == 1 {
                if next_token_is!(self, false, TokenKind::In) {
                    // now we are parsing a ForIn loop in earnest.
                    let _ = try!(self.eat_token(TokenKind::In, false));
                    let expr = try!(self.expression());
                    let _ = try!(self.eat_token(TokenKind::RightParen, false));
                    let stmt = try!(self.statement());
                    let span = Span::new(start, stmt.span.stop);
                    let for_stmt = Statement::ForIn(
                        ForInit::VarDec(
                            Spanned::new(dec_span, Declaration::Variable(declarations))),
                        expr,
                        Box::new(stmt));
                    return Ok(Spanned::new(span, for_stmt));
                }
            }

            // at this point we have one or more declarations and
            // the next token should be a semicolon.
            // not calling `eat_semicolon` here, because that method
            // implicitly does automatic semicolon insertion, and
            // the spec does not allow the inserting of any of the
            // semicolons in a `for` statement.
            let _ = try!(self.eat_token(TokenKind::Semicolon, false));
            let condition = if !next_token_is!(self, false, TokenKind::Semicolon) {
                Some(try!(self.expression()))
            } else {
                None
            };

            let _ = try!(self.eat_token(TokenKind::Semicolon, false));
            let action = if !next_token_is!(self, false, TokenKind::RightParen) {
                Some(try!(self.expression()))
            } else {
                None
            };

            let _ = try!(self.eat_token(TokenKind::RightParen, false));
            let stmt = try!(self.statement());
            let span = Span::new(start, stmt.span.stop);
            let for_stmt = Statement::For(
                Some(ForInit::VarDec(
                    Spanned::new(dec_span, Declaration::Variable(declarations)))),
                condition,
                action,
                Box::new(stmt));
            return Ok(Spanned::new(span, for_stmt));
        }

        // two options here - either we are a for-loop with an init expression and
        // no variable declarations or we are a for-in loop.
        // in the for-in loop case, only left-hand-side expressions are permitted.
        // to prevent catastrophic lookahead, we parse the first expression as an
        // expression and then restrict it artificially (see assignment_expression
        // docs for more details)
        if next_token_is!(self, false, TokenKind::Semicolon) {
            // this definitely can't be a for-in loop.
            let _ = try!(self.eat_token(TokenKind::Semicolon, false));
            let condition = if !next_token_is!(self, false, TokenKind::Semicolon) {
                Some(try!(self.expression()))
            } else {
                None
            };

            let _ = try!(self.eat_token(TokenKind::Semicolon, false));
            let action = if !next_token_is!(self, false, TokenKind::RightParen) {
                Some(try!(self.expression()))
            } else {
                None
            };

            let _ = try!(self.eat_token(TokenKind::RightParen, false));
            let stmt = try!(self.statement());
            let span = Span::new(start, stmt.span.stop);
            let for_stmt = Statement::For(
                None,
                condition,
                action,
                Box::new(stmt));
            return Ok(Spanned::new(span, for_stmt));
        }

        // we have an expression of some kind here.
        self.in_disallowed = true;
        let expr = try!(self.expression());
        self.in_disallowed = false;
        // depending on what the next token is, we restrict what expr can be.
        if next_token_is!(self, false, TokenKind::In) {
            let _ = try!(self.verify_assignment_lhs(expr.clone()));
            // TODO(ES6) - in ES5 only identifiers are legal as patterns.
            // this assumption will not be valid in ES6.
            let _ = try!(self.eat_token(TokenKind::In, false));
            let in_expr = try!(self.expression());
            let _ = try!(self.eat_token(TokenKind::RightParen, false));
            let stmt = try!(self.statement());
            let span = Span::new(start, stmt.span.stop);
            let for_stmt = Statement::ForIn(
                ForInit::Expr(expr),
                in_expr,
                Box::new(stmt));
            return Ok(Spanned::new(span, for_stmt));
        }

        // otherwise, it can be anything.
        let _ = try!(self.eat_token(TokenKind::Semicolon, false));
        let condition = if !next_token_is!(self, false, TokenKind::Semicolon) {
            Some(try!(self.expression()))
        } else {
            None
        };

        let _ = try!(self.eat_token(TokenKind::Semicolon, false));
        let action = if !next_token_is!(self, false, TokenKind::RightParen) {
            Some(try!(self.expression()))
        } else {
            None
        };

        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let stmt = try!(self.statement());
        let span = Span::new(start, stmt.span.stop);
        let for_stmt = Statement::For(
            Some(ForInit::Expr(expr)),
            condition,
            action,
            Box::new(stmt));
        return Ok(Spanned::new(span, for_stmt));
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
            if self.next_token_preceded_by_newline() || next_token_is!(self, true, TokenKind::RightBrace) {
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
            if self.next_token_preceded_by_newline() || next_token_is!(self, true, TokenKind::RightBrace) {
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
        let expr = if !next_token_is!(self, true, TokenKind::Semicolon) {
            // if our next token is not a semicolon, we have to try to ascertain
            // what it is.
            if self.next_token_preceded_by_newline() || next_token_is!(self, true, TokenKind::RightBrace) {
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
        let Span { start, .. } = try!(self.eat_token(TokenKind::Switch, true));
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let expr = try!(self.expression());
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let _ = try!(self.eat_token(TokenKind::LeftBrace, false));
        let mut cases = vec![];
        loop {
            // SPEC_NOTE: for some reason a switch statement with no cases
            // is legal.
            if next_token_is!(self, false, TokenKind::RightBrace) {
                break;
            }

            cases.push(try!(self.switch_case()));
        }

        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBrace, false));

        Ok(Spanned::new(Span::new(start, stop), Statement::Switch(expr, cases)))
    }

    fn switch_case(&mut self) -> ParseResult<SwitchCase> {
        let test = if next_token_is!(self, false, TokenKind::Default) {
            let _ = try!(self.eat_token(TokenKind::Default, false));
            let _ = try!(self.eat_token(TokenKind::Colon, false));
            None
        } else {
            let _ = try!(self.eat_token(TokenKind::Case, false));
            let expr = try!(self.expression());
            let _ = try!(self.eat_token(TokenKind::Colon, false));
            Some(expr)
        };

        // this part is a little weird since we're parsing recursive
        // descent, unlike the reference grammar. We want to parse a list of statements.
        // We stop parsing when 1) we see either a "case" or "default" token, indicating
        // that we're going to start a new switch case, or 2) we see a right brace,
        // in which case we've reached the end of the switch statement.
        let mut statements = vec![];
        loop {
            if next_token_is!(self, false, TokenKind::Default, TokenKind::Case, TokenKind::RightBrace) {
                break;
            }

            statements.push(try!(self.statement()));
        }

        Ok(SwitchCase {
            test: test,
            body: statements
        })
    }

    fn identifier_or_label(&mut self) -> ParseResult<SpannedStatement> {
        // this production is a side-effect of the reference grammar being
        // LL(2). When we are parsing a statement and see an identifier,
        // it can be one of two things:
        //   1) An expression - "hello + 2", "hello.foo", etc.
        //   2) A labeled statement: "thing: while(true) break thing;"
        // The parser is currently looking at an identifier. We need to look ahead
        // one more token to see if the next token after that is a colon. If so,
        // this function parses a labeled statement. If not, this function backs
        // up one token and calls into the expression production.
        let ident_token = try!(self.identifier());
        match self.peek(false) {
            Some(&Token { kind: TokenKind::Colon, .. }) => (),
            _ => {
                // the peek stack looks like this
                // <EOS> <NOT_COLON_TOKEN>
                //       ^----------------
                // we want it to look like:
                // <EOS> <IDENTIFIER> <NOT_COLON_TOKEN>
                //       ^----------
                // we're going to have to bail here
                // since this is an expression statement
                self.insert_token(Token {
                    span: ident_token.span,
                    kind: TokenKind::Identifier(ident_token.data),
                    // i've thought about this for a while, and I think it's
                    // okay to always set this to false. The reason is that
                    // even if we choose to back off and enter the expression
                    // statement prodouction, what we see now can /always/
                    // be parsed as an expression statement with an inserted
                    // semicolon.
                    preceded_by_newline: false
                });
                return self.expression_statement();
            }
        }

        let _ = try!(self.eat_token(TokenKind::Colon, false));
        let stmt = try!(self.statement());
        Ok(Spanned::new(Span::new(ident_token.span.start, stmt.span.stop),
                        Statement::Label(ident_token, Box::new(stmt))))
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
        let Span {start, .. } = try!(self.eat_token(TokenKind::Try, true));
        let block = try!(self.block());
        let catch = if next_token_is!(self, false, TokenKind::Catch) {
            Some(try!(self.catch()))
        } else {
            None
        };

        let finally = if next_token_is!(self, false, TokenKind::Finally) {
            Some(Box::new(try!(self.finally())))
        } else {
            None
        };

        if catch.is_none() && finally.is_none() {
            span_err!(Span::new(start, self.last_span.stop),
                      "missing catch or finally after try block");
        }
        let stop = if finally.is_none() {
            catch.as_ref().unwrap().body.span.stop
        } else {
            finally.as_ref().unwrap().span.stop
        };

        Ok(Spanned::new(Span::new(start, stop),
                        Statement::Try(Box::new(block), catch, finally)))
    }

    fn catch(&mut self) -> ParseResult<CatchClause> {
        let _ = try!(self.eat_token(TokenKind::Catch, false));
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let ident = try!(self.identifier());
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let block = try!(self.block());
        Ok(CatchClause {
            param: SpannedPattern {
                span: ident.span,
                data: Pattern::Identifier(ident)
            },
            body: Box::new(block)
        })
    }

    fn finally(&mut self) -> ParseResult<SpannedStatement> {
        let _ = try!(self.eat_token(TokenKind::Finally, false));
        self.block()
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
        let (prolog, body) = try!(self.function_body());
        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBrace, false));

        let function = Function {
            name: Some(ident),
            parameters: params,
            prologue: prolog,
            body: body
        };

        Ok(Spanned::new(Span::new(start, stop),
                        Statement::Declaration(Declaration::Function(function))))
    }

    fn function_body(&mut self) -> ParseResult<(Vec<SpannedStatement>, Vec<SpannedStatement>)> {
        let mut statements = vec![];
        let mut prolog = vec![];
        self.in_directive_prologue = true;
        loop {
            if next_token_is!(self, true, TokenKind::RightBrace) {
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

        Ok((prolog, statements))
    }


    fn formal_parameters(&mut self) -> ParseResult<Vec<SpannedPattern>> {
        // there's guaranteed to be at least one identifier here.
        let mut parameters = vec![];
        let ident = try!(self.identifier());
        parameters.push(Spanned::new(ident.span, Pattern::Identifier(ident)));

        loop {
            if next_token_is!(self, false, TokenKind::RightParen) {
                break;
            }

            let _ = try!(self.eat_token(TokenKind::Comma, false));
            let ident = try!(self.identifier());
            parameters.push(Spanned::new(ident.span, Pattern::Identifier(ident)));
        }

        Ok(parameters)
    }

    fn expression_statement(&mut self) -> ParseResult<SpannedStatement> {
        let expr = try!(self.expression());
        let Span { stop, ..} = try!(self.eat_semicolon());
        if self.in_directive_prologue && expr.data == Expression::Literal(Literal::String("use strict".to_string())) {
            self.strict_mode = true;
        }

        Ok(Spanned::new(Span::new(expr.span.start, stop), Statement::Expression(expr)))
    }

    // Expressions!

    fn primary_expression(&mut self) -> ParseResult<SpannedExpression> {
        match_peek! {
            self, true,
            TokenKind::This => self.this(),
            TokenKind::Identifier(_) => self.identifier_expr(),
            TokenKind::BooleanLiteral(_) => self.boolean_literal(),
            TokenKind::StringLiteral(_) => self.string_literal(),
            TokenKind::RegularExpressionLiteral(_, _) => self.regex_literal(),
            TokenKind::DecimalLiteral(_) => self.decimal_literal(),
            TokenKind::HexIntegerLiteral(_) => self.hex_integer_literal(),
            TokenKind::OctalIntegerLiteral(_) => self.octal_integer_literal(),
            TokenKind::NullLiteral => self.null_literal(),
            TokenKind::LeftBracket => self.array_literal(),
            TokenKind::LeftBrace => self.object_literal(),
            TokenKind::LeftParen => self.paren(),
            _ => span_err!(Default::default(), "unexpected token: {:?}", self.peek(true).unwrap())
        }
    }

    fn identifier_or_keyword(&mut self) -> ParseResult<SpannedExpression> {
        // SPEC_NOTE: the spec is not clear on this, but it's actually
        // legal to use a keyword as an identifier as long as that
        // identifier is part of a member statement (i.e. this.continue())
        let token = match self.next_token(false) {
            Some(tok) => tok,
            None => span_unexpected_eof!()
        };

        self.last_span = token.span;

        let as_identifier = match token.as_identifier() {
            Some(Token { kind: TokenKind::Identifier(ref data) , .. }) => data.clone(),
            _ => span_err!(token.span, "unexpected token: expected an identifier, got {:?}", token.kind)
        };

        let ident = Spanned::new(token.span, as_identifier);
        Ok(Spanned::new(ident.span, Expression::Identifier(ident)))
    }

    fn this(&mut self) -> ParseResult<SpannedExpression> {
        let span = try!(self.eat_token(TokenKind::This, false));
        Ok(Spanned::new(span, Expression::This))
    }

    fn identifier_expr(&mut self) -> ParseResult<SpannedExpression> {
        let ident = try!(self.identifier());
        Ok(Spanned::new(ident.span, Expression::Identifier(ident)))
    }

    fn boolean_literal(&mut self) -> ParseResult<SpannedExpression> {
        let Token { span, kind, .. } = self.next_token(true).unwrap();
        if let TokenKind::BooleanLiteral(b) = kind {
            let lit = Literal::Boolean(b);
            Ok(Spanned::new(span, Expression::Literal(lit)))
        } else {
            unreachable!()
        }
    }

    fn string_literal(&mut self) -> ParseResult<SpannedExpression> {
        let Token { span, kind, .. } = self.next_token(true).unwrap();
        if let TokenKind::StringLiteral(s) = kind {
            let lit = Literal::String(s);
            Ok(Spanned::new(span, Expression::Literal(lit)))
        } else {
            unreachable!()
        }
    }

    fn regex_literal(&mut self) -> ParseResult<SpannedExpression> {
        let Token { span, kind, .. } = self.next_token(true).unwrap();
        if let TokenKind::RegularExpressionLiteral(pat, flags) = kind {
            let lit = Literal::RegExp(pat, flags);
            Ok(Spanned::new(span, Expression::Literal(lit)))
        } else {
            unreachable!()
        }
    }

    fn decimal_literal(&mut self) -> ParseResult<SpannedExpression> {
        let Token { span, kind, .. } = self.next_token(true).unwrap();
        if let TokenKind::DecimalLiteral(str_value) = kind {
            let value = self.decimal_mathmatical_value(&str_value);
            Ok(Spanned::new(span, Expression::Literal(Literal::Numeric(value))))
        } else {
            unreachable!()
        }
    }

    fn decimal_mathmatical_value(&mut self, s: &str) -> f64 {
        let dot_split : Vec<_> = s.split('.').collect();
        if dot_split.len() == 1 {
            // there was no dot at all. In this case this is
            // just an integer, possibly with an exponent
            let exponent_split : Vec<_> = s.split(|f| f == 'e' || f == 'E')
                .collect();
            if exponent_split.len() == 1 {
                return self.decimal_integer_mathematical_value(s) as f64;
            }

            let base = exponent_split[0];
            let exponent = exponent_split[1];
            let mv = self.decimal_integer_mathematical_value(base);
            let exp_mv = self.decimal_integer_mathematical_value(exponent);
            let exp_value : i32 = match FromPrimitive::from_i64(exp_mv) {
                Some(value) => value,
                None => if exp_mv.is_positive() {
                    return f64::INFINITY
                } else {
                    return 0f64
                }
            };

            return mv as f64 * 10f64.powi(exp_value);
        }

        debug_assert!(dot_split.len() == 2);
        let before_dot = dot_split[0];
        let after_dot = dot_split[1];
        if after_dot.is_empty() {
            // literals like "10." become the MV of
            // the string before the dot.
            // Can also possibly have an exponent.

            return self.decimal_integer_mathematical_value(before_dot) as f64;
        }

        if before_dot.is_empty() {
            let exponent_split : Vec<_> = after_dot
                .split(|f| f == 'e' || f == 'E')
                .collect();

            // literals like ".10" become the MV
            // of the decimal digits.
            if exponent_split.len() == 1 {
                let (mv, digits) = self.decimal_digits_mathematical_value(after_dot);
                return mv as f64 * 10f64.powi(-(digits as i32));
            }

            let digits = exponent_split[0];
            let exponent = exponent_split[1];
            let (mv, num_digits) = self.decimal_digits_mathematical_value(digits);
            let exp_mv = self.decimal_integer_mathematical_value(exponent);

            let exp_value : i32 = match FromPrimitive::from_i64(exp_mv - num_digits as i64) {
                Some(value) => value,
                None => if exp_mv.is_positive() {
                    return f64::INFINITY
                } else {
                    return 0f64
                }
            };

            return mv as f64 * 10f64.powi(-exp_value);
        }

        // next, we have to check for exponents.
        // our string has been partitioned like this so far:
        // 101949.23828e39
        // [----] [------]
        let exponent_split : Vec<_> = after_dot
            .split(|f| f == 'e' || f == 'E')
            .collect();
        if exponent_split.len() == 1 {
            // in this case, there's no exponent.
            let dmv = self.decimal_integer_mathematical_value(before_dot);
            let (after_mv, num_digits) = self.decimal_digits_mathematical_value(after_dot);
            return dmv as f64 + (after_mv as f64 * 10f64.powi(-(num_digits as i32)));
        }

        debug_assert!(exponent_split.len() == 2);
        let digits = exponent_split[0];
        let exponent = exponent_split[1];
        let dmv = self.decimal_integer_mathematical_value(before_dot);
        let exp_mv = self.decimal_integer_mathematical_value(exponent);

        // SPEC_NOTE: the spec does not specify what happens when the
        // MV of an exponent is too huge. For really huge exponents,
        // v8 reports Infinity and for very small ones it reports 0,
        // so I'll do the same.
        let exp_value : i32 = match FromPrimitive::from_i64(exp_mv) {
            Some(value) => value,
            None => if exp_mv.is_positive() {
                return f64::INFINITY
            } else {
                return 0f64
            }
        };

        if digits.is_empty() {
            return dmv as f64 * 10f64.powi(exp_value);
        }

        // now we've partitioned into this:
        // 101949.23828e39
        // [----] [---] []
        let (digit_mv, num_digits) = self.decimal_digits_mathematical_value(digits);
        return (dmv as f64 + (digit_mv as f64 * 10f64.powi(-(num_digits as i32)))) * 10f64.powi(exp_value);
    }

    fn decimal_integer_mathematical_value(&mut self, s: &str) -> i64 {
        // this is pretty straightforward integer parsing.
        if s.chars().next().unwrap() == '+' {
            // from_str_radix does not like a leading plus. The number is going
            // to be positive anyway, so get rid of it.
            i64::from_str_radix(&s[1..], 10).expect("string can't be parsed as an i64 but made it this far?")
        } else {
            i64::from_str_radix(s, 10).expect("string can't be parsed as an i64 but made it this far?")
        }
    }

    fn decimal_digits_mathematical_value(&mut self, s: &str) -> (i64, usize) {
        // this is also pretty straightforward int parsing, except
        // we have to keep track of how long the original string was.
        (i64::from_str_radix(s, 10).expect("string can't be parsed as an i64 but made it this far?"), s.len())
    }

    fn hex_integer_literal(&mut self) -> ParseResult<SpannedExpression> {
        let Token { span, kind, .. } = self.next_token(true).unwrap();
        if let TokenKind::HexIntegerLiteral(str_value) = kind {
            let value = self.hex_mathematical_value(&str_value);
            Ok(Spanned::new(span, Expression::Literal(Literal::Numeric(value))))
        } else {
            unreachable!()
        }
    }

    fn hex_mathematical_value(&mut self, s: &str) -> f64 {
        // by convention, the lexer produces the hex literal with the "0x"
        // still on it.
        i64::from_str_radix(&s[2..], 16).expect("invalid hex literal?") as f64
    }

    fn octal_integer_literal(&mut self) -> ParseResult<SpannedExpression> {
        let Token { span, kind, .. } = self.next_token(true).unwrap();
        if self.strict_mode {
            span_err!(span, "octal literals are not permitted in strict mode")
        }

        if let TokenKind::OctalIntegerLiteral(str_value) = kind {
            let value = self.octal_mathematical_value(&str_value);
            Ok(Spanned::new(span, Expression::Literal(Literal::Numeric(value))))
        } else {
            unreachable!()
        }
    }

    fn octal_mathematical_value(&mut self, s: &str) -> f64 {
        // by convention, the lexer produces the octal literal with the
        // leading zero still on it.
        i64::from_str_radix(&s[1..], 8).expect("invalid octal literal?") as f64
    }

    fn null_literal(&mut self) -> ParseResult<SpannedExpression> {
        let span = try!(self.eat_token(TokenKind::NullLiteral, true));
        Ok(Spanned::new(span, Expression::Literal(Literal::Null)))
    }

    fn array_literal(&mut self) -> ParseResult<SpannedExpression> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::LeftBracket, true));
        let mut exprs = vec![];
        loop {
            if next_token_is!(self, false, TokenKind::RightBracket) {
                break;
            }

            if next_token_is!(self, false, TokenKind::Comma) {
                // commas indicate the "Elison" production.
                let _ = try!(self.eat_token(TokenKind::Comma, false));
                exprs.push(None);
            } else {
                exprs.push(Some(try!(self.assignment_expression())));
                if !next_token_is!(self, false, TokenKind::RightBracket) {
                    // SPEC_NOTE: this is sorta weird. Things such as
                    // "[42,]" get parsed as a single-element array,
                    // by v8 and esprima, while the spec makes it seem like
                    // it should be a 42 followed by an elison.
                    let _ = try!(self.eat_token(TokenKind::Comma, false));
                }
            }
        }

        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBracket, false));
        Ok(Spanned::new(Span::new(start, stop), Expression::Array(exprs)))
    }

    fn object_literal(&mut self) -> ParseResult<SpannedExpression> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::LeftBrace, true));
        let mut properties = vec![];
        loop {
            if next_token_is!(self, false, TokenKind::RightBrace) {
                break;
            }

            properties.push(try!(self.property()));

            // the spec explicitly permits a trailing comma
            // after the final property in an object literal.
            // if it's there, eat it. If it's not, break.
            if next_token_is!(self, false, TokenKind::Comma) {
                let _ = try!(self.eat_token(TokenKind::Comma, false));
            } else {
                break;
            }
        }

        // we are now looking at either a } or a ,}.
        if next_token_is!(self, false, TokenKind::Comma) {
            let _ = try!(self.eat_token(TokenKind::Comma, false));
        }
        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBrace, false));
        Ok(Spanned::new(Span::new(start, stop), Expression::Object(properties)))
    }

    fn paren(&mut self) -> ParseResult<SpannedExpression> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::LeftParen, true));
        let mut expr = try!(self.expression());
        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightParen, true));
        expr.span = Span::new(start, stop);
        Ok(expr)
    }

    fn property(&mut self) -> ParseResult<Property> {
        // if we see a special identifier "get" or an identifier "set",
        // we have to parse these differently

        // ugly hack this sucks: rust's mutability checking
        // can not handle what we'd like to do here...
        let property_kind = match self.peek(false) {
            Some(&Token { kind: TokenKind::Identifier(ref p), .. }) if p == "get" => PropertyKind::Get,
            Some(&Token { kind: TokenKind::Identifier(ref p), .. }) if p == "set" => PropertyKind::Set,
            _ => PropertyKind::Init
        };

        match property_kind {
            PropertyKind::Get => return self.property_get(),
            PropertyKind::Set => return self.property_set(),
            PropertyKind::Init => return self.property_init()
        }
    }

    fn property_init(&mut self) -> ParseResult<Property> {
        // otherwise, we're parsing a normal property
        let name = try!(self.property_name());
        let _ = try!(self.eat_token(TokenKind::Colon, false));
        let expr = try!(self.assignment_expression());
        Ok(Property {
            key: name,
            value: Box::new(expr),
            kind: PropertyKind::Init
        })
    }

    fn property_get(&mut self) -> ParseResult<Property> {
        // we're looking at the special identifier "get"
        let ident = try!(self.identifier());
        if next_token_is!(self, false, TokenKind::Colon) {
            // this grammar's LL(2)-ness strikes again...
            // bail to the property_init production if someone
            // defined a property named "get"
            self.insert_token(Token {
                span: ident.span,
                kind: TokenKind::Identifier(ident.data),
                preceded_by_newline: false
            });

            return self.property_init();
        }

        let name = try!(self.property_name());
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let Span { start, .. } = try!(self.eat_token(TokenKind::LeftBrace, false));
        let (prolog, body) = try!(self.function_body());
        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBrace, false));
        let expr = Expression::Function(Box::new(Function {
            name: None,
            parameters: vec![],
            prologue: prolog,
            body: body
        }));
        Ok(Property {
            key: name,
            value: Box::new(Spanned::new(Span::new(start, stop), expr)),
            kind: PropertyKind::Get
        })
    }

    fn property_set(&mut self) -> ParseResult<Property> {
        let ident = try!(self.identifier());
        if next_token_is!(self, false, TokenKind::Colon) {
            // this grammar's LL(2)-ness strikes again...
            // bail to the property_init production if someone
            // defined a property named "get"
            self.insert_token(Token {
                span: ident.span,
                kind: TokenKind::Identifier(ident.data),
                preceded_by_newline: false
            });

            return self.property_init();
        }

        let name = try!(self.property_name());
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let param = try!(self.identifier());
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let Span { start, .. } = try!(self.eat_token(TokenKind::LeftBrace, false));
        let (prolog, body) = try!(self.function_body());
        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBrace, false));
        let expr = Expression::Function(Box::new(Function {
            name: None,
            parameters: vec![Spanned::new(param.span, Pattern::Identifier(param))],
            prologue: prolog,
            body: body
        }));
        Ok(Property {
            key: name,
            value: Box::new(Spanned::new(Span::new(start, stop), expr)),
            kind: PropertyKind::Set
        })
    }

    fn property_name(&mut self) -> ParseResult<LiteralOrIdentifier> {
        if next_token_is!(self, false, TokenKind::Identifier(_)) {
            let ident = try!(self.identifier());
            return Ok(LiteralOrIdentifier::Identifier(ident))
        }

        if next_token_is!(self, false, TokenKind::StringLiteral(_)) {
            let Token { span, kind, .. } = self.next_token(false).unwrap();
            if let TokenKind::StringLiteral(s) = kind {
                return Ok(LiteralOrIdentifier::Literal(Spanned::new(span, Literal::String(s))))
            } else {
                unreachable!()
            }
        }

        if next_token_is!(self, false, TokenKind::DecimalLiteral(_)) {
            let Token { span, kind, .. } = self.next_token(false).unwrap();
            if let TokenKind::DecimalLiteral(s) = kind {
                let value = self.decimal_mathmatical_value(&s);
                return Ok(LiteralOrIdentifier::Literal(Spanned::new(span, Literal::Numeric(value))))
            } else {
                unreachable!()
            }
        }

        if next_token_is!(self, false, TokenKind::HexIntegerLiteral(_)) {
            let Token { span, kind, .. } = self.next_token(false).unwrap();
            if let TokenKind::HexIntegerLiteral(s) = kind {
                let value = self.hex_mathematical_value(&s);
                return Ok(LiteralOrIdentifier::Literal(Spanned::new(span, Literal::Numeric(value))))
            } else {
                unreachable!()
            }
        }

        let Token { span, kind, .. } = self.next_token(false).unwrap();
        if let TokenKind::OctalIntegerLiteral(s) = kind {
            let value = self.octal_mathematical_value(&s);
            return Ok(LiteralOrIdentifier::Literal(Spanned::new(span, Literal::Numeric(value))))
        } else {
            span_err!(span, "invalid name for property: {:#?}", kind);
        }
    }

    fn expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.assignment_expression());
        if next_token_is!(self, false, TokenKind::Comma) {
            // this is a comma-operator sequence
            let mut sequence = vec![expr];
            loop {
                if !next_token_is!(self, false, TokenKind::Comma) {
                    break;
                }

                let _ = try!(self.eat_token(TokenKind::Comma, false));
                sequence.push(try!(self.expression()));
            }

            let span = Span::new(sequence.first().unwrap().span.start,
                                 sequence.last().unwrap().span.stop);
            Ok(Spanned::new(span, Expression::Sequence(sequence)))
        } else {
            Ok(expr)
        }
    }

    fn assignment_expression(&mut self) -> ParseResult<SpannedExpression> {
        let lhs = try!(self.conditional_expression());
        if next_token_is!(self, false, TokenKind::Equal,
                          TokenKind::StarEqual,
                          TokenKind::DivEquals,
                          TokenKind::PercentEqual,
                          TokenKind::PlusEqual,
                          TokenKind::MinusEqual,
                          TokenKind::LeftShiftEqual,
                          TokenKind::RightShiftEqual,
                          TokenKind::TripleRightShiftEqual,
                          TokenKind::BitwiseAndEqual,
                          TokenKind::BitwiseXorEqual,
                          TokenKind::BitwiseOrEqual) {
            let op = try!(self.eat_assignment_op());
            let rhs = try!(self.assignment_expression());
            let span = Span::new(lhs.span.start, rhs.span.stop);
            let lhs_pat = try!(self.verify_assignment_lhs(lhs));
            Ok(Spanned::new(span, Expression::Assignment(op, lhs_pat, Box::new(rhs))))
        } else {
            Ok(lhs)
        }
    }

    fn eat_assignment_op(&mut self) -> ParseResult<AssignmentOperator> {
        let op = match_peek!{
            self, false,
            TokenKind::Equal => AssignmentOperator::Equal,
            TokenKind::StarEqual => AssignmentOperator::TimesEqual,
            TokenKind::DivEquals => AssignmentOperator::DivEqual,
            TokenKind::PercentEqual => AssignmentOperator::ModEqual,
            TokenKind::PlusEqual => AssignmentOperator::PlusEqual,
            TokenKind::MinusEqual => AssignmentOperator::MinusEqual,
            TokenKind::LeftShiftEqual => AssignmentOperator::LeftShiftEqual,
            TokenKind::RightShiftEqual => AssignmentOperator::RightShiftEqual,
            TokenKind::TripleRightShiftEqual => AssignmentOperator::TripleRightShiftEqual,
            TokenKind::BitwiseAndEqual => AssignmentOperator::BitwiseAndEqual,
            TokenKind::BitwiseXorEqual => AssignmentOperator::BitwiseXorEqual,
            TokenKind::BitwiseOrEqual => AssignmentOperator::BitwiseOrEqual,
            _ => unreachable!()
        };

        self.next_token(false).unwrap();
        Ok(op)
    }

    fn verify_assignment_lhs(&mut self, lhs: SpannedExpression) -> ParseResult<PatternOrExpression> {
        // In order to avoid catastrophic lookahead when parsing the
        // AssignmentExpression production as given in the reference grammar,
        // we parse a strict superset of what's allowed in the left hand
        // size of an assignment statement. Here, we have to verify
        // that whatever we've parsed is legal as the left hand side
        // of an assignment operator.
        //
        // In general, all we have to do here is verify that whatever we
        // parsed is not an "operator expression", i.e. some sort of binary,
        // unary, or ternary expression.
        //
        // It's also impossible for a Sequence expression to appear
        // on the LHS of an assignment statement, since
        // the comma operator is lower in precedence than the assignment operators.
        // The expression "x, y = 42" is parsed as the expression sequence
        // Seq(x, y = 42), since the comma operator has such low precedence.
        match lhs.data {
            Expression::Unary(_, _, _) |
            Expression::Binary(_, _, _) |
            Expression::Conditional(_, _, _) |
            Expression::Assignment(_, _, _) |
            Expression::Logical(_, _, _) |
            Expression::Update(_, _, _) => span_err!(lhs.span, "illegal left-hand side in assignment"),
            _ => ()
        }

        // es5 only allows identifiers as patterns.
        let pat_or_exp = match lhs.data {
            Expression::Identifier(ref data) => PatternOrExpression::Pattern(Spanned {
                span: lhs.span,
                data: Pattern::Identifier(data.clone())
            }),
            _ => PatternOrExpression::Expr(Box::new(lhs))
        };

        Ok(pat_or_exp)
    }

    fn conditional_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.logical_or_expression());
        if next_token_is!(self, true, TokenKind::QuestionMark) {
            // ternary operator
            let _ = try!(self.eat_token(TokenKind::QuestionMark, false));
            let true_expr = try!(self.assignment_expression());
            let _ = try!(self.eat_token(TokenKind::Colon, false));
            let false_expr = try!(self.assignment_expression());
            let span = Span::new(expr.span.start, false_expr.span.stop);
            Ok(Spanned::new(span,
                            Expression::Conditional(Box::new(expr),
                                                    Box::new(true_expr),
                                                    Box::new(false_expr))))
        } else {
            Ok(expr)
        }
    }

    fn logical_or_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.logical_and_expression());
        if next_token_is!(self, true, TokenKind::LogicalOr) {
            let _ = try!(self.eat_token(TokenKind::LogicalOr, false));
            let rhs = try!(self.logical_or_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Logical(LogicalOperator::Or, Box::new(expr), Box::new(rhs))))

        } else {
            Ok(expr)
        }
    }

    fn logical_and_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.bitwise_or_expression());
        if next_token_is!(self, true, TokenKind::LogicalAnd) {
            let _ = try!(self.eat_token(TokenKind::LogicalAnd, false));
            let rhs = try!(self.logical_and_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Logical(LogicalOperator::And, Box::new(expr), Box::new(rhs))))
        } else {
            Ok(expr)
        }
    }

    fn bitwise_or_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.bitwise_xor_expression());
        if next_token_is!(self, true, TokenKind::BitwiseOr) {
            let _ = try!(self.eat_token(TokenKind::BitwiseOr, false));
            let rhs = try!(self.bitwise_or_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Binary(BinaryOperator::BitwiseOr, Box::new(expr), Box::new(rhs))))
        } else {
            Ok(expr)
        }
    }

    fn bitwise_xor_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.bitwise_and_expression());
        if next_token_is!(self, true, TokenKind::BitwiseXor) {
            let _ = try!(self.eat_token(TokenKind::BitwiseXor, false));
            let rhs = try!(self.bitwise_xor_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Binary(BinaryOperator::BitwiseXor, Box::new(expr), Box::new(rhs))))
        } else {
            Ok(expr)
        }
    }

    fn bitwise_and_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.equality_expression());
        if next_token_is!(self, true, TokenKind::BitwiseAnd) {
            let _ = try!(self.eat_token(TokenKind::BitwiseAnd, false));
            let rhs = try!(self.bitwise_and_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Binary(BinaryOperator::BitwiseAnd, Box::new(expr), Box::new(rhs))))
        } else {
            Ok(expr)
        }
    }

    fn equality_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.relational_expression());
        if next_token_is!(self, true, TokenKind::DoubleEqual,
                          TokenKind::DoubleNotEqual,
                          TokenKind::TripleEqual,
                          TokenKind::TripleNotEqual) {
            let kind = try!(self.eat_equality_operator());
            let rhs = try!(self.equality_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Binary(kind, Box::new(expr), Box::new(rhs))))
        } else {
            Ok(expr)
        }
    }

    fn eat_equality_operator(&mut self) -> ParseResult<BinaryOperator> {
        let kind = match_peek! {
            self, false,
            TokenKind::DoubleEqual => BinaryOperator::Equal,
            TokenKind::DoubleNotEqual => BinaryOperator::NotEqual,
            TokenKind::TripleEqual => BinaryOperator::StrictEqual,
            TokenKind::TripleNotEqual => BinaryOperator::StrictNotEqual,
            _ => unreachable!()
        };

        self.next_token(false).unwrap();
        Ok(kind)
    }

    fn relational_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.shift_expression());
        if next_token_is!(self, true, TokenKind::LessThan,
                          TokenKind::LessThanEqual,
                          TokenKind::GreaterThan,
                          TokenKind::GreaterThanEqual,
                          TokenKind::InstanceOf) {
            let kind = try!(self.eat_relational_operator());
            let rhs = try!(self.relational_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Binary(kind, Box::new(expr), Box::new(rhs))))
        } else if !self.in_disallowed && next_token_is!(self, false, TokenKind::In) {
            let _ = try!(self.eat_token(TokenKind::In, false));
            let rhs = try!(self.relational_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Binary(BinaryOperator::In, Box::new(expr), Box::new(rhs))))
        } else {
            Ok(expr)
        }
    }

    fn eat_relational_operator(&mut self) -> ParseResult<BinaryOperator> {
        let kind = match_peek! {
            self, false,
            TokenKind::LessThan => BinaryOperator::LessThan,
            TokenKind::LessThanEqual => BinaryOperator::LessThanEq,
            TokenKind::GreaterThan => BinaryOperator::GreaterThan,
            TokenKind::GreaterThanEqual => BinaryOperator::GreaterThanEq,
            TokenKind::InstanceOf => BinaryOperator::Instanceof,
            _ => unreachable!()
        };

        self.next_token(false).unwrap();
        Ok(kind)
    }

    fn shift_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.additive_expression());
        if next_token_is!(self, true, TokenKind::LeftShift,
                          TokenKind::RightShift,
                          TokenKind::TripleRightShift) {
            let kind = try!(self.eat_shift_operator());
            let rhs = try!(self.shift_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Binary(kind, Box::new(expr), Box::new(rhs))))
        } else {
            Ok(expr)
        }
    }

    fn eat_shift_operator(&mut self) -> ParseResult<BinaryOperator> {
        let kind = match_peek! {
            self, false,
            TokenKind::LeftShift => BinaryOperator::LeftShift,
            TokenKind::RightShift => BinaryOperator::RightShift,
            TokenKind::TripleRightShift => BinaryOperator::TripleRightShift,
            _ => unreachable!()
        };

        self.next_token(false).unwrap();
        Ok(kind)
    }

    fn additive_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.multiplicative_expression());
        if next_token_is!(self, true, TokenKind::Plus,
                          TokenKind::Minus) {
            let kind = try!(self.eat_additive_operator());
            let rhs = try!(self.additive_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Binary(kind, Box::new(expr), Box::new(rhs))))
        } else {
            Ok(expr)
        }
    }

    fn eat_additive_operator(&mut self) -> ParseResult<BinaryOperator> {
        let kind = match_peek! {
            self, false,
            TokenKind::Plus => BinaryOperator::Plus,
            TokenKind::Minus => BinaryOperator::Minus,
            _ => unreachable!()
        };

        self.next_token(false).unwrap();
        Ok(kind)
    }

    fn multiplicative_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.unary_expression());
        if next_token_is!(self, true, TokenKind::Star,
                          TokenKind::Div,
                          TokenKind::Percent) {
            let kind = try!(self.eat_multiplicative_operator());
            let rhs = try!(self.multiplicative_expression());
            let span = Span::new(expr.span.start, rhs.span.stop);
            Ok(Spanned::new(span,
                            Expression::Binary(kind, Box::new(expr), Box::new(rhs))))
        } else {
            Ok(expr)
        }
    }

    fn eat_multiplicative_operator(&mut self) -> ParseResult<BinaryOperator> {
        let kind = match_peek! {
            self, false,
            TokenKind::Star => BinaryOperator::Times,
            TokenKind::Div => BinaryOperator::Div,
            TokenKind::Percent => BinaryOperator::Mod,
            _ => unreachable!()
        };

        self.next_token(false).unwrap();
        Ok(kind)
    }

    fn unary_expression(&mut self) -> ParseResult<SpannedExpression> {
        if next_token_is!(self, true, TokenKind::Delete,
                          TokenKind::Void,
                          TokenKind::TypeOf,
                          TokenKind::Plus,
                          TokenKind::Minus,
                          TokenKind::BitwiseNot,
                          TokenKind::LogicalNot) {
            let kind = try!(self.eat_unary_operator());
            let kind_span = self.last_span;
            let expr = Box::new(try!(self.unary_expression()));
            let span = Span::new(kind_span.start, expr.span.stop);
            Ok(Spanned::new(span, Expression::Unary(kind, true, expr)))
        } else if next_token_is!(self, true, TokenKind::DoublePlus,
                                 TokenKind::DoubleMinus) {
            let kind = try!(self.eat_update_operator());
            let kind_span = self.last_span;
            let expr = Box::new(try!(self.unary_expression()));
            let span = Span::new(kind_span.start, expr.span.stop);
            Ok(Spanned::new(span, Expression::Update(kind, true, expr)))
        }
        else {
            self.postfix_expression()
        }
    }

    fn eat_update_operator(&mut self) -> ParseResult<UpdateOperator> {
        let kind = match_peek! {
            self, false,
            TokenKind::DoublePlus => UpdateOperator::Increment,
            TokenKind::DoubleMinus => UpdateOperator::Decrement,
            _ => unreachable!()
        };

        self.next_token(false).unwrap();
        Ok(kind)
    }

    fn eat_unary_operator(&mut self) -> ParseResult<UnaryOperator> {
        let kind = match_peek! {
            self, false,
            TokenKind::Delete => UnaryOperator::Delete,
            TokenKind::Void => UnaryOperator::Void,
            TokenKind::TypeOf => UnaryOperator::Typeof,
            TokenKind::Plus => UnaryOperator::Plus,
            TokenKind::Minus => UnaryOperator::Minus,
            TokenKind::BitwiseNot => UnaryOperator::BitwiseNot,
            TokenKind::LogicalNot => UnaryOperator::LogicalNot,
            _ => unreachable!()
        };

        self.next_token(false).unwrap();
        Ok(kind)
    }

    fn postfix_expression(&mut self) -> ParseResult<SpannedExpression> {
        let expr = try!(self.left_hand_side_expression());
        if next_token_is!(self, true, TokenKind::DoublePlus,
                          TokenKind::DoubleMinus) {
            let token = self.next_token(true).unwrap();
            // this is a restricted production. If this token is
            // preceded by a newline, we have to insert a semicolon
            // and backtrack.
            if token.preceded_by_newline {
                self.insert_token(token);
                self.insert_semicolon();
                Ok(expr)
            } else {
                let kind = if token.kind == TokenKind::DoublePlus {
                    UpdateOperator::Increment
                } else {
                    UpdateOperator::Decrement
                };
                let span = Span::new(expr.span.start, token.span.stop);
                Ok(Spanned::new(span, Expression::Update(kind, false, Box::new(expr))))
            }
        } else {
            Ok(expr)
        }
    }

    fn left_hand_side_expression(&mut self) -> ParseResult<SpannedExpression> {
        match_peek! {
            self, true,
            TokenKind::New => self.new_expression(),
            _ => self.call_expression()
        }
    }

    // SPEC_NOTE: we deviate from the reference grammar a little bit here to
    // avoid separating the logic that parses new calls.
    fn new_expression(&mut self) -> ParseResult<SpannedExpression> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::New, true));
        let mut expr = if next_token_is!(self, true, TokenKind::New) {
            let new_expr = try!(self.new_expression());
            let span = Span::new(start, new_expr.span.stop);
            Spanned::new(span, Expression::New(Box::new(new_expr), vec![]))
        } else {
            let member = try!(self.member_expression());
            if next_token_is!(self, false, TokenKind::LeftParen) {
                let _ = try!(self.eat_token(TokenKind::LeftParen, false));
                let args = try!(self.actual_parameters());
                let Span { stop, .. } = try!(self.eat_token(TokenKind::RightParen, false));
                Spanned::new(Span::new(start, stop),
                                Expression::New(Box::new(member), args))
            } else {
                // it's not required for a new expression to have arguments.
                let span = Span::new(start, member.span.stop);
                Spanned::new(span, Expression::New(Box::new(member), vec![]))
            }
        };

        // parse potentially chained calls
        loop {
            if self.peek(false).is_none() {
                break;
            }

            match_peek! {
                self, false,
                TokenKind::LeftParen => {
                    let _ = try!(self.eat_token(TokenKind::LeftParen, false));
                    let args = try!(self.actual_parameters());
                    let Span { stop, .. } = try!(self.eat_token(TokenKind::RightParen, false));
                    expr = Spanned::new(Span::new(expr.span.start, stop),
                                        Expression::Call(Box::new(expr), args));
                },
                TokenKind::LeftBracket => {
                    let _ = try!(self.eat_token(TokenKind::LeftBracket, false));
                    let property = try!(self.expression());
                    let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBracket, false));
                    expr = Spanned::new(Span::new(expr.span.start, stop),
                                        Expression::Member(Box::new(expr), Box::new(property), true));
                },
                TokenKind::Dot => {
                    let _ = try!(self.eat_token(TokenKind::Dot, false));
                    let ident = try!(self.identifier_or_keyword());
                    expr = Spanned::new(Span::new(expr.span.start, ident.span.stop),
                                        Expression::Member(Box::new(expr), Box::new(ident), false));
                },
                _ => break
            }
        }

        Ok(expr)
    }

    fn actual_parameters(&mut self) -> ParseResult<Vec<SpannedExpression>> {
        if next_token_is!(self, true, TokenKind::RightParen) {
            return Ok(vec![])
        }

        let mut parameters = vec![];
        loop {
            parameters.push(try!(self.assignment_expression()));

            if next_token_is!(self, true, TokenKind::RightParen) {
                break;
            }

            let _ = try!(self.eat_token(TokenKind::Comma, false));
        }

        Ok(parameters)
    }

    fn call_expression(&mut self) -> ParseResult<SpannedExpression> {
        let member = try!(self.member_expression());
        if !next_token_is!(self, false, TokenKind::LeftParen) {
            return Ok(member)
        }

        // otherwise - it's a call.
        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let args = try!(self.actual_parameters());
        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightParen, false));

        let mut expr = Spanned::new(Span::new(member.span.start, stop),
                                    Expression::Call(Box::new(member), args));
        // if we are looking at a (, a [, or a ., this is a chained call
        // and we need to keep parsing.
        loop {
            if self.peek(false).is_none() {
                break;
            }

            match_peek! {
                self, false,
                TokenKind::LeftParen => {
                    let _ = try!(self.eat_token(TokenKind::LeftParen, false));
                    let args = try!(self.actual_parameters());
                    let Span { stop, .. } = try!(self.eat_token(TokenKind::RightParen, false));
                    expr = Spanned::new(Span::new(expr.span.start, stop),
                                        Expression::Call(Box::new(expr), args));
                },
                TokenKind::LeftBracket => {
                    let _ = try!(self.eat_token(TokenKind::LeftBracket, false));
                    let property = try!(self.expression());
                    let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBracket, false));
                    expr = Spanned::new(Span::new(expr.span.start, stop),
                                        Expression::Member(Box::new(expr), Box::new(property), true));
                },
                TokenKind::Dot => {
                    let _ = try!(self.eat_token(TokenKind::Dot, false));
                    let ident = try!(self.identifier_or_keyword());
                    expr = Spanned::new(Span::new(expr.span.start, ident.span.stop),
                                        Expression::Member(Box::new(expr), Box::new(ident), false));
                },
                _ => break
            }
        }

        Ok(expr)
    }

    fn member_expression(&mut self) -> ParseResult<SpannedExpression> {
        let mut expr = if next_token_is!(self, true, TokenKind::Function) {
            try!(self.function_expression())
        } else {
            try!(self.primary_expression())
        };

        loop {
            if self.peek(false).is_none() {
                break;
            }

            match_peek! {
                self, false,
                TokenKind::LeftBracket => {
                    let _ = try!(self.eat_token(TokenKind::LeftBracket, false));
                    let property = try!(self.expression());
                    let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBracket, false));
                    expr = Spanned::new(Span::new(expr.span.start, stop),
                                        Expression::Member(Box::new(expr), Box::new(property), true));
                },
                TokenKind::Dot => {
                    let _ = try!(self.eat_token(TokenKind::Dot, false));
                    let ident = try!(self.identifier_or_keyword());
                    expr = Spanned::new(Span::new(expr.span.start, ident.span.stop),
                                        Expression::Member(Box::new(expr), Box::new(ident), false));
                },
                _ => break
            }
        }

        Ok(expr)
    }

    fn function_expression(&mut self) -> ParseResult<SpannedExpression> {
        let Span { start, .. } = try!(self.eat_token(TokenKind::Function, true));
        let ident = if next_token_is!(self, false, TokenKind::Identifier(_)) {
            Some(try!(self.identifier()))
        } else {
            None
        };

        let _ = try!(self.eat_token(TokenKind::LeftParen, false));
        let params = if next_token_is!(self, false, TokenKind::RightParen) {
            vec![]
        } else {
            try!(self.formal_parameters())
        };
        let _ = try!(self.eat_token(TokenKind::RightParen, false));
        let _ = try!(self.eat_token(TokenKind::LeftBrace, false));
        let (prolog, body) = try!(self.function_body());
        let Span { stop, .. } = try!(self.eat_token(TokenKind::RightBrace, false));

        Ok(Spanned::new(Span::new(start, stop),
                        Expression::Function(Box::new(Function {
                            name: ident,
                            parameters: params,
                            prologue: prolog,
                            body: body
                        }))))
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

        single_statement_test!(test_try_catch,
                               Spanned {
                                   data: Statement::Try(_,
                                                        Some(CatchClause {
                                                            ..
                                                        }),
                                                        None),
                                   ..
                               },
                               "try { debugger; } catch (ident) { debugger; }");
    }
}
