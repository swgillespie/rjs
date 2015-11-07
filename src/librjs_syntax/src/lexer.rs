use phf;
use itertools::PutBackN;
use std::char;
use char_classes::CharClassExt;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
pub struct Span {
    pub start: Position,
    pub stop: Position
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
pub struct Position {
    pub col: u32,
    pub line: u32
}

impl Span {
    pub fn new(start: Position, stop: Position) -> Span {
        Span {
            start: start,
            stop: stop
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
    pub preceded_by_newline: bool
}

impl Token {
    pub fn is_illegal(&self) -> bool {
        match self.kind {
            TokenKind::Illegal(_) => true,
            _ => false
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // ReservedWords, section 7.6.1.1
    Break,
    Do,
    InstanceOf,
    TypeOf,
    Case,
    Else,
    New,
    Var,
    Catch,
    Finally,
    Return,
    Void,
    Continue,
    For,
    Switch,
    While,
    Debugger,
    Function,
    This,
    With,
    Default,
    If,
    Throw,
    Delete,
    In,
    Try,
    // FutureReservedWords, section 7.6.1.2
    Class,
    Enum,
    Extends,
    Super,
    Const,
    Export,
    Import,
    // Strict mode FutureReservedWords, section 7.6.1.2
    // implements, let, private, public, yield, interface,
    // package, protected, static
    FutureReservedWordStrict(&'static str),
    // Punctuators, section 7.7
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Dot,
    Semicolon,
    Comma,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    DoubleEqual,
    DoubleNotEqual,
    TripleEqual,
    TripleNotEqual,
    Plus,
    Minus,
    Star,
    Percent,
    DoublePlus,
    DoubleMinus,
    LeftShift,
    RightShift,
    TripleRightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    QuestionMark,
    Colon,
    Equal,
    PlusEqual,
    MinusEqual,
    StarEqual,
    PercentEqual,
    LeftShiftEqual,
    RightShiftEqual,
    TripleRightShiftEqual,
    BitwiseAndEqual,
    BitwiseOrEqual,
    BitwiseXorEqual,
    // DivPunctuators, section 7.7
    Div,
    DivEquals,
    // Literals, section 7.8
    NullLiteral,                 // 7.8.1
    BooleanLiteral(bool),        // 7.8.2
    StringLiteral(String),
    RegularExpressionLiteral(String, String),
    // Numeric literals, section 7.8.3
    DecimalLiteral(String),
    HexIntegerLiteral(String),
    // non-strict-mode Octal Literals, section B.1.1
    OctalIntegerLiteral(String),
    // Identifiers, section 7.6
    Identifier(String),
    // Token indicating that an error occured during lexing.
    // The parser has no rules that accept this token, so it
    // will always cause an error.
    Illegal(String)
}

include!(concat!(env!("OUT_DIR"), "/reserved_words.rs"));

pub struct Lexer<I: Iterator<Item=char>> {
    position_col: u32,
    position_line: u32,
    stream: PutBackN<I>,
    prev_token_was_newline: bool
}

impl<I: Iterator<Item=char>> Lexer<I> {
    pub fn new(iter: I) -> Lexer<I> {
        Lexer {
            position_col: 0,
            position_line: 0,
            stream: PutBackN::new(iter),
            prev_token_was_newline: false
        }
    }

    fn illegal_token(&self, message: String) -> Token {
        Token {
            span: Span {
                start: Position {
                    col: self.position_col,
                    line: self.position_line
                },
                stop: Position {
                    col: self.position_col,
                    line: self.position_line
                }
            },
            kind: TokenKind::Illegal(message),
            preceded_by_newline: self.prev_token_was_newline
        }
    }

    fn advance(&mut self) {
        let _ = self.stream.next().expect("advance called when stream was empty");
        self.position_col += 1;
    }

    fn unadvance(&mut self, c: char) {
        match c {
            '\n' | '\r' | '\u{2028}' | '\u{2029}' => panic!("unadvanced a line terminator!"),
            _ => ()
        }

        self.stream.put_back(c);
        self.position_col -= 1;
    }

    fn peek(&mut self) -> Option<char> {
        let c = self.stream.next();
        if let Some(n) = c {
            self.stream.put_back(n);
        }

        c
    }

    fn yield_single_char_token(&mut self, kind: TokenKind) -> Option<Token> {
        let tok = Token {
            span: Span {
                start: Position {
                    col: self.position_col,
                    line: self.position_line
                },
                stop: Position {
                    col: self.position_col,
                    line: self.position_line
                }
            },
            kind: kind,
            preceded_by_newline: self.prev_token_was_newline
        };

        self.position_col += 1;
        self.advance();
        Some(tok)
    }

    // section 7.4 - comments
    fn comment(&mut self) -> Result<(), String> {
        // here we are pointed at the second character of something
        // that we've confirmed to be a comment - it'll be either * or /
        match self.peek() {
            Some('*') => self.multiline_comment(),
            Some('/') => {
                self.singleline_comment();
                Ok(())
            },
            _ => unreachable!()
        }
    }

    fn singleline_comment(&mut self) {
        self.advance();
        // all we have to do here is scan to the first line terminator.
        loop {
            match self.peek() {
                Some(c) if c.is_es_line_terminator() => {
                    self.eat_line_terminator();
                    break;
                }
                _ => self.advance()
            }
        }
    }

    fn multiline_comment(&mut self) -> Result<(), String> {
        self.advance();
        // now we are in the body of a comment.
        // we proceed until we find a */. Nested comments are not
        // allowed in ecmascript so the comments ends as soon
        // as this sequence is found.
        //
        // Here we have a small state machine to detect the ending pattern
        let mut has_line_terminator = false;
        let mut prev_char_is_star = false;
        loop {
            match self.peek() {
                Some('*') => {
                    prev_char_is_star = true;
                    self.advance();
                },
                Some('/') if prev_char_is_star => {
                    // end of comment
                    self.advance();
                    self.prev_token_was_newline = has_line_terminator;
                    return Ok(());
                },
                Some(c) if c.is_es_line_terminator() => {
                    has_line_terminator = true;
                    prev_char_is_star = false;
                    self.eat_line_terminator();
                },
                Some(_) => {
                    prev_char_is_star = false;
                    self.advance();
                }
                _ => return Err("unterminated block comment".to_string())
            }
        }
    }

    fn new_token_of_length(&mut self, kind: TokenKind, len: u32) -> Token {
        // the contract is that the input stream has been advanced to the final character
        // of this token. Therefore, the start position is self.current_col - len.
        // YOU SHOULD NOT USE THIS to report the Line Separator token, as the span calculation
        // is different in that case.
        Token {
            span: Span {
                start: Position {
                    col: self.position_col - len,
                    line: self.position_line
                },
                stop: Position {
                    col: self.position_col - 1,
                    line: self.position_line
                }
            },
            kind: kind,
            preceded_by_newline: self.prev_token_was_newline
        }
    }

    fn div_or_div_equal(&mut self) -> Token {
        self.advance();
        match self.peek() {
            Some('=') => {
                self.advance();
                self.new_token_of_length(TokenKind::DivEquals, 2)
            },
            _ => self.new_token_of_length(TokenKind::Div, 1)
        }
    }

    fn regex(&mut self) -> Token {
        // the lexer is pointed at a /
        self.advance();
        let regex_body = match self.regex_body() {
            Ok(r) => r,
            Err(e) => return self.illegal_token(e)
        };

        // the lexer is again pointed at a /
        self.advance();
        let regex_flags = match self.regex_flags() {
            Ok(r) => r,
            Err(e) => return self.illegal_token(e)
        };

        self.new_token_of_length(TokenKind::RegularExpressionLiteral(regex_body.clone(), regex_flags.clone()),
                                 regex_body.len() as u32 + regex_flags.len() as u32 + 2)
    }

    fn regex_body(&mut self) -> Result<String, String> {
        let mut buffer = String::new();
        // we've already verified that the character we are currently
        // pointed at is not a * or / - if it was, it would be lexed as a comment.
        // since we are here and not at the comment handler, we can assume
        // we're not looking at a * or /.
        debug_assert!(self.peek() != Some('*'));
        debug_assert!(self.peek() != Some('/'));
        loop {
            match self.peek() {
                Some('\\') => try!(self.regex_backslash_sequence(&mut buffer)),
                Some('[') => try!(self.regex_char_class(&mut buffer)),
                Some('/') => break,
                Some(c) if !c.is_es_line_terminator() => buffer.push(c),
                _ => return Err("unterminated regex literal".to_string())
            }

            self.advance();
        }

        Ok(buffer)
    }

    fn regex_backslash_sequence(&mut self, buffer: &mut String) -> Result<(), String> {
        self.advance();
        match self.peek() {
            Some(c) if !c.is_es_line_terminator() => {
                buffer.push('\\');
                buffer.push(c);
                Ok(())
            },
            _ => Err("invalid regex literal".to_string())
        }
    }

    fn regex_char_class(&mut self, buffer: &mut String) -> Result<(), String> {
        self.advance();
        buffer.push('[');
        loop {
            match self.peek() {
                Some(']') => break,
                Some('\\') => try!(self.regex_backslash_sequence(buffer)),
                Some(c) if !c.is_es_line_terminator() => buffer.push(c),
                _ => return Err("unterminated regex literal".to_string())
            }

            self.advance();
        }

        buffer.push(']');
        Ok(())
    }

    fn regex_flags(&mut self) -> Result<String, String> {
        let mut buffer = String::new();
        loop {
            match self.peek() {
                Some(c) if c.is_es_identifier_part() => buffer.push(c),
                _ => break
            }

            self.advance();
        }

        Ok(buffer)
    }

    fn plus_or_plus_equal(&mut self) -> Token {
        // the lexer is pointed at a +.
        // this can be either a +, +=, or ++
        self.advance();
        match self.peek() {
            Some('=') => {
                self.advance();
                self.new_token_of_length(TokenKind::PlusEqual, 2)
            },
            Some('+') => {
                self.advance();
                self.new_token_of_length(TokenKind::DoublePlus, 2)
            },
            _ => self.new_token_of_length(TokenKind::Plus, 1)
        }
    }

    fn minus_or_minus_equal(&mut self) -> Token {
        // can be either -, -=, or --
        self.advance();
        match self.peek() {
            Some('=') => {
                self.advance();
                self.new_token_of_length(TokenKind::MinusEqual, 2)
            },
            Some('-') => {
                self.advance();
                self.new_token_of_length(TokenKind::DoubleMinus, 2)
            }
            _ => self.new_token_of_length(TokenKind::Minus, 1)
        }
    }

    fn star_or_star_equal(&mut self) -> Token {
        self.advance();
        match self.peek() {
            Some('=') => {
                self.advance();
                self.new_token_of_length(TokenKind::StarEqual, 2)
            },
            _ => self.new_token_of_length(TokenKind::Star, 1)
        }
    }
    fn right_cmp_or_shift(&mut self) -> Token {
        // the lexer is pointed at a <.
        // This can be either <, <=, <<, or <<=.
        self.advance();
        match self.peek() {
            Some('=') => {
                // only one possibility: <=.
                self.advance();
                self.new_token_of_length(TokenKind::LessThanEqual, 2)
            },
            Some('<') => {
                // can be either << or <<=.
                self.advance();
                match self.peek() {
                    Some('=') => {
                        self.advance();
                        self.new_token_of_length(TokenKind::LeftShiftEqual, 3)
                    },
                    _ => self.new_token_of_length(TokenKind::LeftShift, 2)
                }
            },
            _ => self.new_token_of_length(TokenKind::LessThan, 1)
        }
    }

    fn left_cmp_or_shift(&mut self) -> Token {
        // the lexer is pointed at a >.
        // This can be either >, >=, >>, >>=, >>>, or >>>=
        self.advance();
        match self.peek() {
            Some('=') => {
                self.advance();
                self.new_token_of_length(TokenKind::GreaterThanEqual, 2)
            },
            Some('>') => {
                self.advance();
                match self.peek() {
                    Some('>') => {
                        self.advance();
                        match self.peek() {
                            Some('=') => {
                                self.advance();
                                self.new_token_of_length(TokenKind::TripleRightShiftEqual, 4)
                            },
                            _ => self.new_token_of_length(TokenKind::TripleRightShift, 3)
                        }
                    },
                    Some('=') => {
                        self.advance();
                        self.new_token_of_length(TokenKind::RightShiftEqual, 3)
                    },
                    _ => self.new_token_of_length(TokenKind::RightShift, 2)
                }
            },
            _ => self.new_token_of_length(TokenKind::GreaterThan, 1)
        }
    }

    fn eq_cmp_or_assign(&mut self) -> Token {
        // the lexer is pointed at a =.
        // This can be either =, ==, or ===.
        self.advance();
        match self.peek() {
            Some('=') => {
                self.advance();
                match self.peek() {
                    Some('=') => {
                        self.advance();
                        self.new_token_of_length(TokenKind::TripleEqual, 3)
                    },
                    _ => self.new_token_of_length(TokenKind::DoubleEqual, 2)
                }
            },
            _ => self.new_token_of_length(TokenKind::Equal, 1)
        }
    }

    fn not_or_not_eq(&mut self) -> Token {
        // the lexer is pointed at a !.
        // This can be either !, !=, or !==.
        self.advance();
        match self.peek() {
            Some('=') => {
                self.advance();
                match self.peek() {
                    Some('=') => {
                        self.advance();
                        self.new_token_of_length(TokenKind::TripleNotEqual, 3)
                    },
                    _ => self.new_token_of_length(TokenKind::DoubleNotEqual, 2)
                }
            },
            _ => self.new_token_of_length(TokenKind::LogicalNot, 1)
        }
    }

    fn and_or_bitwise_and(&mut self) -> Token {
        // the lexer is pointed at a &.
        // This can be either &, &&, or &=.
        self.advance();
        match self.peek() {
            Some('&') => {
                self.advance();
                self.new_token_of_length(TokenKind::LogicalAnd, 2)
            },
            Some('=') => {
                self.advance();
                self.new_token_of_length(TokenKind::BitwiseAndEqual, 2)
            },
            _ => self.new_token_of_length(TokenKind::BitwiseAnd, 1)
        }
    }

    fn or_or_bitwise_or(&mut self) -> Token {
        // the lexer is pointed at a |.
        // This can either be |, ||, or |=
        self.advance();
        match self.peek() {
            Some('|') => {
                self.advance();
                self.new_token_of_length(TokenKind::LogicalOr, 2)
            },
            Some('=') => {
                self.advance();
                self.new_token_of_length(TokenKind::BitwiseOrEqual, 2)
            },
            _ => self.new_token_of_length(TokenKind::BitwiseOr, 1)
        }
    }

    fn xor_or_assign(&mut self) -> Token {
        // the lexer is pointed at a ^.
        // This can either be ^ or ^=.
        self.advance();
        match self.peek() {
            Some('=') => {
                self.advance();
                self.new_token_of_length(TokenKind::BitwiseXorEqual, 2)
            },
            _ => self.new_token_of_length(TokenKind::BitwiseXor, 1)
        }
    }

    fn percent_or_equals(&mut self) -> Token {
        // the lexer is pointed at a %.
        // This can either be % or %=.
        self.advance();
        match self.peek() {
            Some('=') => {
                self.advance();
                self.new_token_of_length(TokenKind::PercentEqual, 2)
            },
            _ => self.new_token_of_length(TokenKind::Percent, 1)
        }
    }

    fn identifier(&mut self) -> Token {
        // four possible cases for what we're looking at:
        // 1) unicode letter, in which case we scan an identifier
        // 2) $, or _, in which case we scan an identifier,
        // 3) \, in which case we scan a unicode escape sequence.
        // this is only for the first letter of the identifier

        // XXX: identifier unicode literals
        let first_char = match self.peek().unwrap() {
            '\\' => return self.illegal_token("identifier unicode literals are not implemented".to_string()),
            c => c
        };

        self.advance();

        // scan the identifier, starting with the character we're looking at.
        let mut buffer = String::new();
        buffer.push(first_char);
        loop {
            match self.peek() {
                Some(c) if c.is_es_identifier_part() => buffer.push(c),
                _ => break
            }

            self.advance();
        }

        let kind = RESERVED_WORDS
            .get(&*buffer)
            .cloned()
            .unwrap_or(TokenKind::Identifier(buffer.clone()));

        self.new_token_of_length(kind, buffer.len() as u32)
    }

    fn eat_line_terminator(&mut self) {
        debug_assert!(self.peek().is_some());
        debug_assert!(self.peek().unwrap().is_es_line_terminator());
        if let Some('\n') = self.peek() {
            self.advance();
            if let Some('\r') = self.peek() {
                self.advance();
            }
        } else {
            self.advance();
        }

        self.position_col = 0;
        self.position_line += 1;
    }

    fn whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(c) if c.is_es_whitespace() => self.advance(),
                Some(c) if c.is_es_line_terminator() => {
                    self.prev_token_was_newline = true;
                    self.eat_line_terminator();
                },
                _ => break
            }
        }
    }

    fn dot_or_decimal_literal(&mut self) -> Token {
        // the lexer is pointing at a dot.
        // if the thing after a dot is a digit, it's a decimal literal.
        self.advance();
        match self.peek() {
            Some(c) if c.is_digit(10) => {
                self.unadvance('.');
                self.decimal_literal(false)
            },
            _ => self.new_token_of_length(TokenKind::Dot, 1)
        }
    }

    fn numeric_literal(&mut self) -> Token {
        // the lexer is pointing at something that is a digit OR a dot.
        // if it's a 0, it can be one of several things:
        //   1) decimal literal (0.012345),
        //   2) hexadecimal literal (0xDEADBEEF)
        //   3) octal literal (0777)
        // We can determine which one it is by peeking at the second character.
        // if it's a dot, it has to be a decimal literal.
        match self.peek() {
            Some('0') => {
                self.advance();
                match self.peek() {
                    Some('x') | Some('X') => self.hex_literal(),
                    Some('.') => self.decimal_literal(true),
                    Some(_) => self.octal_literal(),
                    None => self.new_token_of_length(TokenKind::DecimalLiteral("0".to_string()), 1)
                }
            },
            Some(_) => self.decimal_literal(false),
            _ => unreachable!()
        }
    }

    fn hex_literal(&mut self) -> Token {
        // for a literal like 0xBEEF, the lexer is pointing
        //                     ^-- here.
        // So we advance the stream and scan as many digits as we can.
        self.advance();
        let mut buffer = String::new();
        buffer.push('0');
        buffer.push('x');
        loop {
            match self.peek() {
                Some(c) if c.is_digit(16) => buffer.push(c),
                Some(c) if c.is_es_identifier_start() => {
                    return self.illegal_token("invalid hex literal".to_string())
                },
                _ => break
            }

            self.advance();
        }

        self.new_token_of_length(TokenKind::HexIntegerLiteral(buffer.clone()), buffer.len() as u32)
    }

    fn decimal_literal(&mut self, leading_zero: bool) -> Token {
        // here we could be looking at several things:
        //  1) a dot, in which case we can assume that either there
        //     was a leading zero and it's gone or there was not,
        //  2) a digit, in which case it's a straight decimal literal.
        let mut buffer = String::new();
        if leading_zero {
            buffer.push('0');
        }

        let mut has_seen_dot = false;
        loop {
            match self.peek() {
                Some('.') if !has_seen_dot => {
                    buffer.push('.');
                    has_seen_dot = true;
                },
                Some('.') if has_seen_dot => {
                    return self.illegal_token("invalid decimal literal".to_string());
                },
                Some(c) if c.is_digit(10) => buffer.push(c),
                Some(c) if c.is_es_identifier_start() => {
                    // special case - e and E start an exponent.
                    if c == 'e' || c == 'E' {
                        break;
                    }

                    return self.illegal_token("invalid decimal literal".to_string());
                }
                _ => break
            }

            self.advance();
        }

        // at this point we've scanned all of the decimals we can.
        // we need to scan an exponent, if it exists.
        // for a token like 2583.592e+10, the lexer head is
        //                          ^--- here.
        match self.peek() {
            Some('e') | Some('E') => {
                buffer.push('e');
                // from here, we have to scan a signed integer
                self.advance();
                match self.peek() {
                    Some('+') => {
                        buffer.push('+');
                        self.advance();
                    }
                    Some('-') => {
                        buffer.push('-');
                        self.advance();
                    },
                    _ => {}
                }

                let previous_len = buffer.len();

                loop {
                    match self.peek() {
                        Some(c) if c.is_digit(10) => buffer.push(c),
                        Some(c) if c.is_es_identifier_start() => {
                            return self.illegal_token("invalid decimal literal".to_string());
                        },
                        _ if previous_len == buffer.len() => {
                            return self.illegal_token("invalid decimal literal".to_string());
                        },
                        _ => break
                    }

                    self.advance();
                }
            },
            _ => {}
        }

        self.new_token_of_length(TokenKind::DecimalLiteral(buffer.clone()), buffer.len() as u32)
    }

    fn octal_literal(&mut self) -> Token {
        // SPEC_NOTE: v8 and jsc consider an octal literal
        // with non-octal digits to be valid and falls
        // back to a decimal interpretation.
        // The spec is not clear on this, but I'll follow
        // the lead of v8 and jsc.

        // for a literal like 0777, the lexer head is positioned
        //                     ^-- here.
        let mut still_in_octal_mode = true;
        let mut buffer = String::new();
        buffer.push('0');
        loop {
            match self.peek() {
                Some(c) if c.is_digit(8) => buffer.push(c),
                Some(c) if c.is_digit(10) => {
                    still_in_octal_mode = false;
                    buffer.push(c);
                }
                Some(c) if c.is_es_identifier_start() => {
                    return self.illegal_token("invalid octal literal".to_string());
                },
                _ => break
            }

            self.advance();
        }

        if still_in_octal_mode {
            self.new_token_of_length(TokenKind::OctalIntegerLiteral(buffer.clone()), buffer.len() as u32)
        } else {
            self.new_token_of_length(TokenKind::DecimalLiteral(buffer.clone()), buffer.len() as u32)
        }
    }

    fn string_literal(&mut self, started_with_dbl_quote: bool) -> Token {
        // the lexer is pointing at either a " or '. Either way, consume it.
        self.advance();
        let mut buffer = String::new();
        loop {
            match self.peek() {
                Some('"') if started_with_dbl_quote => break,
                Some('\'') if !started_with_dbl_quote => break,
                Some('\\') => if let Err(e) = self.string_literal_escape_sequence(&mut buffer) {
                    return self.illegal_token(e);
                },
                Some(c) => {
                    self.advance();
                    buffer.push(c)
                },
                _ => return self.illegal_token("unterminated string literal".to_string())
            }
        }

        // we are looking at a " or ' again, whatever is appropriate.
        self.advance();
        self.new_token_of_length(TokenKind::StringLiteral(buffer.clone()), buffer.len() as u32 + 2)
    }

    fn string_literal_escape_sequence(&mut self,
                                      buffer: &mut String)
                                      -> Result<(), String> {
        // we're looking at a \.
        self.advance();
        match self.peek() {
            Some('\'') => {
                self.advance();
                buffer.push('\'');
            },
            Some('"') => {
                self.advance();
                buffer.push('"');
            }
            Some('\\') => {
                self.advance();
                buffer.push('\\');
            },
            Some('b') => {
                self.advance();
                buffer.push('\u{0008}');
            },
            Some('t') => {
                self.advance();
                buffer.push('\u{0009}');
            },
            Some('n') => {
                self.advance();
                buffer.push('\n');
            },
            Some('v') => {
                self.advance();
                buffer.push('\u{000B}');
            },
            Some('f') => {
                self.advance();
                buffer.push('\u{000C}');
            },
            Some('r') => {
                self.advance();
                buffer.push('\u{000D}');
            }
            Some('x') => try!(self.string_hex_escape(buffer)),
            Some('u') => try!(self.string_unicode_escape(buffer)),
            Some('0') => {
                // SPEC_NOTE: the spec says that this production is only valid
                // if the character following this one is not a digit. Strings such as
                // '\0123' result in implementation-defined behavior. Both v8 and jsc turn \0123 into
                // an octal escape ('\n3'). The spec does not say whether or not an implementation can allow
                // for octal literals, but it does specifically disallow it for strict mode code.
                // TODO: i'm leaving this undefined for now.
                self.advance();
                buffer.push('\0');
            },
            Some(c) if c.is_es_line_terminator() => {
                self.eat_line_terminator();
                buffer.push(c);
            },
            Some(c) => {
                self.advance();
                buffer.push(c);
            }
            _ => return Err("unterminated string literal".to_string())
        }

        Ok(())
    }

    fn string_hex_escape(&mut self, buffer: &mut String) -> Result<(), String> {
        // we're looking at an x and need to scan two hex digits.
        self.advance();
        let mut buf = Vec::new();
        for _ in 0..2 {
            match self.peek() {
                Some(c) if c.is_digit(16) => buf.push(c),
                _ => return Err("invalid escape sequence".to_owned())
            }

            self.advance();
        }

        // this is obvious, but just to make sure...
        debug_assert!(buf.len() == 2);
        // quick 2-digit hex to decimal conversion
        let value = (16 * buf[0].to_digit(16).unwrap()) + buf[1].to_digit(16).unwrap();

        let actual_char = if let Some(v) = char::from_u32(value) {
            v
        } else {
            return Err("invalid escape sequence".to_owned());
        };

        buffer.push(actual_char);
        Ok(())
    }

    fn string_unicode_escape(&mut self, buffer: &mut String) -> Result<(), String> {
        // we're looking at a u and need to scan four hex digits.
        self.advance();
        let mut buf = Vec::new();
        for _ in 0..4 {
            match self.peek() {
                Some(c) if c.is_digit(16) => buf.push(c),
                _ => return Err("invalid escape sequence".to_owned())
            }

            self.advance();
        }

        // again, should be obvious...
        debug_assert!(buf.len() == 4);
        // quick 4-digit hex to decimal conversion
        let value =
              (4096 * buf[0].to_digit(16).unwrap())
            + (256  * buf[1].to_digit(16).unwrap())
            + (16   * buf[2].to_digit(16).unwrap())
            + (       buf[3].to_digit(16).unwrap());

        let actual_char = if let Some(v) = char::from_u32(value) {
            v
        } else {
            return Err("invalid escape sequence".to_owned());
        };

        buffer.push(actual_char);
        Ok(())
    }

    fn next_token_inner(&mut self, leading_div: bool) -> Option<Token> {
        // top level of the state machine
        // we need to wind forward to the next non-whitespace and non-comment token.

        self.prev_token_was_newline = false;
        loop {
            self.whitespace();

            // so we are pointed at something that's not whitespace.
            // 1) is the next character a /?
            if self.peek() == Some('/') {
                self.advance();
                // 2) is the next character a * or /?
                if self.peek() == Some('*') || self.peek() == Some('/') {
                    // 2.5) if so, were scanning a comment.
                    // in a comment like "// this is a comment", the lexer head is pointed at
                    //                     ^------ here.
                    // this is so comment() can decide whether or not this is a block comment.
                    if let Err(e) = self.comment() {
                        // comment returns an Err whenever it encounters an unterminated block comment.
                        return Some(self.illegal_token(e));
                    }
                    // 3) continue scanning. the lexer will be pointed at the next character
                    // that is not part of the comment.
                    // note that the lexer may now be pointed at whitespace, so we continue the loop.
                    continue;
                } else {
                    // otherwise - back off, point the lexer head back at /,
                    // and enter the main state machine.
                    self.unadvance('/');
                    break;
                }
            }

            // 1.5) if it's not, enter the main state machine.
            break;
        }


        // enter the mains tate machine. the "leading_div" parameter influences
        // that the state machine does when it sees a /. If true, the state machine
        // starts a regex literal, while if false, it starts either a div(/)
        // or divequal (/=) token.
        match self.peek() {
            Some('/') if !leading_div => Some(self.div_or_div_equal()),
            Some('/') if leading_div => Some(self.regex()),
            Some('+') => Some(self.plus_or_plus_equal()),
            Some('-') => Some(self.minus_or_minus_equal()),
            Some('*') => Some(self.star_or_star_equal()),
            Some('<') => Some(self.right_cmp_or_shift()),
            Some('>') => Some(self.left_cmp_or_shift()),
            Some('=') => Some(self.eq_cmp_or_assign()),
            Some('!') => Some(self.not_or_not_eq()),
            Some('&') => Some(self.and_or_bitwise_and()),
            Some('|') => Some(self.or_or_bitwise_or()),
            Some('^') => Some(self.xor_or_assign()),
            Some('%') => Some(self.percent_or_equals()),
            Some('.') => Some(self.dot_or_decimal_literal()),
            Some('{') => self.yield_single_char_token(TokenKind::LeftBrace),
            Some('}') => self.yield_single_char_token(TokenKind::RightBrace),
            Some('(') => self.yield_single_char_token(TokenKind::LeftParen),
            Some(')') => self.yield_single_char_token(TokenKind::RightParen),
            Some('[') => self.yield_single_char_token(TokenKind::LeftBracket),
            Some(']') => self.yield_single_char_token(TokenKind::RightBracket),
            Some(';') => self.yield_single_char_token(TokenKind::Semicolon),
            Some(',') => self.yield_single_char_token(TokenKind::Comma),
            Some('~') => self.yield_single_char_token(TokenKind::BitwiseNot),
            Some('?') => self.yield_single_char_token(TokenKind::QuestionMark),
            Some(':') => self.yield_single_char_token(TokenKind::Colon),
            Some('"') => Some(self.string_literal(true)),
            Some('\'') => Some(self.string_literal(false)),
            Some(c) if c.is_es_identifier_start() => Some(self.identifier()),
            Some(c) if c.is_digit(10) => Some(self.numeric_literal()),
            Some(c) => Some(self.illegal_token(format!("unexpected char: {}", c))),
            None => None
        }
    }

    pub fn next_token_leading_div(&mut self) -> Option<Token> {
        self.next_token_inner(true)
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.next_token_inner(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn single_token_test(string: &'static str, token: TokenKind, leading_div: bool) {
        let mut lexer = Lexer::new(string.chars());
        let found_token = if !leading_div {
            lexer.next_token()
        } else {
            lexer.next_token_leading_div()
        };

        match found_token {
            Some(Token { kind, span, .. } ) => {
                assert_eq!(kind, token);
                assert_eq!(span.start.col, 0);
                assert_eq!(span.start.line, 0);
                assert_eq!(span.stop.col, (string.len() as u32) -1);
                assert_eq!(span.stop.line, 0);
            }
            _ => panic!("unexpected token value: {:?}, expected: {:?}", found_token, token)
        }
    }

    macro_rules! single_token_test {
        ($name:ident, $token:expr => $value:expr) => {
            #[test]
            fn $name() {
                single_token_test($token, $value, false);
            }
        };
    }

    macro_rules! single_token_test_leading_div {
        ($name:ident, $token:expr => $value:expr) => {
            #[test]
            fn $name() {
                single_token_test($token, $value, true);
            }
        };
    }


    single_token_test!(test_identifier, "hello" => TokenKind::Identifier("hello".to_string()));
    single_token_test!(test_boolean_true, "true" => TokenKind::BooleanLiteral(true));
    single_token_test!(test_boolean_false, "false" => TokenKind::BooleanLiteral(false));
    single_token_test!(test_null_literal, "null" => TokenKind::NullLiteral);
    single_token_test!(test_string_literal, "\"string literal\"" => TokenKind::StringLiteral("string literal".to_string()));
    single_token_test_leading_div!(test_regex_literal, "/asdf/g" => TokenKind::RegularExpressionLiteral("asdf".to_string(), "g".to_string()));

    single_token_test!(test_break, "break" => TokenKind::Break);
    single_token_test!(test_do, "do" => TokenKind::Do);
    single_token_test!(test_instanceof, "instanceof" => TokenKind::InstanceOf);
    single_token_test!(test_typeof, "typeof" => TokenKind::TypeOf);
    single_token_test!(test_case, "case" => TokenKind::Case);
    single_token_test!(test_else, "else" => TokenKind::Else);
    single_token_test!(test_new, "new" => TokenKind::New);
    single_token_test!(test_var, "var" => TokenKind::Var);
    single_token_test!(test_catch, "catch" => TokenKind::Catch);
    single_token_test!(test_finally, "finally" => TokenKind::Finally);
    single_token_test!(test_return, "return" => TokenKind::Return);
    single_token_test!(test_void, "void" => TokenKind::Void);
    single_token_test!(test_continue, "continue" => TokenKind::Continue);
    single_token_test!(test_switch, "switch" => TokenKind::Switch);
    single_token_test!(test_while, "while" => TokenKind::While);
    single_token_test!(test_debugger, "debugger" => TokenKind::Debugger);
    single_token_test!(test_function, "function" => TokenKind::Function);
    single_token_test!(test_this, "this" => TokenKind::This);
    single_token_test!(test_with, "with" => TokenKind::With);
    single_token_test!(test_default, "default" => TokenKind::Default);
    single_token_test!(test_if, "if" => TokenKind::If);
    single_token_test!(test_throw, "throw" => TokenKind::Throw);
    single_token_test!(test_delete, "delete" => TokenKind::Delete);
    single_token_test!(test_in, "in" => TokenKind::In);
    single_token_test!(test_try, "try" => TokenKind::Try);

    single_token_test!(test_class, "class" => TokenKind::Class);
    single_token_test!(test_enum, "enum" => TokenKind::Enum);
    single_token_test!(test_extends, "extends" => TokenKind::Extends);
    single_token_test!(test_super, "super" => TokenKind::Super);
    single_token_test!(test_const, "const" => TokenKind::Const);
    single_token_test!(test_export, "export" => TokenKind::Export);
    single_token_test!(test_import, "import" => TokenKind::Import);

    single_token_test!(test_implements, "implements" => TokenKind::FutureReservedWordStrict("implements"));
    single_token_test!(test_let, "let" => TokenKind::FutureReservedWordStrict("let"));
    single_token_test!(test_private, "private" => TokenKind::FutureReservedWordStrict("private"));
    single_token_test!(test_public, "public" => TokenKind::FutureReservedWordStrict("public"));
    single_token_test!(test_yield, "yield" => TokenKind::FutureReservedWordStrict("yield"));
    single_token_test!(test_interface, "interface" => TokenKind::FutureReservedWordStrict("interface"));
    single_token_test!(test_package, "package" => TokenKind::FutureReservedWordStrict("package"));
    single_token_test!(test_protected, "protected" => TokenKind::FutureReservedWordStrict("protected"));
    single_token_test!(test_static, "static" => TokenKind::FutureReservedWordStrict("static"));

    single_token_test!(test_left_brace, "{" => TokenKind::LeftBrace);
    single_token_test!(test_right_brace, "}" => TokenKind::RightBrace);
    single_token_test!(test_left_paren, "(" => TokenKind::LeftParen);
    single_token_test!(test_right_paren, ")" => TokenKind::RightParen);
    single_token_test!(test_left_bracket, "[" => TokenKind::LeftBracket);
    single_token_test!(test_right_bracket, "]" => TokenKind::RightBracket);
    single_token_test!(test_dot, "." => TokenKind::Dot);
    single_token_test!(test_semicolon, ";" => TokenKind::Semicolon);
    single_token_test!(test_comma, "," => TokenKind::Comma);
    single_token_test!(test_less_than, "<" => TokenKind::LessThan);
    single_token_test!(test_greater_than, ">" => TokenKind::GreaterThan);
    single_token_test!(test_less_than_equal, "<=" => TokenKind::LessThanEqual);
    single_token_test!(test_greater_than_equal, ">=" => TokenKind::GreaterThanEqual);
    single_token_test!(test_double_equal, "==" => TokenKind::DoubleEqual);
    single_token_test!(test_double_not_equal, "!=" => TokenKind::DoubleNotEqual);
    single_token_test!(test_triple_equal, "===" => TokenKind::TripleEqual);
    single_token_test!(test_triple_not_equal, "!==" => TokenKind::TripleNotEqual);
    single_token_test!(test_plus, "+" => TokenKind::Plus);
    single_token_test!(test_minus, "-" => TokenKind::Minus);
    single_token_test!(test_star, "*" => TokenKind::Star);
    single_token_test!(test_percent, "%" => TokenKind::Percent);
    single_token_test!(test_double_plus, "++" => TokenKind::DoublePlus);
    single_token_test!(test_double_minus, "--" => TokenKind::DoubleMinus);
    single_token_test!(test_left_shift, "<<" => TokenKind::LeftShift);
    single_token_test!(test_right_shift, ">>" => TokenKind::RightShift);
    single_token_test!(test_triple_right_shift, ">>>" => TokenKind::TripleRightShift);
    single_token_test!(test_bitwise_and, "&" => TokenKind::BitwiseAnd);
    single_token_test!(test_bitwise_or, "|" => TokenKind::BitwiseOr);
    single_token_test!(test_bitwise_xor, "^" => TokenKind::BitwiseXor);
    single_token_test!(test_bitwise_not, "~" => TokenKind::BitwiseNot);
    single_token_test!(test_logical_and, "&&" => TokenKind::LogicalAnd);
    single_token_test!(test_logical_or, "||" => TokenKind::LogicalOr);
    single_token_test!(test_logical_not, "!" => TokenKind::LogicalNot);
    single_token_test!(test_question_mark, "?" => TokenKind::QuestionMark);
    single_token_test!(test_colon, ":" => TokenKind::Colon);
    single_token_test!(test_equal, "=" => TokenKind::Equal);
    single_token_test!(test_plus_equal, "+=" => TokenKind::PlusEqual);
    single_token_test!(test_minus_equal, "-=" => TokenKind::MinusEqual);
    single_token_test!(test_star_equal, "*=" => TokenKind::StarEqual);
    single_token_test!(test_percent_equal, "%=" => TokenKind::PercentEqual);
    single_token_test!(test_left_shift_equal, "<<=" => TokenKind::LeftShiftEqual);
    single_token_test!(test_right_shift_equal, ">>=" => TokenKind::RightShiftEqual);
    single_token_test!(test_triple_right_shift_equal, ">>>=" => TokenKind::TripleRightShiftEqual);
    single_token_test!(test_bitwise_and_equal, "&=" => TokenKind::BitwiseAndEqual);
    single_token_test!(test_bitwise_or_equal, "|=" => TokenKind::BitwiseOrEqual);
    single_token_test!(test_bitwise_xor_equal, "^=" => TokenKind::BitwiseXorEqual);

    #[test]
    fn test_whitespace() {
        let mut lexer = Lexer::new("a b".chars());
        assert_eq!(TokenKind::Identifier("a".to_string()),
                   lexer.next_token().unwrap().kind);
        assert_eq!(TokenKind::Identifier("b".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_line_terminator() {
        let mut lexer = Lexer::new("a\nb".chars());
        assert_eq!(TokenKind::Identifier("a".to_string()),
                   lexer.next_token().unwrap().kind);
        let tok = lexer.next_token().unwrap();
        assert_eq!(TokenKind::Identifier("b".to_string()),
                   tok.kind);
        assert!(tok.preceded_by_newline);

    }

    #[test]
    fn test_comment() {
        let mut lexer = Lexer::new("before // this is a comment\nafter".chars());
        assert_eq!(TokenKind::Identifier("before".to_string()),
                   lexer.next_token().unwrap().kind);
        assert_eq!(TokenKind::Identifier("after".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_block_comment() {
        let mut lexer = Lexer::new("before /* this is a block comment */ after".chars());
        assert_eq!(TokenKind::Identifier("before".to_string()),
                   lexer.next_token().unwrap().kind);
        assert_eq!(TokenKind::Identifier("after".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_block_comment_with_line_terminator() {
        let mut lexer = Lexer::new("before /* this is a\nmultiline comment */ after".chars());
        assert_eq!(TokenKind::Identifier("before".to_string()),
                   lexer.next_token().unwrap().kind);
        let tok = lexer.next_token().unwrap();
        assert_eq!(TokenKind::Identifier("after".to_string()),
                   tok.kind);
        assert!(tok.preceded_by_newline);
    }

    #[test]
    fn test_unterminated_block_comment() {
        let mut lexer = Lexer::new("/*".chars());
        assert!(lexer.next_token().unwrap().is_illegal());
    }

    #[test]
    fn test_block_comments_do_not_nest() {
        let mut lexer = Lexer::new("/* /* */ stuff".chars());
        assert_eq!(TokenKind::Identifier("stuff".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_decimal_literal() {
        let mut lexer = Lexer::new("42".chars());
        assert_eq!(TokenKind::DecimalLiteral("42".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_hex_literal() {
        let mut lexer = Lexer::new("0xDEADBEEF".chars());
        assert_eq!(TokenKind::HexIntegerLiteral("0xDEADBEEF".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_octal_literal() {
        let mut lexer = Lexer::new("0777".chars());
        assert_eq!(TokenKind::OctalIntegerLiteral("0777".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_invalid_octal_literal() {
        let mut lexer = Lexer::new("0778".chars());
        assert_eq!(TokenKind::DecimalLiteral("0778".to_string()),
                   lexer.next_token().unwrap().kind);
    }


    #[test]
    fn test_decimal_leading_dot_literal() {
        let mut lexer = Lexer::new(".99".chars());
        assert_eq!(TokenKind::DecimalLiteral(".99".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_decimal_leading_zero_literal() {
        let mut lexer = Lexer::new("0.99".chars());
        assert_eq!(TokenKind::DecimalLiteral("0.99".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_decimal_invalid_terminator() {
        let mut lexer = Lexer::new("599q".chars());
        assert!(lexer.next_token().unwrap().is_illegal());
    }

    #[test]
    fn test_decimal_invalid_terminator_2() {
        let mut lexer = Lexer::new("0.5e+f".chars());
        assert!(lexer.next_token().unwrap().is_illegal());
    }

    #[test]
    fn test_decimal_invalid_terminator_3() {
        let mut lexer = Lexer::new("0.a".chars());
        assert!(lexer.next_token().unwrap().is_illegal());
    }

    #[test]
    fn test_decimal_positive_exponent() {
        let mut lexer = Lexer::new("10e+20".chars());
        assert_eq!(TokenKind::DecimalLiteral("10e+20".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_decimal_negative_exponent() {
        let mut lexer = Lexer::new("10e-20".chars());
        assert_eq!(TokenKind::DecimalLiteral("10e-20".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_decimal_invalid_exponent() {
        let mut lexer = Lexer::new("10e".chars());
        assert!(lexer.next_token().unwrap().is_illegal());
    }

    #[test]
    fn test_regex_no_flags() {
        let mut lexer = Lexer::new("/asdf/".chars());
        assert_eq!(TokenKind::RegularExpressionLiteral("asdf".to_string(), "".to_string()),
                   lexer.next_token_leading_div().unwrap().kind);
    }

    #[test]
    fn test_regex_char_class() {
        let mut lexer = Lexer::new("/[a-zA-Z]/".chars());
        assert_eq!(TokenKind::RegularExpressionLiteral("[a-zA-Z]".to_string(), "".to_string()),
                   lexer.next_token_leading_div().unwrap().kind);
    }

    #[test]
    fn test_regex_escape_sequence() {
        let mut lexer = Lexer::new(r#"/\+/"#.chars());
        assert_eq!(TokenKind::RegularExpressionLiteral(r#"\+"#.to_string(), "".to_string()),
                   lexer.next_token_leading_div().unwrap().kind);
    }

    #[test]
    fn test_regex_unterminated() {
        let mut lexer = Lexer::new("/asdf".chars());
        assert!(lexer.next_token_leading_div().unwrap().is_illegal());
    }

    #[test]
    fn test_string_hex_literal() {
        let mut lexer = Lexer::new("\"\\xFF\"".chars());
        assert_eq!(TokenKind::StringLiteral("ÿ".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_string_unicode_literal() {
        let mut lexer = Lexer::new("\"\\u2603\"".chars());
        assert_eq!(TokenKind::StringLiteral("☃".to_string()),
                   lexer.next_token().unwrap().kind);
    }

    #[test]
    fn test_string_regular_escapel() {
        let mut lexer = Lexer::new("\"\\XFF\"".chars());
        assert_eq!(TokenKind::StringLiteral("XFF".to_string()),
                   lexer.next_token().unwrap().kind);
    }
}
