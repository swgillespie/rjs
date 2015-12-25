use super::string_interer::InternedString;
use std::default::Default;

#[derive(Clone, PartialEq, Debug)]
pub struct Program {
    is_strict: bool,
    statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            is_strict: false,
            statements: vec![],
        }
    }

    pub fn add_statement(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }

    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }

    pub fn is_strict(&self) -> bool {
        self.is_strict
    }

    pub fn set_is_strict(&mut self, value: bool) {
        self.is_strict = value;
    }
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Function {
    uses_with: bool,
    uses_arguments_identifier: bool,
    name: Option<InternedString>,
    parameters: Vec<InternedString>,
    is_strict: bool,
    body: Vec<Statement>,
}

impl Function {
    pub fn new() -> Function {
        Default::default()
    }

    pub fn uses_with(&self) -> bool {
        self.uses_with
    }

    pub fn set_uses_with(&mut self, value: bool) {
        self.uses_with = value;
    }

    pub fn uses_arguments_identifier(&self) -> bool {
        self.uses_arguments_identifier
    }

    pub fn set_uses_arguments_identifier(&mut self, value: bool) {
        self.uses_arguments_identifier = value;
    }

    pub fn is_strict(&self) -> bool {
        self.is_strict
    }

    pub fn set_is_strict(&mut self, value: bool) {
        self.is_strict = value;
    }

    pub fn name(&self) -> Option<InternedString> {
        self.name
    }

    pub fn set_name(&mut self, name: InternedString) {
        self.name = Some(name);
    }

    pub fn body(&self) -> &[Statement] {
        &self.body[..]
    }

    pub fn add_statement(&mut self, stmt: Statement) {
        self.body.push(stmt);
    }

    pub fn parameters(&self) -> &[InternedString] {
        &self.parameters[..]
    }

    pub fn add_parameter(&mut self, param: InternedString) {
        self.parameters.push(param);
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    Expression(Expression),
    Block(Vec<Statement>),
    Empty,
    Debugger,
    With(Expression, Box<Statement>),
    Return(Option<Expression>),
    Label(InternedString, Box<Statement>),
    Break(Option<InternedString>),
    Continue(Option<InternedString>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Switch(Expression, Vec<SwitchCase>),
    Throw(Expression),
    Try(Box<Statement>, Option<CatchClause>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    DoWhile(Expression, Box<Statement>),
    ForIn(ForInit, Expression, Box<Statement>),
    Declaration(Declaration),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    This,
    Array(Vec<Option<Expression>>),
    Object(Vec<Property>),
    Function(Box<Function>),
    Unary(UnaryOperator, bool, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    ReferenceAssignment(InternedString, Box<Expression>),
    LValueAssignment(Box<Expression>, Box<Expression>),
    Logical(LogicalOperator, Box<Expression>, Box<Expression>),
    IdentifierMember(Box<Expression>, InternedString),
    CalculatedMember(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    New(Box<Expression>, Vec<Expression>),
    Sequence(Vec<Statement>, Box<Expression>),
    Identifier(InternedString),
    Literal(Literal),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Declaration {
    Function(Function),
    Variable(Vec<VariableDeclarator>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct VariableDeclarator {
    pub name: InternedString,
    pub initial_value: Option<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Property {
    pub key: InternedString,
    pub value: Box<Expression>,
    pub kind: PropertyKind,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UnaryOperator {
    Minus,
    Plus,
    LogicalNot,
    BitwiseNot,
    Typeof,
    Void,
    Delete,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UpdateOperator {
    Increment,
    Decrement,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum BinaryOperator {
    Equal,
    NotEqual,
    StrictEqual,
    StrictNotEqual,
    GreaterThan,
    GreaterThanEq,
    LessThan,
    LessThanEq,
    LeftShift,
    RightShift,
    TripleRightShift,
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    In,
    Instanceof,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum LogicalOperator {
    Or,
    And,
}

#[derive(Clone, PartialEq, Debug)]
pub struct SwitchCase {
    pub test: Option<Expression>,
    pub body: Vec<Statement>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum ForInit {
    VarDec(Declaration),
    Expr(Expression),
}

#[derive(Clone, PartialEq, Debug)]
pub struct CatchClause {
    pub param: InternedString,
    pub body: Box<Statement>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    String(InternedString),
    Boolean(bool),
    Null,
    Numeric(f64),
    RegExp(InternedString, InternedString),
}
