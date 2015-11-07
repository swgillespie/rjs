use lexer::Span;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Spanned<T: Debug> {
    pub data: T,
    pub span: Span
}

impl<T: Clone + Debug> Clone for Spanned<T> {
    fn clone(&self) -> Spanned<T> {
        Spanned {
            data: self.data.clone(),
            span: self.span.clone()
        }
    }
}

impl<T: PartialEq + Debug> PartialEq for Spanned<T> {
    fn eq(&self, other: &Spanned<T>) -> bool {
        self.span.eq(&other.span) && self.data.eq(&other.data)
    }
}

impl<T: Debug> Spanned<T> {
    pub fn new(span: Span, data: T) -> Spanned<T> {
        Spanned {
            span: span,
            data: data
        }
    }
}

pub type Identifier = Spanned<String>;

pub type SpannedLiteral = Spanned<Literal>;

#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    String(String),
    Boolean(bool),
    Null,
    Numeric(f64),
    RegExp(String, String)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Program {
    pub directive_prologue: Vec<SpannedStatement>,
    pub statements: Vec<SpannedStatement>
}

pub type SpannedFunction = Spanned<Function>;

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub name: Option<Identifier>,
    pub parameters: Vec<SpannedPattern>,
    pub prologue: Vec<SpannedStatement>,
    pub body: Vec<SpannedStatement>
}

pub type SpannedStatement = Spanned<Statement>;

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    Expression(SpannedExpression),
    Block(Vec<SpannedStatement>),
    Empty,
    Debugger,
    With(SpannedExpression, Box<SpannedStatement>),
    Return(Option<SpannedExpression>),
    Label(Identifier, Box<SpannedStatement>),
    Break(Option<Identifier>),
    Continue(Option<Identifier>),
    If(SpannedExpression, Box<SpannedStatement>, Option<Box<SpannedStatement>>),
    Switch(SpannedExpression, Vec<SwitchCase>),
    Throw(SpannedExpression),
    Try(Box<SpannedStatement>, Option<CatchClause>, Option<Box<SpannedStatement>>),
    While(SpannedExpression, Box<SpannedStatement>),
    DoWhile(SpannedExpression, Box<SpannedStatement>),
    For(Option<ForInit>, Option<SpannedExpression>, Option<SpannedExpression>, Box<SpannedStatement>),
    ForIn(ForInit, SpannedExpression, Box<SpannedStatement>),
    Declaration(Declaration)
}

#[derive(Clone, PartialEq, Debug)]
pub struct SwitchCase {
    pub test: Option<SpannedExpression>,
    pub body: Vec<SpannedStatement>
}

#[derive(Clone, PartialEq, Debug)]
pub enum ForInit {
    VarDec(SpannedDeclaration),
    Expr(SpannedExpression),
    None
}

#[derive(Clone, PartialEq, Debug)]
pub struct CatchClause {
    pub param: SpannedPattern,
    pub body: Box<SpannedStatement>
}

pub type SpannedDeclaration = Spanned<Declaration>;
#[derive(Clone, PartialEq, Debug)]
pub enum Declaration {
    Function(Function),
    Variable(Vec<VariableDeclarator>)
}

#[derive(Clone, PartialEq, Debug)]
pub struct VariableDeclarator {
    pub id: SpannedPattern,
    pub initial_value: Option<SpannedExpression>
}

pub type SpannedExpression = Spanned<Expression>;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    This,
    Array(Vec<Option<SpannedExpression>>),
    Object(Vec<Property>),
    Function(Box<Function>),
    Unary(UnaryOperator, bool, Box<SpannedExpression>),
    Update(UpdateOperator, bool, Box<SpannedExpression>),
    Binary(BinaryOperator, Box<SpannedExpression>, Box<SpannedExpression>),
    Assignment(AssignmentOperator, PatternOrExpression, Box<SpannedExpression>),
    Logical(LogicalOperator, Box<SpannedExpression>, Box<SpannedExpression>),
    Member(Box<SpannedExpression>, Box<SpannedExpression>, bool),
    Conditional(Box<SpannedExpression>, Box<SpannedExpression>, Box<SpannedExpression>),
    Call(Box<SpannedExpression>, Vec<SpannedExpression>),
    New(Box<SpannedExpression>, Vec<SpannedExpression>),
    Sequence(Vec<SpannedExpression>),
    Identifier(Identifier),
    Literal(Literal)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Property {
    pub key: LiteralOrIdentifier,
    pub value: Box<SpannedExpression>,
    pub kind: PropertyKind
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum PropertyKind {
    Init,
    Get,
    Set
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UnaryOperator {
    Minus,
    Plus,
    LogicalNot,
    BitwiseNot,
    Typeof,
    Void,
    Delete
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UpdateOperator {
    Increment,
    Decrement
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
    Instanceof
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum AssignmentOperator {
    Equal,
    PlusEqual,
    MinusEqual,
    TimesEqual,
    DivEqual,
    ModEqual,
    LeftShiftEqual,
    RightShiftEqual,
    TripleRightShiftEqual,
    BitwiseOrEqual,
    BitwiseXorEqual,
    BitwiseAndEqual
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum LogicalOperator {
    Or,
    And
}

#[derive(Clone, PartialEq, Debug)]
pub enum LiteralOrIdentifier {
    Literal(SpannedLiteral),
    Identifier(Identifier)
}

pub type SpannedPattern = Spanned<Pattern>;

#[derive(Clone, PartialEq, Debug)]
pub enum Pattern {
    Identifier(Identifier)
}

#[derive(Clone, PartialEq, Debug)]
pub enum PatternOrExpression {
    Pattern(SpannedPattern),
    Expr(Box<SpannedExpression>)
}
