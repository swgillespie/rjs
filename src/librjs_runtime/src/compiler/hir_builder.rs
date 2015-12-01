//! This module provides a type responsible for "lowering" an ECMAScript syntax tree
//! into a *high-level intermediate representation*, or **HIR**, upon which all other
//! compiler passes will operate. The purpose of a high-level intermediate language is to
//! provide a high-level overview of a program's semantics in a way that is closely tied to
//! the language definition, but *desugared*.
//!
//! The objective of desugaring is to reduce the surface area upon which compiler passes have
//! to operate. Desugaring the AST into a HIR makes it much easier to write a bytecode lowerer,
//! since many syntactic constructs get eliminated when translated to HIR.
//!
//! ## Syntax Sugar in ECMAScript
//! ECMAScript is not a particularly saccharine language - most of its syntactic constructs
//! can't be reduced to anything more fundamental. However, there are several notable exceptions
//! that get lowered by this pass:
//!
//! 1. The ternary operator `?`, which desugars into a local + an `if` statement,
//! 2. C-style `for`-loops, which desugar into a `while` loop,
//! 3. Compound assignment operators (e.g. `+=`, `-=`), which desugar into a binary operation
//!    and an assignment.
//!
//! It is very likely that this pass will be able to desugar more and more things as time goes
//! on and this interpreter matures.

use librjs_syntax::ast;
use super::hir;
use super::string_interer::StringInterner;
use std::string::ToString;
use std::default::Default;

#[derive(Copy, Clone, Debug, Default)]
struct BuilderState {
    uses_with: bool,
    uses_arguments_ident: bool
}

impl BuilderState {
    pub fn new() -> BuilderState {
        Default::default()
    }

    pub fn reset(&mut self) {
        self.uses_arguments_ident = false;
        self.uses_with = false;
    }
}

/// Struct responsible for lowering an AST into HIR, a high-level
/// intermediate language that is transformed into bytecode.
pub struct HirBuilder<'a> {
    interner: &'a mut StringInterner,
    state: BuilderState
}

impl<'a> HirBuilder<'a> {
    /// Creates a new HirBuilder from a given string interner.
    pub fn new(interner: &'a mut StringInterner) -> HirBuilder {
        HirBuilder {
            interner: interner,
            state: BuilderState::new()
        }
    }

    /// Lowers a program into HIR.
    pub fn lower_program(&mut self, program: &ast::Program) -> hir::Program {
        let mut hir_program = hir::Program::new();
        for prolog_entry in &program.directive_prologue {
            if is_use_strict_expr(&prolog_entry.data) {
                hir_program.set_is_strict(true);
            }
        }

        for stmt in &program.statements {
            hir_program.add_statement(self.lower_statement(stmt));
        }

        hir_program
    }

    /// Lowers a statement into HIR.
    pub fn lower_statement(&mut self, stmt: &ast::SpannedStatement) -> hir::Statement {
        match stmt.data {
            ast::Statement::Expression(ref expr) => self.lower_expression_statement(expr),
            ast::Statement::Block(ref block) => self.lower_block(block),
            ast::Statement::Empty => self.lower_empty_statement(),
            ast::Statement::Debugger => self.lower_debugger_statement(),
            ast::Statement::With(ref expr, ref stmt) => self.lower_with_statement(expr, stmt),
            ast::Statement::Return(ref expr) => self.lower_return_statement(expr.as_ref()),
            ast::Statement::Label(ref label, ref statement) => self.lower_labelled_statement(label, statement),
            ast::Statement::Break(ref label) => self.lower_break_statement(label.as_ref()),
            ast::Statement::Continue(ref label) => self.lower_continue_statement(label.as_ref()),
            ast::Statement::If(ref cond, ref true_branch, ref false_branch) =>
                self.lower_if_statement(cond, true_branch, false_branch.as_ref().map(|x| &**x)),
            ast::Statement::Switch(ref cond, ref cases) => self.lower_switch_statement(cond, cases),
            ast::Statement::Throw(ref expr) => self.lower_throw_statement(expr),
            ast::Statement::Try(ref body, ref catch, ref finally) =>
                self.lower_try_statement(body, catch.as_ref(), finally.as_ref().map(|x| &**x)),
            ast::Statement::While(ref cond, ref body) => self.lower_while_statement(cond, body),
            ast::Statement::DoWhile(ref cond, ref body) => self.lower_do_while_statement(cond, body),
            ast::Statement::For(ref init, ref condition, ref update, ref body) =>
                self.lower_for_statement(init.as_ref(), condition.as_ref(), update.as_ref(), body),
            ast::Statement::ForIn(ref init, ref binding, ref body) =>
                self.lower_for_in_statement(init, binding, body),
            ast::Statement::Declaration(ref declaration) => self.lower_declaration_statement(declaration)
        }
    }

    fn lower_expression_statement(&mut self, expr: &ast::SpannedExpression) -> hir::Statement {
        let lowered_expr = self.lower_expression(expr);
        hir::Statement::Expression(lowered_expr)
    }

    fn lower_block(&mut self, block: &[ast::SpannedStatement]) -> hir::Statement {
        let lowered_stmts : Vec<_> = block.iter().map(|s| self.lower_statement(s)).collect();
        hir::Statement::Block(lowered_stmts)
    }

    fn lower_empty_statement(&mut self) -> hir::Statement {
        // nothing to do here, obviously.
        hir::Statement::Empty
    }

    fn lower_debugger_statement(&mut self) -> hir::Statement {
        // maybe one day we'll do something here.
        hir::Statement::Debugger
    }

    fn lower_with_statement(&mut self,
                            obj: &ast::SpannedExpression,
                            body: &ast::SpannedStatement) -> hir::Statement {
        self.state.uses_with = true;
        let lowered_obj = self.lower_expression(obj);
        let lowered_body = self.lower_statement(body);
        hir::Statement::With(lowered_obj, Box::new(lowered_body))
    }

    fn lower_return_statement(&mut self, expr: Option<&ast::SpannedExpression>) -> hir::Statement {
        let lowered = expr.map(|e| self.lower_expression(e));
        hir::Statement::Return(lowered)
    }

    fn lower_labelled_statement(&mut self, label: &ast::Identifier, stmt: &ast::SpannedStatement) -> hir::Statement {
        let interned_label = self.interner.intern(&label.data);
        let lowered_stmt = self.lower_statement(stmt);
        hir::Statement::Label(interned_label, Box::new(lowered_stmt))
    }

    fn lower_break_statement(&mut self, label: Option<&ast::Identifier>) -> hir::Statement {
        let interned_label = label.map(|s| self.interner.intern(&s.data));
        hir::Statement::Break(interned_label)
    }

    fn lower_continue_statement(&mut self, label: Option<&ast::Identifier>) -> hir::Statement {
        let interned_label = label.map(|s| self.interner.intern(&s.data));
        hir::Statement::Continue(interned_label)
    }

    fn lower_if_statement(&mut self,
                          condition: &ast::SpannedExpression,
                          true_branch: &ast::SpannedStatement,
                          false_branch: Option<&ast::SpannedStatement>)-> hir::Statement {
        let cond = self.lower_expression(condition);
        let true_branch = self.lower_statement(true_branch);
        let false_branch = false_branch.map(|f| Box::new(self.lower_statement(f)));
        hir::Statement::If(cond, Box::new(true_branch), false_branch)
    }

    fn lower_switch_statement(&mut self, cond: &ast::SpannedExpression, cases: &[ast::SwitchCase]) -> hir::Statement {
        // TODO this is a pretty nice optimization opportunity
        let cond = self.lower_expression(cond);
        let cases = cases.iter().map(|c| self.lower_switch_case(c)).collect();
        hir::Statement::Switch(cond, cases)
    }

    fn lower_switch_case(&mut self, case: &ast::SwitchCase) -> hir::SwitchCase {
        let test = case.test.as_ref()
            .map(|t| self.lower_expression(&t));
        let body = case.body.iter()
            .map(|b| self.lower_statement(b))
            .collect::<Vec<_>>();
        hir::SwitchCase {
            test: test,
            body: body
        }
    }

    fn lower_throw_statement(&mut self, expr: &ast::SpannedExpression) -> hir::Statement {
        let expr = self.lower_expression(expr);
        hir::Statement::Throw(expr)
    }

    fn lower_try_statement(&mut self,
                           body: &ast::SpannedStatement,
                           catch: Option<&ast::CatchClause>,
                           finally: Option<&ast::SpannedStatement>) -> hir::Statement {
        // TODO optimization. It would be nice to have a general HIR node for try/finally-like
        // constructs, since a with is basically a try-finally with extra scoping rules.
        let lowered_body = self.lower_statement(body);
        let lowered_catch = catch.map(|c| self.lower_catch_clause(c));
        let lowered_finally = finally.map(|f| Box::new(self.lower_statement(f)));
        hir::Statement::Try(Box::new(lowered_body), lowered_catch, lowered_finally)
    }

    fn lower_catch_clause(&mut self, catch: &ast::CatchClause) -> hir::CatchClause {
        // TODO(ES6) - pattern destructuring
        let ast::Pattern::Identifier(ref ident) = catch.param.data;
        self.check_name(&ident.data);
        let interned_param = self.interner.intern(&ident.data);
        let interned_body = self.lower_statement(&*catch.body);
        hir::CatchClause {
            param: interned_param,
            body: Box::new(interned_body)
        }
    }

    fn lower_while_statement(&mut self,
                             cond: &ast::SpannedExpression,
                             stmt: &ast::SpannedStatement) -> hir::Statement {
        // TODO while loops can't be desugared into do while without a generalized goto statement,
        // which ECMAScript does not have. Maybe we can add one to the HIR?
        let lowered_cond = self.lower_expression(cond);
        let lowered_body = self.lower_statement(stmt);
        hir::Statement::While(lowered_cond, Box::new(lowered_body))
    }

    fn lower_do_while_statement(&mut self,
                                cond: &ast::SpannedExpression,
                                stmt: &ast::SpannedStatement) -> hir::Statement {
        let lowered_cond = self.lower_expression(cond);
        let lowered_body = self.lower_statement(stmt);
        hir::Statement::DoWhile(lowered_cond, Box::new(lowered_body))
    }

    fn lower_for_statement(&mut self,
                          init: Option<&ast::ForInit>,
                          condition: Option<&ast::SpannedExpression>,
                          update: Option<&ast::SpannedExpression>,
                          body: &ast::SpannedStatement) -> hir::Statement {
        // the goal of this stage is to lower a for loop into a while loop, like this:
        // for(init, cond, update) body -> init; while(cond) { body; update }
        // we handle this on a case-by-case basis, since all three of init, cond, and update
        // are optional.
        //
        // This isn't generally legal in every language, since for introduces a new scope,
        // but it is in JS because any variables declared with var are visible anywhere in the
        // function.
        let lowered_for_init = init.map(|i| self.lower_for_init(i));
        let lowered_cond = if let Some(e) = condition {
            self.lower_expression(e)
        } else {
            // for(;;) becomes while(true)
            hir::Expression::Literal(hir::Literal::Boolean(true))
        };
        let lowered_update = update.map(|u| self.lower_expression(u));
        let lowered_body = self.lower_statement(body);
        let desugared_body = if let Some(update_stmt) = lowered_update {
            let mut new_body = vec![];
            new_body.push(lowered_body);
            new_body.push(hir::Statement::Expression(update_stmt));
            hir::Statement::Block(new_body)
        } else {
            lowered_body
        };

        let while_stmt = hir::Statement::While(lowered_cond, Box::new(desugared_body));

        if let Some(init_bindings) = lowered_for_init {
            let mut desugared_outer = vec![];
            desugared_outer.push(init_bindings);
            desugared_outer.push(while_stmt);
            hir::Statement::Block(desugared_outer)
        } else {
            while_stmt
        }
    }

    fn lower_for_init(&mut self, init: &ast::ForInit) -> hir::Statement {
        match *init {
            ast::ForInit::VarDec(ref decls) => hir::Statement::Declaration(self.lower_declaration(&decls.data)),
            ast::ForInit::Expr(ref expr) => self.lower_expression_statement(expr)
        }
    }

    fn lower_for_in_statement(&mut self,
                             init: &ast::ForInit,
                             binding: &ast::SpannedExpression,
                             body: &ast::SpannedStatement) -> hir::Statement {
        // TODO for-in statements are a little special, so we don't desugar them for now.
        // we should strive to eliminate ForIn from the HIR at some point.
        let lowered_init = match *init {
            // ForInit will be obsolete if we remove ForIn for the HIR
            ast::ForInit::Expr(ref expr) => hir::ForInit::Expr(self.lower_expression(expr)),
            ast::ForInit::VarDec(ref decl) => hir::ForInit::VarDec(self.lower_declaration(&decl.data))
        };

        let lowered_binding = self.lower_expression(binding);
        let lowered_body = self.lower_statement(body);
        hir::Statement::ForIn(lowered_init, lowered_binding, Box::new(lowered_body))
    }

    fn lower_declaration_statement(&mut self, decls: &ast::Declaration) -> hir::Statement {
        let decl = self.lower_declaration(decls);
        hir::Statement::Declaration(decl)
    }

    fn lower_declaration(&mut self,
                        decl: &ast::Declaration) -> hir::Declaration {
        match *decl {
            ast::Declaration::Function(ref func) => hir::Declaration::Function(self.lower_function(func)),
            ast::Declaration::Variable(ref vardec) => self.lower_variable_declarators(vardec)
        }
    }

    fn lower_function(&mut self, func: &ast::Function) -> hir::Function {
        let mut function = hir::Function::new();
        for prolog_entry in &func.prologue {
            if is_use_strict_expr(&prolog_entry.data) {
                function.set_is_strict(true);
            }
        }

        let old_state = self.state;
        self.state.reset();

        for stmt in &func.body {
            function.add_statement(self.lower_statement(stmt));
        }

        for param in &func.parameters {
            // TODO(ES6) - destructuring patterns
            let ast::Pattern::Identifier(ref ident) = param.data;
            self.check_name(&ident.data);
            let interned_param = self.interner.intern(&ident.data);
            function.add_parameter(interned_param);
        }

        function.set_uses_arguments_identifier(self.state.uses_arguments_ident);
        function.set_uses_with(self.state.uses_with);
        self.state = old_state;

        if let Some(ref name) = func.name {
            self.check_name(&name.data);
            let interned = self.interner.intern(&name.data);
            function.set_name(interned);
        }

        function
    }

    fn lower_variable_declarators(&mut self, decls: &[ast::VariableDeclarator]) -> hir::Declaration {
        let lowered_decls : Vec<_> = decls.iter()
            .map(|d| {
                let ast::Pattern::Identifier(ref ident) = d.id.data;
                self.check_name(&ident.data);
                let interned_name = self.interner.intern(&ident.data);
                let lowered_value = d.initial_value.as_ref().map(|s| self.lower_expression(s));
                hir::VariableDeclarator {
                    name: interned_name,
                    initial_value: lowered_value
                }
            })
            .collect();

        hir::Declaration::Variable(lowered_decls)
    }

    fn lower_expression(&mut self, expr: &ast::SpannedExpression) -> hir::Expression {
        match expr.data {
            ast::Expression::This => hir::Expression::This,
            ast::Expression::Array(ref values) => self.lower_array_literal(values),
            ast::Expression::Object(ref properties) => self.lower_object_literal(properties),
            ast::Expression::Function(ref function) => self.lower_function_expression(function),
            ast::Expression::Unary(op, prefix, ref expr) => self.lower_unary_op(op, prefix, expr),
            ast::Expression::Binary(op, ref left, ref right) => self.lower_binary_op(op, left, right),
            ast::Expression::Update(op, prefix, ref expr) => self.lower_update_op(op, prefix, expr),
            ast::Expression::Logical(op, ref left, ref right) => self.lower_logical_op(op, left, right),
            ast::Expression::Member(ref base, ref target, calculated) => self.lower_member_expression(base, target, calculated),
            ast::Expression::Assignment(op, ref target, ref value) => self.lower_assignment(op, target, value),
            ast::Expression::Conditional(ref cond, ref true_value, ref false_value) =>
                self.lower_conditional_expression(cond, true_value, false_value),
            ast::Expression::Call(ref base, ref args) =>
                self.lower_call_expression(base, args),
            ast::Expression::New(ref base, ref args) =>
                self.lower_new_expression(base, args),
            ast::Expression::Sequence(ref seq) => self.lower_sequence(seq),
            ast::Expression::Identifier(ref ident) => self.lower_identifier(ident),
            ast::Expression::Literal(ref lit) => self.lower_literal(lit)
        }
    }

    fn lower_function_expression(&mut self, func: &ast::Function) -> hir::Expression {
        let lowered_func = self.lower_function(func);
        hir::Expression::Function(Box::new(lowered_func))
    }

    fn lower_array_literal(&mut self,
                           values: &[Option<ast::SpannedExpression>]) -> hir::Expression {
        let lowered_elements : Vec<_> = values.iter()
            .map(|s| s.as_ref().map(|q| self.lower_expression(q)))
            .collect();
        hir::Expression::Array(lowered_elements)
    }

    fn lower_object_literal(&mut self,
                            properties: &[ast::Property]) -> hir::Expression {
        let lowered_properties : Vec<_> = properties.iter().map(|p| self.lower_property(p)).collect();
        hir::Expression::Object(lowered_properties)
    }

    fn lower_property(&mut self, prop: &ast::Property) -> hir::Property {
        let name = match prop.key {
            ast::LiteralOrIdentifier::Identifier(ref ident) => self.interner.intern(&ident.data),
            ast::LiteralOrIdentifier::Literal(ref lit) => {
                match lit.data {
                    ast::Literal::String(ref data) => self.interner.intern(data),
                    // SPEC_NOTE: null occupies a strange place in that it's both a reserved
                    // word and an identifier. The grammar does not specify whether or not null
                    // is a valid property name, but SpiderMonkey and v8 both accept it.
                    ast::Literal::Null => self.interner.intern("null"),
                    // TODO have to be careful here, this should be in sync with [[ToString]]
                    // pretty sure this will have edge case problems
                    ast::Literal::Numeric(data) => self.interner.intern(data.to_string()),
                    ref unknown => panic!("literal type and property name {:?} made it to hir lowering, \
                                           despite not being a legal property name", unknown)
                }
            }
        };

        let value = self.lower_expression(&prop.value);
        let kind = match prop.kind {
            ast::PropertyKind::Init => hir::PropertyKind::Init,
            ast::PropertyKind::Get => hir::PropertyKind::Get,
            ast::PropertyKind::Set => hir::PropertyKind::Set
        };

        hir::Property {
            key: name,
            value: Box::new(value),
            kind: kind
        }
    }

    fn lower_unary_op(&mut self,
                      op: ast::UnaryOperator,
                      prefix: bool,
                      expr: &ast::SpannedExpression) -> hir::Expression {
        let lowered_op = self.lower_unary_operator(op);
        let lowered_expr = self.lower_expression(expr);
        hir::Expression::Unary(lowered_op, prefix, Box::new(lowered_expr))
    }

    fn lower_unary_operator(&mut self, op: ast::UnaryOperator) -> hir::UnaryOperator {
        match op {
            ast::UnaryOperator::Minus => hir::UnaryOperator::Minus,
            ast::UnaryOperator::Plus => hir::UnaryOperator::Plus,
            ast::UnaryOperator::LogicalNot => hir::UnaryOperator::LogicalNot,
            ast::UnaryOperator::BitwiseNot => hir::UnaryOperator::BitwiseNot,
            ast::UnaryOperator::Typeof => hir::UnaryOperator::Typeof,
            ast::UnaryOperator::Void => hir::UnaryOperator::Void,
            ast::UnaryOperator::Delete => hir::UnaryOperator::Delete
        }
    }

    fn lower_binary_op(&mut self,
                       op: ast::BinaryOperator,
                       left: &ast::SpannedExpression,
                       right: &ast::SpannedExpression) -> hir::Expression {
        let lowered_op = self.lower_binary_operator(op);
        let lowered_left = self.lower_expression(left);
        let lowered_right = self.lower_expression(right);
        hir::Expression::Binary(lowered_op, Box::new(lowered_left), Box::new(lowered_right))
    }

    fn lower_binary_operator(&mut self, op: ast::BinaryOperator) -> hir::BinaryOperator {
        match op {
            ast::BinaryOperator::Equal => hir::BinaryOperator::Equal,
            ast::BinaryOperator::NotEqual => hir::BinaryOperator::NotEqual,
            ast::BinaryOperator::StrictEqual => hir::BinaryOperator::StrictEqual,
            ast::BinaryOperator::StrictNotEqual => hir::BinaryOperator::StrictNotEqual,
            ast::BinaryOperator::GreaterThan => hir::BinaryOperator::GreaterThan,
            ast::BinaryOperator::GreaterThanEq => hir::BinaryOperator::GreaterThanEq,
            ast::BinaryOperator::LessThan => hir::BinaryOperator::LessThan,
            ast::BinaryOperator::LessThanEq => hir::BinaryOperator::LessThanEq,
            ast::BinaryOperator::LeftShift => hir::BinaryOperator::LeftShift,
            ast::BinaryOperator::RightShift => hir::BinaryOperator::RightShift,
            ast::BinaryOperator::TripleRightShift => hir::BinaryOperator::TripleRightShift,
            ast::BinaryOperator::Plus => hir::BinaryOperator::Plus,
            ast::BinaryOperator::Minus => hir::BinaryOperator::Minus,
            ast::BinaryOperator::Times => hir::BinaryOperator::Times,
            ast::BinaryOperator::Div => hir::BinaryOperator::Div,
            ast::BinaryOperator::Mod => hir::BinaryOperator::Mod,
            ast::BinaryOperator::BitwiseOr => hir::BinaryOperator::BitwiseOr,
            ast::BinaryOperator::BitwiseXor => hir::BinaryOperator::BitwiseXor,
            ast::BinaryOperator::BitwiseAnd => hir::BinaryOperator::BitwiseAnd,
            ast::BinaryOperator::In => hir::BinaryOperator::In,
            ast::BinaryOperator::Instanceof => hir::BinaryOperator::Instanceof
        }
    }

    fn lower_logical_op(&mut self,
                       op: ast::LogicalOperator,
                       left: &ast::SpannedExpression,
                       right: &ast::SpannedExpression) -> hir::Expression {
        let lowered_op = self.lower_logical_operator(op);
        let lowered_left = self.lower_expression(left);
        let lowered_right = self.lower_expression(right);
        hir::Expression::Logical(lowered_op, Box::new(lowered_left), Box::new(lowered_right))
    }

    fn lower_logical_operator(&mut self, op: ast::LogicalOperator) -> hir::LogicalOperator {
        match op {
            ast::LogicalOperator::Or => hir::LogicalOperator::Or,
            ast::LogicalOperator::And => hir::LogicalOperator::And
        }
    }

    fn lower_update_op(&mut self,
                       op: ast::UpdateOperator,
                       prefix: bool,
                       expr: &ast::SpannedExpression) -> hir::Expression {
        let lowered_op = self.lower_update_operator(op);
        let lowered_expr = self.lower_expression(expr);
        hir::Expression::Update(lowered_op, prefix, Box::new(lowered_expr))
    }

    fn lower_update_operator(&mut self, op: ast::UpdateOperator) -> hir::UpdateOperator {
        match op {
            ast::UpdateOperator::Increment => hir::UpdateOperator::Increment,
            ast::UpdateOperator::Decrement => hir::UpdateOperator::Decrement
        }
    }

    fn lower_member_expression(&mut self,
                               base: &ast::SpannedExpression,
                               target: &ast::SpannedExpression,
                               calculated: bool) -> hir::Expression {
        let lowered_base = self.lower_expression(base);
        if calculated {
            // this is a member expression utilizing the index operator. The
            // index is not guaranteed to be a string so it's necessary for the compiler
            // to emit code to coerce the value into a string before doing a property
            // lookup.
            let lowered_target = self.lower_expression(target);
            hir::Expression::CalculatedMember(Box::new(lowered_base), Box::new(lowered_target))
        } else {
            // this is a member expression utilizing the dot operator. The index
            // is guaranteed to be an identifier.
            let ident = if let ast::Expression::Identifier(ref value) = target.data {
                self.interner.intern(&value.data)
            } else {
                panic!("parser emitted a non-calculated member expression with a non-identifier \
                        target node! AST node: {:?}", target);
            };

            hir::Expression::IdentifierMember(Box::new(lowered_base), ident)
        }
    }

    fn lower_assignment(&mut self,
                        op: ast::AssignmentOperator,
                        target: &ast::PatternOrExpression,
                        value: &ast::SpannedExpression) -> hir::Expression {
        // this stage desugars the many different assignment operators into an expression
        // that only uses the `=` operator.
        match *target {
            ast::PatternOrExpression::Pattern(ref pat) => {
                // TODO(ES6) - destructing assignments
                let ast::Pattern::Identifier(ref ident) = pat.data;
                // assignments where the LHS is an identifier get desugared to a different HIR node
                // than if the LHS is an expression.
                let interned_target = self.interner.intern(&ident.data);
                let lowered_value = self.lower_expression(value);

                if op == ast::AssignmentOperator::Equal {
                    // if this is a simple equals-assignment, we don't have to desugar anything.
                    return hir::Expression::ReferenceAssignment(interned_target, Box::new(lowered_value));
                }

                // if it's not, we have to desugar.
                // The basic approach is that we are turning something like
                //   <ident> *= 42
                // into
                //   <ident> = <ident> * 42
                let binop = self.assignment_op_to_binop(op);
                let new_value = hir::Expression::Binary(
                    binop,
                    Box::new(hir::Expression::Identifier(interned_target)),
                    Box::new(lowered_value)
                );

                hir::Expression::ReferenceAssignment(interned_target, Box::new(new_value))
            },
            ast::PatternOrExpression::Expr(ref expr) => {
                // even if the LHS is an expression, we employ the same sort of desugar strategy.
                let lowered_lhs = self.lower_expression(expr);
                let lowered_value = self.lower_expression(value);

                if op == ast::AssignmentOperator::Equal {
                    return hir::Expression::LValueAssignment(Box::new(lowered_lhs), Box::new(lowered_value));
                }

                let binop = self.assignment_op_to_binop(op);
                let new_value = hir::Expression::Binary(
                    binop,
                    Box::new(lowered_lhs.clone()),
                    Box::new(lowered_value)
                );

                hir::Expression::LValueAssignment(Box::new(lowered_lhs), Box::new(new_value))
            }
        }
    }

    fn assignment_op_to_binop(&mut self, op: ast::AssignmentOperator) -> hir::BinaryOperator {
        match op {
            ast::AssignmentOperator::Equal => panic!("called assignment_op_to_binop on Equal"),
            ast::AssignmentOperator::PlusEqual => hir::BinaryOperator::Plus,
            ast::AssignmentOperator::MinusEqual => hir::BinaryOperator::Minus,
            ast::AssignmentOperator::TimesEqual => hir::BinaryOperator::Times,
            ast::AssignmentOperator::DivEqual => hir::BinaryOperator::Div,
            ast::AssignmentOperator::ModEqual => hir::BinaryOperator::Mod,
            ast::AssignmentOperator::LeftShiftEqual => hir::BinaryOperator::LeftShift,
            ast::AssignmentOperator::RightShiftEqual => hir::BinaryOperator::RightShift,
            ast::AssignmentOperator::TripleRightShiftEqual => hir::BinaryOperator::TripleRightShift,
            ast::AssignmentOperator::BitwiseOrEqual => hir::BinaryOperator::BitwiseOr,
            ast::AssignmentOperator::BitwiseXorEqual => hir::BinaryOperator::BitwiseXor,
            ast::AssignmentOperator::BitwiseAndEqual => hir::BinaryOperator::BitwiseAnd
        }
    }

    fn lower_conditional_expression(&mut self,
                                    cond: &ast::SpannedExpression,
                                    true_value: &ast::SpannedExpression,
                                    false_value: &ast::SpannedExpression) -> hir::Expression {
        // desugar expression of the form "cond ? true_value : false_value" into
        // var (gensym);
        // if (cond) { (gensym) = true_value } else { (gensym) = false_value }
        let local = self.interner.gensym();
        let lowered_cond = self.lower_expression(cond);
        let lowered_true = self.lower_expression(true_value);
        let lowered_false = self.lower_expression(false_value);

        let desugared = hir::Statement::If(
            lowered_cond,
            Box::new(hir::Statement::Expression(
                hir::Expression::ReferenceAssignment(
                    local,
                    Box::new(lowered_true)
                )
            )),
            Some(Box::new(hir::Statement::Expression(
                hir::Expression::ReferenceAssignment(
                    local,
                    Box::new(lowered_false)
                )
            )))
        );

        // this doesn't directly translate back to ES, due to the additional semantics of the
        // HIR "Sequence" node, which is more general than ECMAScript's comma operator.
        hir::Expression::Sequence(vec![desugared], Box::new(hir::Expression::Identifier(local)))
    }

    fn lower_call_expression(&mut self,
                             base: &ast::SpannedExpression,
                             args: &[ast::SpannedExpression]) -> hir::Expression {
        let lowered_base = self.lower_expression(base);
        let lowered_args : Vec<_> = args.iter().map(|s| self.lower_expression(&s)).collect();
        hir::Expression::Call(Box::new(lowered_base), lowered_args)
    }

    fn lower_new_expression(&mut self,
                             base: &ast::SpannedExpression,
                             args: &[ast::SpannedExpression]) -> hir::Expression {
        let lowered_base = self.lower_expression(base);
        let lowered_args : Vec<_> = args.iter().map(|s| self.lower_expression(&s)).collect();
        hir::Expression::New(Box::new(lowered_base), lowered_args)
    }

    fn lower_identifier(&mut self, ident: &ast::Identifier) -> hir::Expression {
        self.check_name(&ident.data);
        let interned = self.interner.intern(&ident.data);
        hir::Expression::Identifier(interned)
    }

    fn lower_literal(&mut self, lit: &ast::Literal) -> hir::Expression {
        let hir_lit = match *lit {
            ast::Literal::Boolean(value) => hir::Literal::Boolean(value),
            ast::Literal::Null => hir::Literal::Null,
            ast::Literal::Numeric(value) => hir::Literal::Numeric(value),
            ast::Literal::RegExp(ref regex, ref flags) => hir::Literal::RegExp(regex.clone(), flags.clone()),
            ast::Literal::String(ref value) => hir::Literal::String(value.clone())
        };

        hir::Expression::Literal(hir_lit)
    }

    fn lower_sequence(&mut self, seq: &[ast::SpannedExpression]) -> hir::Expression {
        // The Sequence HIR node has more broad semantics than ECMAScript's comma operator.
        // it can chain any number of Statement nodes together, which will be evlauated and
        // discarded, followed by a single Expression node that will be evaluated and whose
        // value becomes the value of the entire expression.
        let (statements, expr) = seq.split_at(seq.len() - 1);
        debug_assert!(expr.len() == 1);
        let lowered_statements : Vec<_> = statements.iter()
            .map(|e| {
                let lowered_expr = self.lower_expression(e);
                hir::Statement::Expression(lowered_expr)
            })
            .collect();
        let final_expr = self.lower_expression(&expr[0]);
        hir::Expression::Sequence(lowered_statements, Box::new(final_expr))
    }

    fn check_name(&mut self, name: &str) {
        if name == "arguments" {
            self.state.uses_arguments_ident = true;
        }
    }
}

fn is_use_strict_expr(stmt: &ast::Statement) -> bool {
    if let ast::Statement::Expression(ref expr) = *stmt {
        if let ast::Expression::Literal(ast::Literal::String(ref s)) = expr.data {
            if s == "use strict" {
                return true
            }
        }
    }

    false
}
