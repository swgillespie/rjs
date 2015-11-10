use ast::*;

pub trait Visitor: Sized {
    type Output: Default;

    fn visit_program(&mut self, program: &Program) -> Self::Output {
        walk_program(self, program)
    }

    fn visit_statement(&mut self, stmt: &SpannedStatement) -> Self::Output {
        walk_statement(self, stmt)
    }

    fn visit_expression_statement(&mut self, expr: &SpannedExpression) -> Self::Output {
        walk_expression_statement(self, expr)
    }

    fn visit_block(&mut self, block: &[SpannedStatement]) -> Self::Output {
        walk_block(self, block)
    }

    fn visit_empty_statement(&mut self) -> Self::Output {
        // nothing
        Default::default()
    }

    fn visit_debugger_statement(&mut self) -> Self::Output {
        // nothing
        Default::default()
    }

    fn visit_with_statement(&mut self, expr: &SpannedExpression, stmt: &SpannedStatement) -> Self::Output {
        walk_with_statement(self, expr, stmt)
    }

    fn visit_return_statement(&mut self, expr: Option<&SpannedExpression>) -> Self::Output {
        walk_return_statement(self, expr)
    }

    fn visit_labelled_statement(&mut self, ident: &Identifier, stmt: &SpannedStatement) -> Self::Output {
        walk_labelled_statement(self, ident, stmt)
    }

    fn visit_break_statement(&mut self, _: Option<&Identifier>) -> Self::Output {
        Default::default()
    }

    fn visit_continue_statement(&mut self, _: Option<&Identifier>) -> Self::Output {
        Default::default()
    }

    fn visit_if_statement(&mut self,
                          cond: &SpannedExpression,
                          true_branch: &SpannedStatement,
                          false_branch: Option<&SpannedStatement>) -> Self::Output {
        walk_if_statement(self, cond, true_branch, false_branch)
    }

    fn visit_switch_statement(&mut self, cond: &SpannedExpression, cases: &[SwitchCase]) -> Self::Output {
        walk_switch_statement(self, cond, cases)
    }

    fn visit_throw_statement(&mut self, expr: &SpannedExpression) -> Self::Output {
        walk_throw_statement(self, expr)
    }

    fn visit_try_statement(&mut self,
                           body: &SpannedStatement,
                           catch: Option<&CatchClause>,
                           finally: Option<&SpannedStatement>) -> Self::Output {
        walk_try_statement(self, body, catch, finally)
    }

    fn visit_while_statement(&mut self,
                             cond: &SpannedExpression,
                             body: &SpannedStatement) -> Self::Output {
        walk_while_statement(self, cond, body)
    }

    fn visit_do_while_statement(&mut self,
                                cond: &SpannedExpression,
                                body: &SpannedStatement) -> Self::Output {
        walk_do_while_statement(self, cond, body)
    }

    fn visit_for_statement(&mut self,
                           init: Option<&ForInit>,
                           condition: Option<&SpannedExpression>,
                           update: Option<&SpannedExpression>,
                           body: &SpannedStatement) -> Self::Output {
        walk_for_statement(self, init, condition, update, body)
    }

    fn visit_for_in_statement(&mut self,
                              init: &ForInit,
                              binding: &SpannedExpression,
                              body: &SpannedStatement) -> Self::Output {
        walk_for_in_statement(self, init, binding, body)
    }

    fn visit_declaration(&mut self, decl: &Declaration) -> Self::Output {
        walk_declaration(self, decl)
    }

    fn visit_expression(&mut self, expr: &SpannedExpression) -> Self::Output {
        walk_expression(self, expr)
    }

    fn visit_function(&mut self, func: &Function) -> Self::Output {
        walk_function(self, func)
    }

    fn visit_this(&mut self) -> Self::Output {
        Default::default()
    }

    fn visit_array_literal(&mut self, values: &[Option<SpannedExpression>]) -> Self::Output {
        walk_array_literal(self, values)
    }

    fn visit_object_literal(&mut self, properties: &[Property]) -> Self::Output {
        walk_object_literal(self, properties)
    }

    fn visit_unary_op(&mut self,
                      _: UnaryOperator,
                      _: bool,
                      expr: &SpannedExpression) -> Self::Output {
        walk_unary_op(self, expr)
    }

    fn visit_binary_op(&mut self,
                       _: BinaryOperator,
                       left: &SpannedExpression,
                       right: &SpannedExpression) -> Self::Output {
        walk_binary_op(self, left, right)
    }

    fn visit_update_op(&mut self,
                       _: UpdateOperator,
                       _: bool,
                       expr: &SpannedExpression) -> Self::Output {
        walk_unary_op(self, expr)
    }

    fn visit_logical_op(&mut self,
                        _: LogicalOperator,
                        left: &SpannedExpression,
                        right: &SpannedExpression) -> Self::Output {
        walk_binary_op(self, left, right)
    }

    fn visit_member_expression(&mut self,
                               base: &SpannedExpression,
                               target: &SpannedExpression,
                               calculated: bool) -> Self::Output {
        walk_member_expression(self, base, target, calculated)
    }

    fn visit_assignment(&mut self,
                        _: AssignmentOperator,
                        target: &PatternOrExpression,
                        value: &SpannedExpression) -> Self::Output {
        walk_assignment(self, target, value)
    }

    fn visit_conditional_expression(&mut self,
                                    cond: &SpannedExpression,
                                    true_value: &SpannedExpression,
                                    false_value: &SpannedExpression) -> Self::Output {
        walk_conditional_expression(self, cond, true_value, false_value)
    }

    fn visit_call_expression(&mut self,
                             base: &SpannedExpression,
                             args: &[SpannedExpression]) -> Self::Output {
        walk_call_expression(self, base, args)
    }

    fn visit_new_expression(&mut self,
                            base: &SpannedExpression,
                            args: &[SpannedExpression]) -> Self::Output {
        walk_call_expression(self, base, args)
    }

    fn visit_sequence(&mut self, seq: &[SpannedExpression]) -> Self::Output {
        walk_sequence(self, seq)
    }

    fn visit_identifier(&mut self, _: &Identifier) -> Self::Output {
        Default::default()
    }

    fn visit_literal(&mut self, _: &Literal) -> Self::Output {
        Default::default()
    }
}

fn walk_program<V: Visitor>(visitor: &mut V, program: &Program) -> V::Output {
    for prolog in &program.directive_prologue {
        visitor.visit_statement(prolog);
    }

    for stmt in &program.statements {
        visitor.visit_statement(stmt);
    }

    Default::default()
}

fn walk_statement<V: Visitor>(visitor: &mut V, stmt: &SpannedStatement) -> V::Output {
    match stmt.data {
        Statement::Expression(ref expr) => visitor.visit_expression_statement(expr),
        Statement::Block(ref block) => visitor.visit_block(block),
        Statement::Empty => visitor.visit_empty_statement(),
        Statement::Debugger => visitor.visit_debugger_statement(),
        Statement::With(ref expr, ref stmt) => visitor.visit_with_statement(expr, stmt),
        Statement::Return(ref expr) => visitor.visit_return_statement(expr.as_ref()),
        Statement::Label(ref label, ref statement) => visitor.visit_labelled_statement(label, statement),
        Statement::Break(ref label) => visitor.visit_break_statement(label.as_ref()),
        Statement::Continue(ref label) => visitor.visit_continue_statement(label.as_ref()),
        Statement::If(ref cond, ref true_branch, ref false_branch) =>
            visitor.visit_if_statement(cond, true_branch, false_branch.as_ref().map(|x| &**x)),
        Statement::Switch(ref cond, ref cases) => visitor.visit_switch_statement(cond, cases),
        Statement::Throw(ref expr) => visitor.visit_throw_statement(expr),
        Statement::Try(ref body, ref catch, ref finally) =>
            visitor.visit_try_statement(body, catch.as_ref(), finally.as_ref().map(|x| &**x)),
        Statement::While(ref cond, ref body) => visitor.visit_while_statement(cond, body),
        Statement::DoWhile(ref cond, ref body) => visitor.visit_do_while_statement(cond, body),
        Statement::For(ref init, ref condition, ref update, ref body) =>
            visitor.visit_for_statement(init.as_ref(), condition.as_ref(), update.as_ref(), body),
        Statement::ForIn(ref init, ref binding, ref body) =>
            visitor.visit_for_in_statement(init, binding, body),
        Statement::Declaration(ref declaration) => visitor.visit_declaration(declaration)
    }
}

fn walk_expression_statement<V: Visitor>(visitor: &mut V, expr: &SpannedExpression) -> V::Output {
    visitor.visit_expression(expr);
    Default::default()
}

fn walk_block<V: Visitor>(visitor: &mut V, block: &[SpannedStatement]) -> V::Output {
    for stmt in block {
        visitor.visit_statement(stmt);
    }

    Default::default()
}

fn walk_with_statement<V: Visitor>(visitor: &mut V,
                                   expr: &SpannedExpression,
                                   stmt: &SpannedStatement) -> V::Output {
    visitor.visit_expression(expr);
    visitor.visit_statement(stmt);
    Default::default()
}

fn walk_return_statement<V: Visitor>(visitor: &mut V,
                                     expr: Option<&SpannedExpression>) -> V::Output {
    if let Some(value) = expr {
        visitor.visit_expression(value);
    }

    Default::default()
}

fn walk_labelled_statement<V: Visitor>(visitor: &mut V,
                                       _: &Identifier,
                                       stmt: &SpannedStatement) -> V::Output {
    visitor.visit_statement(stmt);
    Default::default()
}

fn walk_if_statement<V: Visitor>(visitor: &mut V,
                                 cond: &SpannedExpression,
                                 true_branch: &SpannedStatement,
                                 false_branch: Option<&SpannedStatement>) -> V::Output {
    visitor.visit_expression(cond);
    visitor.visit_statement(true_branch);
    if let Some(stmt) = false_branch {
        visitor.visit_statement(stmt);
    }

    Default::default()
}

fn walk_switch_statement<V: Visitor>(visitor: &mut V,
                                     cond: &SpannedExpression,
                                     cases: &[SwitchCase]) -> V::Output {
    visitor.visit_expression(cond);
    for case in cases {
        if let Some(ref test) = case.test {
            visitor.visit_expression(test);
        }

        for stmt in &case.body {
            visitor.visit_statement(stmt);
        }
    }

    Default::default()
}

fn walk_throw_statement<V: Visitor>(visitor: &mut V,
                                    expr: &SpannedExpression) -> V::Output {
    visitor.visit_expression(expr);
    Default::default()
}

fn walk_try_statement<V: Visitor>(visitor: &mut V,
                                  body: &SpannedStatement,
                                  catch: Option<&CatchClause>,
                                  finally: Option<&SpannedStatement>) -> V::Output {
    visitor.visit_statement(body);
    if let Some(catch_clause) = catch {
        // TODO(ES6) - visiting patterns
        visitor.visit_statement(&*catch_clause.body);
    }

    if let Some(finally_clause) = finally {
        visitor.visit_statement(finally_clause);
    }

    Default::default()
}

fn walk_while_statement<V: Visitor>(visitor: &mut V,
                                    cond: &SpannedExpression,
                                    stmt: &SpannedStatement) -> V::Output {
    visitor.visit_expression(cond);
    visitor.visit_statement(stmt);
    Default::default()
}

fn walk_do_while_statement<V: Visitor>(visitor: &mut V,
                                       cond: &SpannedExpression,
                                       stmt: &SpannedStatement) -> V::Output {
    visitor.visit_expression(cond);
    visitor.visit_statement(stmt);
    Default::default()
}

fn walk_for_statement<V: Visitor>(visitor: &mut V,
                                  init: Option<&ForInit>,
                                  condition: Option<&SpannedExpression>,
                                  update: Option<&SpannedExpression>,
                                  body: &SpannedStatement) -> V::Output {
    match init {
        Some(&ForInit::VarDec(ref decl)) => visitor.visit_declaration(&decl.data),
        Some(&ForInit::Expr(ref expr)) => visitor.visit_expression(expr),
        None => Default::default()
    };

    if let Some(expr) = condition {
        visitor.visit_expression(expr);
    }

    if let Some(expr) = update {
        visitor.visit_expression(expr);
    }

    visitor.visit_statement(body);
    Default::default()
}

fn walk_for_in_statement<V: Visitor>(visitor: &mut V,
                                     init: &ForInit,
                                     binding: &SpannedExpression,
                                     body: &SpannedStatement) -> V::Output {
    match init {
        &ForInit::VarDec(ref decl) => visitor.visit_declaration(&decl.data),
        &ForInit::Expr(ref expr) => visitor.visit_expression(expr)
    };

    visitor.visit_expression(binding);
    visitor.visit_statement(body);
    Default::default()
}

fn walk_declaration<V: Visitor>(visitor: &mut V,
                                decl: &Declaration) -> V::Output {
    match decl {
        &Declaration::Function(ref func) => visitor.visit_function(func),
        &Declaration::Variable(ref variables) => {
            for var in variables {
                // TODO(ES6) - visiting patterns
                if let Some(ref val) = var.initial_value {
                    visitor.visit_expression(val);
                }
            }

            Default::default()
        }
    };

    Default::default()
}

fn walk_expression<V: Visitor>(visitor: &mut V,
                               expr: &SpannedExpression) -> V::Output {
    match expr.data {
        Expression::This => visitor.visit_this(),
        Expression::Array(ref values) => visitor.visit_array_literal(values),
        Expression::Object(ref properties) => visitor.visit_object_literal(properties),
        Expression::Function(ref function) => visitor.visit_function(function),
        Expression::Unary(op, prefix, ref expr) => visitor.visit_unary_op(op, prefix, expr),
        Expression::Binary(op, ref left, ref right) => visitor.visit_binary_op(op, left, right),
        Expression::Update(op, prefix, ref expr) => visitor.visit_update_op(op, prefix, expr),
        Expression::Logical(op, ref left, ref right) => visitor.visit_logical_op(op, left, right),
        Expression::Member(ref base, ref target, calculated) => visitor.visit_member_expression(base, target, calculated),
        Expression::Assignment(op, ref target, ref value) => visitor.visit_assignment(op, target, value),
        Expression::Conditional(ref cond, ref true_value, ref false_value) =>
            visitor.visit_conditional_expression(cond, true_value, false_value),
        Expression::Call(ref base, ref args) =>
            visitor.visit_call_expression(base, args),
        Expression::New(ref base, ref args) =>
            visitor.visit_new_expression(base, args),
        Expression::Sequence(ref seq) => visitor.visit_sequence(seq),
        Expression::Identifier(ref ident) => visitor.visit_identifier(ident),
        Expression::Literal(ref lit) => visitor.visit_literal(lit)
    }
}

fn walk_function<V: Visitor>(visitor: &mut V, func: &Function) -> V::Output {
    // TODO(ES6) - visiting patterns
    for stmt in &func.prologue {
        visitor.visit_statement(stmt);
    }

    for stmt in &func.body {
        visitor.visit_statement(stmt);
    }

    Default::default()
}

fn walk_array_literal<V: Visitor>(visitor: &mut V,
                                  values: &[Option<SpannedExpression>]) -> V::Output {
    for entry in values {
        if let &Some(ref value) = entry {
            visitor.visit_expression(value);
        }
    }

    Default::default()
}

fn walk_object_literal<V: Visitor>(visitor: &mut V,
                                   properties: &[Property]) -> V::Output {
    for prop in properties {
        visitor.visit_expression(&*prop.value);
    }

    Default::default()
}

fn walk_unary_op<V: Visitor>(visitor: &mut V,
                             expr: &SpannedExpression) -> V::Output {
    visitor.visit_expression(expr);
    Default::default()
}

fn walk_binary_op<V: Visitor>(visitor: &mut V,
                              left: &SpannedExpression,
                              right: &SpannedExpression) -> V::Output {
    visitor.visit_expression(left);
    visitor.visit_expression(right);
    Default::default()
}

fn walk_member_expression<V: Visitor>(visitor: &mut V,
                                      base: &SpannedExpression,
                                      target: &SpannedExpression,
                                      calculated: bool) -> V::Output {
    visitor.visit_expression(base);
    if calculated {
        visitor.visit_expression(target);
    }

    Default::default()
}

fn walk_assignment<V: Visitor>(visitor: &mut V,
                               target: &PatternOrExpression,
                               value: &SpannedExpression) -> V::Output {
    match target {
        &PatternOrExpression::Expr(ref expr) => visitor.visit_expression(expr),
        _ => Default::default()
    };

    visitor.visit_expression(value);
    Default::default()
}

fn walk_conditional_expression<V: Visitor>(visitor: &mut V,
                                           cond: &SpannedExpression,
                                           true_value: &SpannedExpression,
                                           false_value: &SpannedExpression) -> V::Output {
    visitor.visit_expression(cond);
    visitor.visit_expression(true_value);
    visitor.visit_expression(false_value);

    Default::default()
}

fn walk_call_expression<V: Visitor>(visitor: &mut V,
                                    base: &SpannedExpression,
                                    args: &[SpannedExpression]) -> V::Output {
    visitor.visit_expression(base);
    for arg in args {
        visitor.visit_expression(arg);
    }

    Default::default()
}

fn walk_sequence<V: Visitor>(visitor: &mut V, seq: &[SpannedExpression]) -> V::Output {
    for expr in seq {
        visitor.visit_expression(expr);
    }

    Default::default()
}
