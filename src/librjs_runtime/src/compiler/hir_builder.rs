use librjs_syntax::ast;
use super::hir;
use super::string_interer::StringInterner;

pub struct HirBuilder<'a> {
    interner: &'a mut StringInterner
}

impl<'a> HirBuilder<'a> {
    pub fn new(interner: &'a mut StringInterner) -> HirBuilder {
        HirBuilder {
            interner: interner
        }
    }

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

    fn lower_statement(&mut self, stmt: &ast::SpannedStatement) -> hir::Statement {
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
            ast::Statement::Declaration(ref declaration) => hir::Statement::Declaration(self.lower_declaration(declaration))
        }
    }

    fn lower_expression_statement(&mut self, expr: &ast::SpannedExpression) -> hir::Statement {
        unimplemented!()
    }

    fn lower_block(&mut self, block: &[ast::SpannedStatement]) -> hir::Statement {
        unimplemented!()
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
        unimplemented!()
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
        unimplemented!()
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
        unimplemented!()
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

        for stmt in &func.body {
            function.add_statement(self.lower_statement(stmt));
        }

        for param in &func.parameters {
            // TODO(ES6) - destructuring patterns
            let ast::Pattern::Identifier(ref ident) = param.data;
            if ident.data == "arguments" {
                // we need to keep track of the arguments identifier,
                // since it influences what we're going to do with
                // the arguments object later
                function.set_uses_arguments_identifier(true);
            }

            let interned_param = self.interner.intern(&ident.data);
            function.add_parameter(interned_param);
        }

        if let Some(ref name) = func.name {
            let interned = self.interner.intern(&name.data);
            function.set_name(interned);
        }

        function
    }

    fn lower_variable_declarators(&mut self, decls: &[ast::VariableDeclarator]) -> hir::Declaration {
        unimplemented!()
    }

    fn lower_expression(&mut self, expr: &ast::SpannedExpression) -> hir::Expression {
        unimplemented!()
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
