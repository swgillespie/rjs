//! The bytecode builder is responsible for transforming the compiler-intermediate
//! HIR into bytecode that will be directly executed by the interpreter.

#![allow(unused_variables, dead_code)]

use super::hir;
use super::emitter::{BytecodeEmitter, GlobalEmitter, CompiledProgram, CompiledFunction,
                     FunctionEmitter};
use super::string_interer::{InternedString, StringInterner};

use std::cell::Cell;

pub struct BytecodeBuilder {
    interner: StringInterner,
    is_strict: bool,
    should_produce_result: bool,
    functions: Vec<CompiledFunction>,
    function_index: Cell<usize>,
}

impl BytecodeBuilder {
    pub fn new(interner: StringInterner) -> BytecodeBuilder {
        BytecodeBuilder {
            interner: interner,
            is_strict: false,
            should_produce_result: false,
            functions: vec![],
            function_index: Cell::new(0),
        }
    }

    pub fn lower_program(mut self, program: &hir::Program) -> CompiledProgram {
        let mut global_emitter = GlobalEmitter::new();
        self.is_strict = program.is_strict();

        for stmt in program.statements() {
            self.lower_statement(&mut global_emitter, stmt);
        }

        global_emitter.bake(self.functions, self.interner)
    }

    fn lower_statement<E: BytecodeEmitter>(&mut self, emitter: &mut E, stmt: &hir::Statement) {
        match *stmt {
            hir::Statement::Expression(ref expr) => self.lower_expression_statement(emitter, expr),
            hir::Statement::Block(ref block) => self.lower_block(emitter, block),
            hir::Statement::Empty => self.lower_empty_statement(emitter),
            hir::Statement::Debugger => self.lower_debugger_statement(emitter),
            hir::Statement::With(ref expr, ref stmt) => {
                self.lower_with_statement(emitter, expr, stmt)
            }
            hir::Statement::Return(ref expr) => self.lower_return_statement(emitter, expr.as_ref()),
            hir::Statement::Label(label, ref statement) => {
                self.lower_labelled_statement(emitter, label, statement)
            }
            hir::Statement::Break(label) => self.lower_break_statement(emitter, label),
            hir::Statement::Continue(label) => self.lower_continue_statement(emitter, label),
            hir::Statement::If(ref cond, ref true_branch, ref false_branch) => {
                self.lower_if_statement(emitter,
                                        cond,
                                        true_branch,
                                        false_branch.as_ref().map(|x| &**x))
            }
            hir::Statement::Switch(ref cond, ref cases) => {
                self.lower_switch_statement(emitter, cond, cases)
            }
            hir::Statement::Throw(ref expr) => self.lower_throw_statement(emitter, expr),
            hir::Statement::Try(ref body, ref catch, ref finally) => {
                self.lower_try_statement(emitter,
                                         body,
                                         catch.as_ref(),
                                         finally.as_ref().map(|x| &**x))
            }
            hir::Statement::While(ref cond, ref body) => {
                self.lower_while_statement(emitter, cond, body)
            }
            hir::Statement::DoWhile(ref cond, ref body) => {
                self.lower_do_while_statement(emitter, cond, body)
            }
            hir::Statement::ForIn(ref init, ref binding, ref body) => {
                self.lower_for_in_statement(emitter, init, binding, body)
            }
            hir::Statement::Declaration(ref declaration) => {
                self.lower_declaration_statement(emitter, declaration)
            }
        }
    }

    fn lower_expression_statement<E: BytecodeEmitter>(&mut self,
                                                      emitter: &mut E,
                                                      expr: &hir::Expression) {
        // <expr> ; results in evaluating the expression and leaving the evaluated value on the stack.
        // If we're in a situation where a value can't be used, we emit a pop to clear it off the
        // stack.
        self.lower_expression(emitter, expr);

        if !self.should_produce_result {
            emitter.emit_pop();
        }
    }

    fn lower_block<E: BytecodeEmitter>(&mut self, emitter: &mut E, stmts: &[hir::Statement]) {
        if stmts.is_empty() {
            // nothing to do here if the block is empty.
            return;
        }

        let (init, last) = stmts.split_at(stmts.len() - 1);
        let saved = self.should_produce_result;

        self.should_produce_result = false;
        for stmt in init {
            self.lower_statement(emitter, stmt);
        }

        // the entire block evaluates to the last statement in the block.
        self.should_produce_result = true;
        self.lower_statement(emitter, &last[0]);
        self.should_produce_result = saved;
    }

    fn lower_empty_statement<E: BytecodeEmitter>(&mut self, emitter: &mut E) {
        // do nothing, obviously. the nop is only for debugging.
        emitter.emit_nop();
    }

    fn lower_debugger_statement<E: BytecodeEmitter>(&mut self, emitter: &mut E) {
        // the debugger opcode in the VM is supposed to trap and break into a debugger,
        // but that definitely doesn't work. For now, it's the same as a nop.
        emitter.emit_debugger();
    }

    fn lower_with_statement<E: BytecodeEmitter>(&mut self,
                                                emitter: &mut E,
                                                expr: &hir::Expression,
                                                stmt: &hir::Statement) {
        self.lower_expression(emitter, expr);
        // with the expression at the TOS, enter a with, codegen the body, and exit.
        emitter.emit_enterwith();
        self.lower_statement(emitter, stmt);
        emitter.emit_exitwith();
    }

    fn lower_return_statement<E: BytecodeEmitter>(&mut self,
                                                  emitter: &mut E,
                                                  expr: Option<&hir::Expression>) {
        if let Some(value) = expr {
            self.lower_expression(emitter, value);
        } else {
            emitter.emit_ldundefined();
        }

        emitter.emit_ret();
    }

    fn lower_labelled_statement<E: BytecodeEmitter>(&mut self,
                                                    emitter: &mut E,
                                                    label: InternedString,
                                                    stmt: &hir::Statement) {
        // TODO break/continue
        self.lower_statement(emitter, stmt);
    }

    fn lower_break_statement<E: BytecodeEmitter>(&mut self,
                                                 emitter: &mut E,
                                                 ident: Option<InternedString>) {
        emitter.emit_not_implemented("break statements");
    }

    fn lower_continue_statement<E: BytecodeEmitter>(&mut self,
                                                    emitter: &mut E,
                                                    ident: Option<InternedString>) {
        emitter.emit_not_implemented("continue statements");
    }

    fn lower_if_statement<E: BytecodeEmitter>(&mut self,
                                              emitter: &mut E,
                                              cond: &hir::Expression,
                                              true_branch: &hir::Statement,
                                              false_branch: Option<&hir::Statement>) {
        self.lower_expression(emitter, cond);
        // the condition is at the TOS.
        // TODO - lower if (!cond) into brtrue instead of brfalse
        if let Some(actual_false) = false_branch {
            // lower this into bytecode like
            // <cond>
            // brfalse false_branch
            // <true_branch>
            // jump done
            // false_branch: <false_branch>
            // done:
            let false_branch_label = emitter.create_label();
            let done_label = emitter.create_label();
            emitter.emit_brfalse(false_branch_label);
            self.lower_statement(emitter, true_branch);
            emitter.emit_jump(done_label);
            emitter.mark_label(false_branch_label);
            self.lower_statement(emitter, actual_false);
            emitter.mark_label(done_label);
        } else {
            // lower this into bytecode like
            // <cond>
            // brfalse done
            // <true_branch>
            // done:
            let done_label = emitter.create_label();
            emitter.emit_brfalse(done_label);
            self.lower_statement(emitter, true_branch);
            emitter.mark_label(done_label);
        }
    }

    fn lower_switch_statement<E: BytecodeEmitter>(&mut self,
                                                  emitter: &mut E,
                                                  cond: &hir::Expression,
                                                  cases: &[hir::SwitchCase]) {
        // TODO - do something really lame with switch cases
        emitter.emit_not_implemented("switch statements");
    }

    fn lower_throw_statement<E: BytecodeEmitter>(&mut self,
                                                 emitter: &mut E,
                                                 expr: &hir::Expression) {
        self.lower_expression(emitter, expr);
        emitter.emit_throw();
    }

    fn lower_try_statement<E: BytecodeEmitter>(&mut self,
                                               emitter: &mut E,
                                               body: &hir::Statement,
                                               catch: Option<&hir::CatchClause>,
                                               finally: Option<&hir::Statement>) {
        emitter.emit_not_implemented("try statements");
    }

    fn lower_while_statement<E: BytecodeEmitter>(&mut self,
                                                 emitter: &mut E,
                                                 cond: &hir::Expression,
                                                 body: &hir::Statement) {
        let cond_label = emitter.create_label();
        let loop_start = emitter.create_label();
        emitter.emit_jump(cond_label);
        emitter.mark_label(loop_start);
        self.lower_statement(emitter, body);
        emitter.mark_label(cond_label);
        self.lower_expression(emitter, cond);
        emitter.emit_brtrue(loop_start);
    }

    fn lower_do_while_statement<E: BytecodeEmitter>(&mut self,
                                                    emitter: &mut E,
                                                    cond: &hir::Expression,
                                                    body: &hir::Statement) {
        let loop_start = emitter.create_label();
        emitter.mark_label(loop_start);
        self.lower_statement(emitter, body);
        self.lower_expression(emitter, cond);
        emitter.emit_brtrue(loop_start);
    }

    fn lower_for_in_statement<E: BytecodeEmitter>(&mut self,
                                                  emitter: &mut E,
                                                  init: &hir::ForInit,
                                                  expr: &hir::Expression,
                                                  body: &hir::Statement) {
        emitter.emit_not_implemented("for-in loops");
    }

    fn lower_declaration_statement<E: BytecodeEmitter>(&mut self,
                                                       emitter: &mut E,
                                                       decl: &hir::Declaration) {
        match *decl {
            hir::Declaration::Function(ref func) => {
                let func_emitter = self.lower_function(func);
                let index = func_emitter.index();
                let compiled_func = func_emitter.bake();
                if let Some(name) = compiled_func.name() {
                    emitter.emit_ldlambda(index);
                    emitter.emit_def(name);
                } else {
                    panic!("parser allowed decl statement of function with no name");
                }

                if index >= self.functions.len() {
                    for _ in self.functions.len()..index + 1 {
                        self.functions.push(Default::default());
                    }
                }

                self.functions[index] = compiled_func;
            }
            hir::Declaration::Variable(ref vars) => {
                for decl in vars {
                    if let Some(ref initial_value) = decl.initial_value.as_ref() {
                        self.lower_expression(emitter, initial_value);
                    } else {
                        emitter.emit_ldundefined();
                    }

                    emitter.emit_def(decl.name);
                }
            }
        }
    }

    fn lower_expression<E: BytecodeEmitter>(&mut self, emitter: &mut E, expr: &hir::Expression) {
        match *expr {
            hir::Expression::This => self.lower_this(emitter),
            hir::Expression::Array(ref values) => self.lower_array_literal(emitter, values),
            hir::Expression::Object(ref properties) => {
                self.lower_object_literal(emitter, properties)
            }
            hir::Expression::Function(ref function) => {
                self.lower_function_expression(emitter, function)
            }
            hir::Expression::Unary(op, prefix, ref expr) => {
                self.lower_unary_op(emitter, op, prefix, expr)
            }
            hir::Expression::Binary(op, ref left, ref right) => {
                self.lower_binary_op(emitter, op, left, right)
            }
            hir::Expression::Call(ref base, ref args) => {
                self.lower_call_expression(emitter, base, args)
            }
            hir::Expression::Logical(op, ref left, ref right) => {
                self.lower_logical_op(emitter, op, left, right)
            }
            hir::Expression::New(ref base, ref args) => {
                self.lower_new_expression(emitter, base, args)
            }
            hir::Expression::Sequence(ref seq, ref expr) => self.lower_sequence(emitter, seq, expr),
            hir::Expression::Identifier(ident) => self.lower_identifier(emitter, ident),
            hir::Expression::Literal(ref lit) => self.lower_literal(emitter, lit),
            hir::Expression::ReferenceAssignment(ident, ref expr) => {
                self.lower_reference_assignment(emitter, ident, expr)
            }
            hir::Expression::LValueAssignment(ref base, ref expr) => {
                self.lower_lvalue_assignment(emitter, base, expr)
            }
            hir::Expression::IdentifierMember(ref base, member) => {
                self.lower_identifier_member(emitter, base, member)
            }
            hir::Expression::CalculatedMember(ref base, ref prop) => {
                self.lower_calculated_member(emitter, base, prop)
            }
        }
    }

    fn lower_this<E: BytecodeEmitter>(&mut self, emitter: &mut E) {
        // this gets resolved at runtime.
        emitter.emit_this();
    }

    fn lower_object_literal<E: BytecodeEmitter>(&mut self,
                                                emitter: &mut E,
                                                props: &[hir::Property]) {
        // first and foremost, we're creating a new object.
        emitter.emit_ldobject();
        for prop in props {
            // for every property in the literal, we need to define it on the object.
            let name = prop.key;
            self.lower_expression(emitter, &*prop.value); // expr obj
            match prop.kind {
                hir::PropertyKind::Init => emitter.emit_init_property(name),
                hir::PropertyKind::Get => emitter.emit_init_property_getter(name),
                hir::PropertyKind::Set => emitter.emit_init_property_setter(name),
            }

            // obj is back on the stack
        }
    }

    fn lower_array_literal<E: BytecodeEmitter>(&mut self,
                                               emitter: &mut E,
                                               values: &[Option<hir::Expression>]) {
        // for the first release, no attempt to optimize arrays into array-based representations
        // is done. We treat them exactly as objects.
        emitter.emit_ldobject();
        for (index, value) in values.iter().enumerate() {
            if let Some(initializer) = value.as_ref() {
                // the spec dictates that we do not emit a property for any elided entries.
                let prop_name = self.interner.intern(index.to_string());
                self.lower_expression(emitter, initializer);
                emitter.emit_init_property(prop_name);
            }
        }
    }

    fn lower_function_expression<E: BytecodeEmitter>(&mut self,
                                                     emitter: &mut E,
                                                     func: &hir::Function) {
        let func_emitter = self.lower_function(func);
        let index = func_emitter.index();
        let compiled_func = func_emitter.bake();
        emitter.emit_ldlambda(index);
        if index >= self.functions.len() {
            for _ in self.functions.len()..index + 1 {
                self.functions.push(Default::default());
            }

        }

        self.functions[index] = compiled_func;
    }

    fn lower_unary_op<E: BytecodeEmitter>(&mut self,
                                          emitter: &mut E,
                                          op: hir::UnaryOperator,
                                          prefix: bool,
                                          expr: &hir::Expression) {
        if !prefix {
            panic!("hunch check: what is a non-prefixed unary op? {:?} {:?}",
                   op,
                   expr);
        }

        if op == hir::UnaryOperator::Delete {
            // delete is a little different than the others
            self.lower_delete(emitter, expr);
            return;
        }

        self.lower_expression(emitter, expr);
        match op {
            hir::UnaryOperator::BitwiseNot => emitter.emit_bitnot(),
            hir::UnaryOperator::Delete => unreachable!(),
            hir::UnaryOperator::LogicalNot => emitter.emit_not(),
            hir::UnaryOperator::Minus => emitter.emit_neg(),
            hir::UnaryOperator::Plus => emitter.emit_pos(),
            hir::UnaryOperator::Typeof => emitter.emit_typeof(),
            hir::UnaryOperator::Void => {
                emitter.emit_pop();
                emitter.emit_ldundefined();
            }
        }
    }

    fn lower_delete<E: BytecodeEmitter>(&mut self, emitter: &mut E, expr: &hir::Expression) {
        // delete can be a number of things depending on what the expression is.
        match *expr {
            hir::Expression::Identifier(ident) => {
                // deleting an identifier requires a name lookup, since we could be
                // within a with statement.
                // this inhibits all sorts of optimizations in spidermonkey, which is interesting.
                emitter.emit_delete_name(ident);
            }
            hir::Expression::IdentifierMember(ref base, property) => {
                // this is a property deletion of the form "delete x.y".
                // we have to calculate x, and emit a delete property op.
                self.lower_expression(emitter, base);
                emitter.emit_delete_property(property);
            }
            hir::Expression::CalculatedMember(ref base, ref calculated) => {
                // this is a property deletion of the form "delete x[y]".
                self.lower_expression(emitter, base);
                self.lower_expression(emitter, calculated);
                emitter.emit_delete_element();
            }
            // nothing else has any effect with the delete statement
            // NB: the spec permits call expressions to return references, but forbids
            // it from happening within ECMAScript. For now it's assumed that does not happen.
            ref e => {
                self.lower_expression(emitter, e);
                emitter.emit_pop();
                // NB: section 11.4.1 of the spec indicates that if the type of e is not a reference,
                // the result of the delete statement should be `true`.
                emitter.emit_ldbool(true);
            }
        }
    }

    fn lower_logical_op<E: BytecodeEmitter>(&mut self,
                                            emitter: &mut E,
                                            op: hir::LogicalOperator,
                                            left: &hir::Expression,
                                            right: &hir::Expression) {
        // for logical operators, we have to perform short-circuit evaluation.
        // this means that:
        //   1) for AND nodes, we have to emit a branch that does not evaluate the RHS
        //      if the LHS is false, and
        //   2) for OR nodes, we have to emit a branch that does not evavluate the RHS
        //      if the LHS is true.
        self.lower_expression(emitter, left);
        let short_circut_label = emitter.create_label();
        match op {
            hir::LogicalOperator::Or => emitter.emit_or(short_circut_label),
            hir::LogicalOperator::And => emitter.emit_and(short_circut_label),
        }

        emitter.emit_pop();
        self.lower_expression(emitter, right);
        emitter.mark_label(short_circut_label);
    }

    fn lower_new_expression<E: BytecodeEmitter>(&mut self,
                                                emitter: &mut E,
                                                base: &hir::Expression,
                                                args: &[hir::Expression]) {
        // new expressions are similar to call expressions, except it invokes
        // the [[Constructor]] internal method and not the [[Call]] one.
        self.lower_expression(emitter, base);
        for arg in args {
            self.lower_expression(emitter, arg);
        }

        emitter.emit_new(args.len());
    }

    fn lower_sequence<E: BytecodeEmitter>(&mut self,
                                          emitter: &mut E,
                                          seq: &[hir::Statement],
                                          expr: &hir::Expression) {
        let old_should_produce = self.should_produce_result;
        self.should_produce_result = false;
        for stmt in seq {
            self.lower_statement(emitter, stmt);
        }

        self.should_produce_result = old_should_produce;
        self.lower_expression(emitter, expr);
    }

    fn lower_identifier<E: BytecodeEmitter>(&mut self, emitter: &mut E, ident: InternedString) {
        emitter.emit_ldname(ident);
    }

    fn lower_literal<E: BytecodeEmitter>(&mut self, emitter: &mut E, lit: &hir::Literal) {
        match *lit {
            hir::Literal::Boolean(b) => emitter.emit_ldbool(b),
            hir::Literal::Null => emitter.emit_ldnull(),
            hir::Literal::Numeric(n) => emitter.emit_ldnum(n),
            hir::Literal::String(s) => emitter.emit_ldstring(s),
            hir::Literal::RegExp(reg, flags) => emitter.emit_ldregex(reg, flags),
        }
    }

    fn lower_reference_assignment<E: BytecodeEmitter>(&mut self,
                                                      emitter: &mut E,
                                                      ident: InternedString,
                                                      expr: &hir::Expression) {
        self.lower_expression(emitter, expr);
        emitter.emit_stname(ident);
    }

    fn lower_binary_op<E: BytecodeEmitter>(&mut self,
                                           emitter: &mut E,
                                           op: hir::BinaryOperator,
                                           left: &hir::Expression,
                                           right: &hir::Expression) {
        self.lower_expression(emitter, left);
        self.lower_expression(emitter, right);
        match op {
            hir::BinaryOperator::BitwiseAnd => emitter.emit_bitand(),
            hir::BinaryOperator::BitwiseOr => emitter.emit_bitor(),
            hir::BinaryOperator::BitwiseXor => emitter.emit_bitxor(),
            hir::BinaryOperator::Div => emitter.emit_div(),
            hir::BinaryOperator::Equal => emitter.emit_eq(),
            hir::BinaryOperator::GreaterThan => emitter.emit_greaterthan(),
            hir::BinaryOperator::GreaterThanEq => emitter.emit_greaterthan_eq(),
            hir::BinaryOperator::In => emitter.emit_in(),
            hir::BinaryOperator::Instanceof => emitter.emit_instanceof(),
            hir::BinaryOperator::LeftShift => emitter.emit_leftshift(),
            hir::BinaryOperator::LessThan => {
                emitter.emit_greaterthan_eq();
                emitter.emit_not();
            }
            hir::BinaryOperator::LessThanEq => {
                emitter.emit_greaterthan();
                emitter.emit_not();
            }
            hir::BinaryOperator::Minus => emitter.emit_sub(),
            hir::BinaryOperator::Mod => emitter.emit_mod(),
            hir::BinaryOperator::NotEqual => {
                emitter.emit_eq();
                emitter.emit_not();
            }
            hir::BinaryOperator::Plus => emitter.emit_add(),
            hir::BinaryOperator::RightShift => emitter.emit_rightshift(),
            hir::BinaryOperator::StrictEqual => emitter.emit_stricteq(),
            hir::BinaryOperator::StrictNotEqual => {
                emitter.emit_stricteq();
                emitter.emit_not();
            }
            hir::BinaryOperator::Times => emitter.emit_mul(),
            hir::BinaryOperator::TripleRightShift => emitter.emit_unsigned_rightshift(),
        }
    }

    fn lower_call_expression<E: BytecodeEmitter>(&mut self,
                                                 emitter: &mut E,
                                                 base: &hir::Expression,
                                                 args: &[hir::Expression]) {
        // call instructions require that the value of `this` be pushed onto the stack in
        // addition to the object being called and the arguments to the function.
        if let hir::Expression::IdentifierMember(ref ident_base, prop) = *base {
            // calls of the form x.ident() require us to pass the value of `x` as `this`.
            self.lower_expression(emitter, ident_base); // this
            emitter.emit_dup();                         // this this
            emitter.emit_get_property(prop);            // prop this
            emitter.emit_rotate();                      // this prop
        } else if let hir::Expression::CalculatedMember(ref ident_base, ref prop_base) = *base {
            // calls of the form x[expr] require us to also pass the value of `x` as `this`.
            // The difference here is that we have to evaluate expr as well and emit a
            // `getelem` instead of invoking the property directly.
            self.lower_expression(emitter, ident_base); // this
            emitter.emit_dup();                         // this this
            self.lower_expression(emitter, prop_base);  // expr this this
            emitter.emit_rotate();                      // this expr this
            emitter.emit_get_element();                 // prop this
            emitter.emit_rotate();                      // this prop
        } else {
            // otherwise, the this value is the implicit this from this environment record
            // something like stuff(42)...
            self.lower_expression(emitter, base); // stuff
            emitter.emit_this();                  // this stuff
        }

        for arg in args {
            self.lower_expression(emitter, arg);    // <args> this prop
        }

        // call - pops args.len() arguments off the stack, pops `this`, and pops
        // an object and invokes the [[Call]] internal method on it.
        emitter.emit_call(args.len());
    }

    fn lower_lvalue_assignment<E: BytecodeEmitter>(&mut self,
                                                   emitter: &mut E,
                                                   base: &hir::Expression,
                                                   value: &hir::Expression) {
        // similar to Call nodes, we need to look at the base expression to see
        // what to emit.
        if let hir::Expression::IdentifierMember(ref ident_base, prop) = *base {
            self.lower_expression(emitter, ident_base);
            self.lower_expression(emitter, value);
            emitter.emit_put_property(prop);
        } else if let hir::Expression::CalculatedMember(ref ident_base, ref prop_base) = *base {
            self.lower_expression(emitter, ident_base); // base
            self.lower_expression(emitter, prop_base);  // prop base
            self.lower_expression(emitter, value);      // value prop base
            emitter.emit_put_element();
        } else if let hir::Expression::Identifier(name) = *base {
            self.lower_expression(emitter, value);
            emitter.emit_stname(name);
        } else {
            // this is impossible with the way the parser works today.
            unreachable!("unexpected lvalue node: {:?}", base);
        }
    }

    fn lower_identifier_member<E: BytecodeEmitter>(&mut self,
                                                   emitter: &mut E,
                                                   base: &hir::Expression,
                                                   ident: InternedString) {
        self.lower_expression(emitter, base);
        emitter.emit_get_property(ident);
    }

    fn lower_calculated_member<E: BytecodeEmitter>(&mut self,
                                                   emitter: &mut E,
                                                   base: &hir::Expression,
                                                   prop: &hir::Expression) {
        self.lower_expression(emitter, base);
        self.lower_expression(emitter, prop);
        emitter.emit_get_element();
    }

    fn lower_function<'a>(&mut self, func: &hir::Function) -> FunctionEmitter {
        let index = self.next_function_index();
        let mut emitter = FunctionEmitter::new(index);
        let old_strict = self.is_strict;
        self.is_strict = func.is_strict();
        emitter.set_name(func.name());
        emitter.set_arity(func.parameters().len());
        for stmt in func.body() {
            self.lower_statement(&mut emitter, stmt);
        }

        self.is_strict = old_strict;
        emitter
    }

    fn next_function_index(&self) -> usize {
        let value = self.function_index.get();
        self.function_index.set(value + 1);
        value
    }
}
