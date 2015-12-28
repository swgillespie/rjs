//! The execution engine is responsible for actually executing ECMAScript programs.
#![allow(unused_variables, dead_code)]

use heap::{Heap, RootedActivationPtr};
use values::{EvalResult, RootedValue, EvalValue, Value, IntoRootedValue};
use values::activation::Activation;
use values::object::HostObject;
use values::function::{FunctionType, Function};
use exec::frame::Frame;
use compiler::{self, CompiledProgram, CompiledFunction, StringInterner, Opcode};
use exec::helpers;

use librjs_syntax::{Lexer, Parser};

// use values::{RootedValue, EvalValue};
use std::convert::AsRef;
use std::collections::LinkedList;
use std::f64;

pub struct ExecutionEngine {
    heap: Box<Heap>,
    stack: LinkedList<Frame>,
    program: CompiledProgram,
}

impl ExecutionEngine {
    pub fn new(program: CompiledProgram) -> ExecutionEngine {
        let mut heap = Heap::new();
        let obj = heap.allocate_object().into_rooted_value(&mut heap);

        // ATTENTION - IT IS EXTREMELY IMPORTANT THAT HEAP NEVER GET MOVED.
        // if it does, it immediately invalidates the pointer that obj contains
        // and we get segmentation faults.
        //
        // In order to avoid moving heap, we initialize global object to be uninitialized
        // and fill it in immediately afterward using the heap in its final place.
        let mut ee = ExecutionEngine {
            heap: Box::new(Heap::new()),
            stack: LinkedList::new(),
            program: program,
        };

        // set up the initial stack frame
        let obj = ee.heap.root_value(Value::Undefined); // TODO global object this
        let global_act = Activation::new_function_activation(&mut ee, None, obj);
        let global_frame = Frame::new(global_act, "<global frame>".to_string());
        ee.stack.push_front(global_frame);

        // let obj = ee.heap_mut().allocate_object();
        // ee.global_object = obj.into_rooted_value(ee.heap_mut());

        // self.initialize_base_environment();
        ee
    }

    pub fn eval_str<S: AsRef<str>>(&mut self, value: S) -> EvalValue {
        let string = value.as_ref();
        self.eval(string.chars())
    }

    pub fn eval<I: Iterator<Item = char>>(&mut self, iter: I) -> EvalValue {
        debug!(target: "exec", "entering new eval context");
        let mut parser = Parser::new(Lexer::new(iter));
        let ast = match parser.parse_program() {
            Ok(ast) => ast,
            Err(e) => {
                debug!(target: "exec", "encountered syntax error: {:?}", e);
                return self.throw_syntax_error("failed to parse");
            }
        };

        debug!(target: "exec", "compiling eval string");
        let hir = compiler::lower_program_to_hir(self.interner_mut(), &ast);
        compiler::lower_hir_to_bytecode_append(&mut self.program, &hir);

        // here, the final entry in the global function vector of program
        // contains the code that should be evaled.
        // According to the spec, eval code should use the same environment
        // as its caller, so we use the existing activation.
        let activation = self.stack
                             .front()
                             .map(|f| f.activation.clone())
                             .expect("stack will not be empty here");

        let code = {
            self.program.current_global_function().to_vec()
        };

        let frame = Frame::new(activation.clone(), "<eval code>".to_string());
        self.stack.push_front(frame);
        self.execute(activation, &code)
    }

    pub fn expose_function<S, F>(&mut self, name: S, func: F)
        where S: AsRef<str>,
              F: Fn(&mut ExecutionEngine, RootedValue, Vec<RootedValue>) -> EvalValue + 'static
    {
        let global_act = self.stack
                             .back()
                             .map(|f| f.activation.clone())
                             .expect("stack should not be null here");
        let ptr: Box<Fn(&mut ExecutionEngine, RootedValue, Vec<RootedValue>) -> EvalValue> =
            Box::new(func);

        // the activation here doesn't matter, it won't get used.
        let function_obj = Function::new(self, FunctionType::Native(ptr), 0, &global_act);
        let name_idx = self.interner_mut().intern(name);
        // TODO this will panic if this is called before calling eval, which is common.
        let global_act = self.stack
                             .back()
                             .map(|f| f.activation.clone())
                             .expect("stack should not be null here");
        global_act.borrow_mut()
                  .create_mutable_binding(self, name_idx, true)
                  .expect("should never throw");
        global_act.borrow_mut()
                  .set_mutable_binding(self, name_idx, &function_obj, false)
                  .expect("should never throw");
    }

    pub fn call(&mut self,
                code_obj: &CompiledFunction,
                args: &[RootedValue],
                this: &RootedValue,
                activation: RootedActivationPtr)
                -> EvalValue {
        let frame_name = {
            code_obj.name()
                    .map(|idx| self.program.interner().get(idx))
                    .unwrap_or("<anonymous function>")
                    .to_string()
        };
        // creating a new function activation
        let function_act = Activation::new_function_activation(self,
                                                               Some(activation.clone()),
                                                               this.clone());

        // Section 10.5 - Declaration Binding Instantiation

        // TODO arguments object
        for (i, arg) in code_obj.arguments().iter().enumerate() {
            let value = if i >= args.len() {
                self.heap.root_value(Value::undefined())
            } else {
                args[i].clone()
            };

            if !function_act.borrow().has_binding(self, *arg) {
                // SPEC_NOTE/TODO: the spec does not indicate whether or not
                // this activation entry is deletable.
                function_act.borrow_mut()
                            .create_mutable_binding(self, *arg, false)
                            .expect("should never throw here");
            }

            function_act.borrow_mut()
                        .set_mutable_binding(self, *arg, &value, false)
                        .expect("should never throw here");
        }
        // create a new stack frame and stick it on the stack.
        let frame = Frame::new(function_act.clone(), frame_name);
        self.stack.push_front(frame);

        // let's go!
        self.execute(function_act, code_obj.code())
    }

    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn interner(&self) -> &StringInterner {
        self.program.interner()
    }

    pub fn interner_mut(&mut self) -> &mut StringInterner {
        self.program.interner_mut()
    }

    pub fn program(&self) -> &CompiledProgram {
        &self.program
    }

    pub fn program_mut(&mut self) -> &mut CompiledProgram {
        &mut self.program
    }

    pub fn throw_type_error<T>(&self, message: &'static str) -> EvalResult<T> {
        unimplemented!()
    }

    pub fn throw_reference_error<T>(&self, message: &'static str) -> EvalResult<T> {
        unimplemented!()
    }

    pub fn throw_syntax_error<T>(&self, message: &'static str) -> EvalResult<T> {
        unimplemented!()
    }

    /// Executes the current stack frame.
    fn execute(&mut self, activation: RootedActivationPtr, code: &[Opcode]) -> EvalValue {
        let mut ip = 0;
        let mut stack: Vec<RootedValue> = vec![];
        debug!(target: "exec", "entering bytecode interpreter loop");
        loop {
            let opcode = code[ip];
            match opcode {
                Opcode::Nop => {}
                Opcode::Dup => {
                    let tos = stack[0].clone();
                    stack.push(tos);
                }
                Opcode::Pop => {
                    stack.pop().expect("popped from empty stack: pop");
                }
                Opcode::Rotate => {
                    stack.swap(0, 1);
                }
                Opcode::Neg => {
                    let tos = stack.pop().expect("popped from empty stack: neg");

                    //
                    // if let Value::Number(value) = *tos {
                    // let numeric_value = {
                    // value.borrow()
                    // };
                    //
                    // if !numeric_value.is_nan() {
                    // value.borrow_mut() = -numeric_value;
                    // }
                    //
                    // stack.push(tos);
                    // } else {
                    let mut num = helpers::to_number(self, &tos);
                    if !num.is_nan() {
                        num = -num;
                    }

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = num;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                    // }
                }
                Opcode::Pos => {
                    let tos = stack.pop().expect("popped from empty stack: pos");
                    let num = helpers::to_number(self, &tos);

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = num;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::Not => {
                    let tos = stack.pop().expect("popped from empty stack: not");
                    let boolean = helpers::to_boolean(self, &tos);

                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = boolean;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::BitNot => {
                    let tos = stack.pop().expect("popped from empty stack: bitnot");
                    let num = helpers::to_int32(self, &tos);

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = !num as f64;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::Typeof => {
                    let tos = stack.pop().expect("popped from empty stack: typeof");
                    let ty = helpers::type_of(self, &tos);

                    let alloc = self.heap.allocate_string();
                    *alloc.borrow_mut() = ty;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::Add => {
                    let rhs = stack.pop().expect("popped from empty stack: add");
                    let lhs = stack.pop().expect("popped from empty stack: add");
                    let rhs_primitive = helpers::to_primitive(self, &rhs);
                    let lhs_primitive = helpers::to_primitive(self, &lhs);

                    if rhs_primitive.is_string() || lhs_primitive.is_string() {
                        let alloc = self.heap.allocate_string();
                        *alloc.borrow_mut() = {
                            let mut one_str = helpers::to_string(self, &lhs_primitive);
                            one_str.push_str(&helpers::to_string(self, &rhs_primitive));
                            one_str
                        };
                        stack.push(alloc.into_rooted_value(&mut self.heap));
                    } else {
                        let alloc = self.heap.allocate_number();
                        //  TODO verify that addition conforms with section 11.6.3,
                        //       changing if it does not
                        *alloc.borrow_mut() = helpers::to_number(self, &lhs_primitive) +
                                              helpers::to_number(self, &rhs_primitive);
                        stack.push(alloc.into_rooted_value(&mut self.heap));
                    }
                }
                Opcode::Sub => {
                    let rhs = stack.pop().expect("popped from empty stack: sub");
                    let lhs = stack.pop().expect("popped from empty stack: sub");
                    let alloc = self.heap.allocate_number();
                    //  TODO verify that subtraction conforms with section 11.6.3,
                    //       changing if it does not
                    *alloc.borrow_mut() = helpers::to_number(self, &lhs) -
                                          helpers::to_number(self, &rhs);
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::Mul => {
                    let one = stack.pop().expect("popped from empty stack: mul");
                    let two = stack.pop().expect("popped from empty stack: mul");
                    let alloc = self.heap.allocate_number();
                    //  TODO verify that multiplication conforms with section 11.6.3,
                    //       changing if it does not
                    *alloc.borrow_mut() = helpers::to_number(self, &one) *
                                          helpers::to_number(self, &two);
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::Div => {
                    // two / one
                    let one = stack.pop().expect("popped from empty stack: div");
                    let two = stack.pop().expect("popped from empty stack: div");
                    let one_number = helpers::to_number(self, &one);
                    let two_number = helpers::to_number(self, &two);
                    let value = if one_number == 0.0f64 {
                        two_number.signum() * f64::INFINITY
                    } else {
                        two_number / one_number
                    };

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = value;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::Mod => {
                    let one = stack.pop().expect("popped from empty stack: mod");
                    let two = stack.pop().expect("popped from empty stack: mod");
                    let one_number = helpers::to_number(self, &one);
                    let two_number = helpers::to_number(self, &two);
                    let value = if one_number == 0.0f64 {
                        two_number.signum() * f64::INFINITY
                    } else {
                        two_number % one_number
                    };

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = value;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::BitOr => {
                    let one = stack.pop().expect("popped from empty stack: bitor");
                    let two = stack.pop().expect("popped from empty stack: bitor");
                    let one_int = helpers::to_int32(self, &one);
                    let two_int = helpers::to_int32(self, &two);

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = (one_int | two_int) as f64;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::BitAnd => {
                    let one = stack.pop().expect("popped from empty stack: bitand");
                    let two = stack.pop().expect("popped from empty stack: bitand");
                    let one_int = helpers::to_int32(self, &one);
                    let two_int = helpers::to_int32(self, &two);

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = (one_int & two_int) as f64;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::BitXor => {
                    let one = stack.pop().expect("popped from empty stack: bitxor");
                    let two = stack.pop().expect("popped from empty stack: bitxor");
                    let one_int = helpers::to_int32(self, &one);
                    let two_int = helpers::to_int32(self, &two);

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = (one_int ^ two_int) as f64;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::In => {
                    let rhs = stack.pop().expect("popped from empty stack: in");
                    let lhs = stack.pop().expect("popped from empty stack: in");
                    if !rhs.is_object() {
                        return self.throw_type_error("invalid type for in expression");
                    }

                    let lhs_string = helpers::to_string(self, &lhs);
                    let lhs_idx = self.interner_mut().intern(lhs_string);
                    let ptr = rhs.unwrap_object();
                    let value = ptr.borrow_mut().has_property(self, lhs_idx);

                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = value;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::LeftShift => {
                    let rhs = stack.pop().expect("popped from empty stack: leftshift");
                    let lhs = stack.pop().expect("popped from empty stack: leftshift");
                    let lhs_int = helpers::to_int32(self, &lhs);
                    let rhs_uint = helpers::to_uint32(self, &rhs) & 0x1F;
                    let result = lhs_int << rhs_uint;

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = result as f64;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::RightShift => {
                    let rhs = stack.pop().expect("popped from empty stack: rightshift");
                    let lhs = stack.pop().expect("popped from empty stack: rightshift");
                    let lhs_int = helpers::to_int32(self, &lhs);
                    let rhs_uint = helpers::to_uint32(self, &rhs) & 0x1F;
                    let result = lhs_int >> rhs_uint;

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = result as f64;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::UnsignedRightShift => {
                    let rhs = stack.pop().expect("popped from empty stack: unsignedrightshift");
                    let lhs = stack.pop().expect("popped from empty stack: unsignedrightshift");
                    let lhs_int = helpers::to_uint32(self, &lhs);
                    let rhs_uint = helpers::to_uint32(self, &rhs) & 0x1F;
                    let result = lhs_int >> rhs_uint;

                    let alloc = self.heap.allocate_number();
                    *alloc.borrow_mut() = result as f64;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::InstanceOf => {
                    let rhs = stack.pop().expect("popped from empty stack: instanceof");
                    let lhs = stack.pop().expect("popped from empty stack: instanceof");
                    if !rhs.is_object() {
                        return self.throw_type_error("invalid type for instanceof operator");
                    }

                    let ptr = rhs.unwrap_object();
                    let value = try!(ptr.borrow_mut().has_instance(self, &lhs));

                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = value;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::And(offset) => {
                    let tos = stack.pop().expect("popped from emptpy stack: and");
                    let as_bool = helpers::to_boolean(self, &tos);

                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = as_bool;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                    if !as_bool {
                        // TODO this is for debugging and not good for performance
                        ip = (ip as isize)
                                 .checked_add(offset)
                                 .expect("arithmetic \
                                          overflow calculati\
                                          ng and offset") as usize;
                    }
                }
                Opcode::Or(offset) => {
                    let tos = stack.pop().expect("popped from emptpy stack: or");
                    let as_bool = helpers::to_boolean(self, &tos);

                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = as_bool;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                    if as_bool {
                        ip = (ip as isize)
                                 .checked_add(offset)
                                 .expect("arithmetic \
                                          overflow calculati\
                                          ng or offset") as usize;
                    }
                }
                Opcode::Eq => {
                    let rhs = stack.pop().expect("popped from empty stack: eq");
                    let lhs = stack.pop().expect("popped from empty stack: eq");
                    let value = helpers::equals(self, &rhs, &lhs);
                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = value;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::StrictEq => {
                    let rhs = stack.pop().expect("popped from empty stack: eq");
                    let lhs = stack.pop().expect("popped from empty stack: eq");
                    let value = helpers::strict_equals(self, &rhs, &lhs);
                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = value;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::GreaterThan => {
                    let rhs = stack.pop().expect("popped from empty stack: gt");
                    let lhs = stack.pop().expect("popped from empty stack: gt");
                    let value = helpers::greater_than(self, &rhs, &lhs);
                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = value;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::GreaterThanEq => {
                    let rhs = stack.pop().expect("popped from empty stack: geq");
                    let lhs = stack.pop().expect("popped from empty stack: geq");
                    let mut value = helpers::greater_than(self, &rhs, &lhs);
                    if !value {
                        value = helpers::equals(self, &rhs, &lhs);
                    }
                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = value;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::DeleteProperty(name) => {
                    let obj = stack.pop().expect("popped from empty stack: deleteproperty");
                    let actual_obj = if obj.is_object() {
                        self.heap.root_value(obj.unwrap_object())
                    } else {
                        helpers::to_object(self, &obj)
                    };

                    let result = try!(actual_obj.borrow_mut().delete(self, name, false));
                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = result;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::DeleteElement => {
                    let prop = stack.pop().expect("popped from empty stack: deleteelement");
                    let obj = stack.pop().expect("popped from empty stack: deleteelement");

                    let actual_obj = if obj.is_object() {
                        self.heap.root_value(obj.unwrap_object())
                    } else {
                        helpers::to_object(self, &obj)
                    };

                    let prop_name = helpers::to_string(self, &prop);
                    let prop_idx = self.interner_mut().intern(prop_name);
                    let result = try!(actual_obj.borrow_mut().delete(self, prop_idx, false));
                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = result;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::DeleteName(name) => {
                    let result = try!(activation.borrow_mut().delete_binding(self, name));
                    let alloc = self.heap.allocate_boolean();
                    *alloc.borrow_mut() = result;
                    stack.push(alloc.into_rooted_value(&mut self.heap));
                }
                Opcode::GetProperty(name) => {
                    let obj = stack.pop().expect("popped from empty stack: getproperty");

                    let actual_obj = if obj.is_object() {
                        self.heap.root_value(obj.unwrap_object())
                    } else {
                        helpers::to_object(self, &obj)
                    };

                    let result = try!(actual_obj.borrow_mut().get(self, name));
                    stack.push(result);
                }
                Opcode::GetElement => {
                    let prop = stack.pop().expect("popped from empty stack: deleteelement");
                    let obj = stack.pop().expect("popped from empty stack: deleteelement");

                    let actual_obj = if obj.is_object() {
                        self.heap.root_value(obj.unwrap_object())
                    } else {
                        helpers::to_object(self, &obj)
                    };

                    let prop_name = helpers::to_string(self, &prop);
                    let prop_idx = self.interner_mut().intern(prop_name);
                    let result = try!(actual_obj.borrow_mut().get(self, prop_idx));
                    stack.push(result);
                }
                Opcode::PutProperty(name) => {
                    let value = stack.pop().expect("popped from empty stack: putproperty");
                    let obj = stack.pop().expect("popped from empty stack: putproperty");

                    let actual_obj = if obj.is_object() {
                        self.heap.root_value(obj.unwrap_object())
                    } else {
                        helpers::to_object(self, &obj)
                    };

                    try!(actual_obj.borrow_mut().put(self, name, &value, false));
                }
                Opcode::PutElement => {
                    let value = stack.pop().expect("popped from empty stack: putelement");
                    let prop = stack.pop().expect("popped from empty stack: putelement");
                    let obj = stack.pop().expect("popped from empty stack: putelement");

                    let actual_obj = if obj.is_object() {
                        self.heap.root_value(obj.unwrap_object())
                    } else {
                        helpers::to_object(self, &obj)
                    };
                    let prop_name = helpers::to_string(self, &prop);
                    let prop_idx = self.interner_mut().intern(prop_name);
                    try!(actual_obj.borrow_mut().put(self, prop_idx, &value, false));
                }
                Opcode::InitProperty(name) => {
                    let value = stack.pop().expect("popped from empty stack: putproperty");
                    let obj = stack.pop().expect("popped from empty stack: putproperty");

                    let actual_obj = if obj.is_object() {
                        self.heap.root_value(obj.unwrap_object())
                    } else {
                        helpers::to_object(self, &obj)
                    };

                    unimplemented!()
                }
                Opcode::InitPropertyGetter(name) => panic!("unimplemented: initpropertygetter"),
                Opcode::InitPropertySetter(name) => panic!("unimplemented: initpropertysetter"),
                Opcode::EnterWith => unimplemented!(),
                Opcode::ExitWith => unimplemented!(),
                Opcode::LdName(name) => {
                    let result = try!(activation.borrow().get_binding_value(self, name));
                    stack.push(result);
                }
                Opcode::StName(name) => {
                    let value = stack.pop().expect("popped from empty stack: stname");
                    try!(activation.borrow_mut().set_mutable_binding(self, name, &value, false));
                }
                Opcode::BrTrue(offset) => {
                    let value = stack.pop().expect("popped from empty stack: brtrue");
                    if helpers::to_boolean(self, &value) {
                        ip = (ip as isize)
                                 .checked_add(offset)
                                 .expect("arithmetic \
                                          overflow calculati\
                                          ng or offset") as usize;
                    }
                }
                Opcode::BrFalse(offset) => {
                    let value = stack.pop().expect("popped from empty stack: brfalse");
                    if !helpers::to_boolean(self, &value) {
                        ip = (ip as isize)
                                 .checked_add(offset)
                                 .expect("arithmetic \
                                          overflow calculati\
                                          ng or offset") as usize;
                    }
                }
                Opcode::Jump(offset) => {
                    ip = (ip as isize)
                             .checked_add(offset)
                             .expect("arithmetic overflow \
                                      calculating or \
                                      offset") as usize;
                }
                Opcode::Debugger => {
                    // do nothing.
                }
                Opcode::LdNum(value) => {
                    let num = self.heap.allocate_number();
                    *num.borrow_mut() = value;
                    stack.push(num.into_rooted_value(&mut self.heap));
                }
                Opcode::LdBool(value) => {
                    let boolean = self.heap.allocate_boolean();
                    *boolean.borrow_mut() = value;
                    stack.push(boolean.into_rooted_value(&mut self.heap));
                }
                Opcode::LdString(value) => {
                    let heap_str = self.heap.allocate_string();
                    let str_value = self.interner().get(value).to_string();
                    *heap_str.borrow_mut() = str_value;
                    stack.push(heap_str.into_rooted_value(&mut self.heap));
                }
                Opcode::LdNull => {
                    stack.push(self.heap.root_value(Value::Null));
                }
                Opcode::LdUndefined => {
                    stack.push(self.heap.root_value(Value::Undefined));
                }
                Opcode::LdRegex(regex, flags) => panic!("unimplemented: ldregex"),
                Opcode::LdLambda(index) => {
                    let compiled_function = self.program.function_index(index).clone();
                    let arity = compiled_function.arity();
                    // make a code object
                    let func = Function::new(self,
                                             FunctionType::Interpreted(compiled_function),
                                             arity,
                                             &activation);
                    stack.push(func);
                }
                Opcode::LdObject => panic!("unimplemented: ldobject"),
                Opcode::Ret => {
                    debug!(target: "exec", "returning from function");
                    let ret_value = stack.pop().expect("popped from empty stack: ret");
                    if stack.len() > 0 {
                        warn!(target: "exec", "returning with {} elements still on stack", stack.len());
                    }
                    return Ok(ret_value);
                }
                Opcode::Throw => {
                    let throw_value = stack.pop().expect("popped from empty stack: throw");
                    // throw exception
                    unimplemented!()
                }
                Opcode::Call(num_args) => {
                    let mut args = vec![];
                    for _ in 0..num_args {
                        args.push(stack.pop().expect("popped from empty stack: call"));
                    }

                    args.reverse();
                    let this = stack.pop().expect("popped from empty stack: call");
                    let func = stack.pop().expect("popped from empty stack: call");
                    let result = try!(self.call_internal(func, args, this));
                    stack.push(result);
                }
                Opcode::New(num_args) => panic!("unimplemented: new"),
                Opcode::Def(name) => {
                    let value = stack.pop().expect("popped from empty stack: def");
                    try!(activation.borrow_mut().create_mutable_binding(self, name, true));
                    try!(activation.borrow_mut().set_mutable_binding(self, name, &value, false));
                }
                Opcode::This => {
                    let this = activation.borrow().implicit_this_value(self);
                    stack.push(this);
                }
                invalid_opcode => {
                    panic!("invalid opcode! {:?}", invalid_opcode);
                }
            }

            ip += 1;
        }
    }

    fn call_internal(&mut self,
                     func: RootedValue,
                     args: Vec<RootedValue>,
                     this: RootedValue)
                     -> EvalValue {
        if !func.is_object() {
            return self.throw_type_error("value is not a function");
        }

        let func_obj = func.unwrap_object();
        return func_obj.borrow().call(self, args, this);
    }
}
