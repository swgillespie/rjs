//! This is the bytecode definition for the rjs VM.

use super::string_interer::InternedString;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Opcode {
    /// Does nothing.
    Nop,
    /// Duplicates the top of the stack.
    Dup,
    /// Pops a value from the stack.
    Pop,
    /// Swaps the position of TOS and TOS-1 on the stack.
    Rotate,
    // unary operations
    /// Pops a value from the stack and pushes `-value`
    /// onto the stack.
    Neg,
    /// Pops a value from the stack and pushes `+value`,
    /// the value converted into a number, onto the stack.
    Pos,
    /// Pops a value from the stack and pushes `!value`,
    /// the logical negation of value, onto the stack.
    Not,
    /// Pops a value from the stack and pushes `~value`,
    /// the bitwise negation of value, onto the stack.
    BitNot,
    /// Pops a value from the stack and pushes `typeof value`
    /// onto the stack.
    Typeof,
    // binary arithmetic operations
    /// Pops two values from the stack, adds them, and pushes
    /// the result onto the stack.
    Add,
    /// Pops two values from the stack, subtracts them, and
    /// pushes the result onto the stack.
    Sub,
    /// Pops two values from the stack, multiplies them, and
    /// pushes the result onto the stack.
    Mul,
    /// Pops two values from the stack, divides them, and
    /// pushes the result onto the stack.
    Div,
    /// Pops two values from the stack, calculates the modulo,
    /// and pushes the result onto the stack.
    Mod,
    /// Pops two values from the stack, calculates the bitwise or,
    /// and pushes the result onto the stack.
    BitOr,
    /// Pops two values from the stack, calculates the bitwise and,
    /// and pushes the result onto the stack.
    BitAnd,
    /// Pops two values from the stack, calculates the bitwise xor,
    /// and pushes the result onto the stack.
    BitXor,
    /// Pops two values from the stack, calculates whether the
    /// first value is contained in the second value, and pushes
    /// the result onto the stack.
    In,
    /// Pops two values `x` and `y` from the stack and calculates
    /// `x << y`, and pushes the result onto the stack.
    LeftShift,
    /// Pops two values `x` and `y` from the stack and calculates
    /// `x >> y`, and pushes the result onto the stack.
    RightShift,
    /// Pops two values `x` and `y` from the stack and calculates
    /// `x >>> y`, and pushes the result onto the stack.
    UnsignedRightShift,
    /// Pops two values from the stack, calculates whether the
    /// first value is an instanceof the second value, and pushes
    /// the result onto the stack.
    InstanceOf,
    // logical operators
    /// Pops a value off the stack, converts it to a boolean, pushes it
    /// back onto the stack, and jumps the given offset if the value is `false`
    And(isize),
    /// Pops a value off the stack, converts it to a boolean, pushes it
    /// back onto the stack, and jumps the given offset if the value is `true`
    Or(isize),
    // comparison operators
    /// Pops two values from the stack and pushes a `true` if the two
    /// values are equal.
    Eq,
    /// Pops two values from the stack and pushes a `true` if the two
    /// values are strictly equal.
    StrictEq,
    /// Pops two values from the stack and pushes a `true` if the first
    /// value is greater than the second value.
    GreaterThan,
    /// Pops two values from the stack and pushes a `true` if the first
    /// value is greater than or equal to the second value.
    GreaterThanEq,
    // property operations
    /// Pops a value from the stack, deletes the property
    /// named by the parameter from it, and pushes `true`
    /// onto the stack if the property deletion was successful
    /// or `false` otherwise.
    DeleteProperty(InternedString),
    /// Pops two values from the stack and deletes a property with the given name
    /// popped of the stack from the object popped off the stack.
    DeleteElement,
    /// Deletes a name from the current scope.
    DeleteName(InternedString),
    /// Pops a value from the stack, coerces it into an object, and calls
    /// [[Get]] upon the property with the given name.
    GetProperty(InternedString),
    /// Pops an object and a string from the stack and calls
    /// [[Get]] upon the property with the given name.
    GetElement,
    /// Pops an object and a value off the stack and calls
    /// [[Put]] upon the property with the given name.
    PutProperty(InternedString),
    /// Pops an object, a string, and a value from the stack and calls
    /// [[Put]] upon the property with the string name.
    PutElement,
    /// Pops an object and a value off the stack, defines a property with that
    /// name on the object, and pushes the object back onto the stack.
    InitProperty(InternedString),
    /// Pops an object and a value off the stack, defines a property getter with that
    /// name on the object, and pushes the object back onto the stack.
    InitPropertyGetter(InternedString),
    /// Pops an object and a value off the stack, defines a property setter with that
    /// name on the object, and pushes the object back onto the stack.
    InitPropertySetter(InternedString),
    // with statement opcodes
    /// Pops an object from the stack and uses it to enter a `with` block,
    /// which exposes the object's properties as identifiers.
    EnterWith,
    /// Pops the `with` scope off the scope chain.
    ExitWith,
    /// Loads a name from the current environment record. This may refer to a stack
    /// slot, a free variable, or a property bound by `with`.
    LdName(InternedString),
    /// Pops a value off the stack and stores it to the value bound by the given identifier
    /// in the environment record.
    StName(InternedString),
    // conditional branching
    /// Pops one element off the stack and, if it is true,
    /// adds the offset to the current PC.
    BrTrue(isize),
    /// Pops one element off the stack and, if it is false,
    /// adds the offset to the current PC.
    BrFalse(isize),
    /// Unconditionally adds the offset to the current PC.
    Jump(isize),
    /// Invokes the debugger.
    Debugger,
    // loading constants
    /// Pushes a constant number onto the stack.
    LdNum(f64),
    /// Pushes a constant boolean onto the stack.
    LdBool(bool),
    /// Pushes a constant string onto the stack.
    LdString(InternedString),
    /// Pushes `null` onto the stack.
    LdNull,
    /// Pushes `undefined` onto the stack.
    LdUndefined,
    /// Pushes a constant regular expression onto the stack.
    LdRegex(InternedString, InternedString),
    /// Indexes into the anonymous function table and pushes a function object
    /// onto the stack. The function object is not valid in this state - it must
    /// be combined with `make_closure` to fill in the free variable vector
    /// with appropriate values.
    LdLambda(usize),
    /// Pushes a new empty object onto the stack.
    LdObject,
    // control flow
    /// Pops a value off the stack and returns from the current function
    /// with that value.
    Ret,
    /// Pops a value off the stack and throws it as an exception.
    Throw,
    /// Pops a function object, a value for `this`, and some number of arguments off the stack and
    /// invokes that function object with the given arguments, pushing the return value
    /// of the function onto the stack.
    Call(usize),
    /// Pops an object and some number of arguments off the stacck and invokes
    /// the [[Construct]] internal method on that object with the given arguments,
    /// pushing the result onto the stack.
    New(usize),
    // environment manipulation
    /// Pops a value off of the stack and adds it to the current environment
    /// record with the current name.
    Def(InternedString),
    /// Pushes a reference to the current value of `this` onto the stack.
    This,
    // branching fixups for the emit stage. These should not persist
    // at runtime
    UnfixedAnd(usize),
    UnfixedOr(usize),
    UnfixedBrTrue(usize),
    UnfixedBrFalse(usize),
    UnfixedJump(usize),
    // opcodes emitted by the compiler indicating that an operation is not yet implemented
    NotImplemented(&'static str),
}
