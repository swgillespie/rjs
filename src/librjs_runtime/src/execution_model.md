# The rjs execution model

rjs is powered by a bytecode interpreter that draws a lot of inspiration
from **Guile**, the GNU Scheme interpreter, and **Spidermonkey**, a JavaScript
interpreter from Mozilla.

ECMAScript is a challenging language to compile to bytecode due to its
reference semantics. There is a clear distinction in the ECMAScript spec
between l-values and r-values, which can be difficult to model in a simple
AST-walking interpreter - things such as closures, `with` statements, and
`eval` can significantly complicate an AST-walking interpreter.

This document aims to provide an overview of the `rjs` execution model, in
hopes of nailing down a plan before executing it. I will annotate things
that are necessary to get an interpreter running, as well as things that are
performance optimizations for the future, or otherwise helpful to keep in mind
when building the interpreter but not critical to its function.

## Lexical Scopes
ECMAScript is a fairly unique language in that it proves a lexical construct
that embeds an object directly into the lexical scope of a program -
the `with` statement. The `with` statement operates on an object and
embeds each of its properties into its child lexical scope:

```js
var object = { a: 42 };
with (object) {
  a++;
}
console.log(object); // => { a: 43 }
```
In this example, the `with` statement embeds the property `a` of `object`
into its child scope, so that reads and writes to those properties go through
`object` instead of a normal lexical lookup.

The `with` statement cannot be implemented using a standard "hashtable"-like
lexical scope. It is not sufficient to iterate over the values of `object`'s
properties and insert them into a lexical scope. Such a scheme would fail
in cases such as this:
```js
var object = { key: 'hello' };
with (object) {
  key = 'world'
}
console.log(object); // => { key: 'helloworld' }
```

Here, `key` resolves to an l-value within the `with` statement - it is a
reference to the property of `object`, and thus can be assigned to. If
`with` introduced a lexical scope that only copied the properties of `object`,
the change to `key` would only exist as long as the `with` scope did - which
is not correct, since the changes made within `with` are reflected in the object
outside of the `with` statement.

Therefore, `rjs` uses **two** types of scopes, like the ECMAScript spec
describes: a *declarative environment record*, which is a standard hashtable
approach, and a *object environment record*, which wraps a reference to an
object and exposes all of that object's properties as identifiers within
the scope.

A **declarative environment record** is a lexical scope that binds identifiers
to ECMAScript values, using a hashtable. It is relatively simple and has no
knowledge of `with` statements - it simply binds names to values.

A **object environment record** consists of a single object and exposes each
of the properties of that object as a name in its scope.

Despite being backed by different mechanisms, both environment records provide
the same interface and are both considered environment records for the sake
of implementation.

### The Environment Record Chain
As a ECMAScript program executes and new scopes are entered and exited,
new lexical scopes are created and destroyed. An important property of lexical
scopes, however, is that they always form a *tree*, whose root is the
*global environment* and whose leaves are leaf scopes. Therefore, for every
environment record, there exists a **chain** of records that link it to the
global environment.

Consider a program such as this:
```js
function global_function() {
  var x = { key: 42 };
  function inner_function() {
    while(true) {
      x.key++;
    }
  }

  function another_inner_function() {
    with (x) {
      key = 99;
    }
  }
}
```

The environment record chain might look something like this:
```
GlobalEnvironment (declarative)
|
|
|
- global_function (declarative, function)
  |
  +- x: variable
  |
  +- inner_function: function (declarative, function)
  |  |
  |  - while_statement (declarative, local)
  |    |
  |    - (empty)
  |
  +- another_inner_function: function (declarative, function)
     |
     + with_statement (object, local)
       |
       - key: variable, referencing x.key      
```

This chain is vital to identifier lookup, since when an identifier is
referenced, the interpreter will begin at current record and traverse up the
record chain until an entry is found or the global record is reached.

### Closures
ECMAScript allows for closures, and so our interpreter will need to be robust
to the problem of variables escaping their declared lexical scope.

Consider the following program:
```js
var Counter = function() {
  var x = 0;
  return function() {
    x++;
    return x;
  }
}

var counter = new Counter();
console.log(counter()); // => 1
console.log(counter()); // => 2
console.log(counter()); // => 3
```

Here the function that is returned, bound to `Counter`, is a **closure**
over the variable `x`. The fact that `x` *escapes* the environment from
which it is declared means that it needs to be heap-allocated, and cannot
simply live on the stack frame of the anonymous function bound to `Counter`.

The solution that `rjs` uses to deal with this problem is the notion of a
"free variable vector" as a part of the standard definition of a `Function`
object. The free variable vector contains a list of free variables that are
referenced by this function. For every free variable, that function maintains
a pointer to that variable's location on the heap.

* For the initial version of `rjs`, all variables will be allocated on the
heap, regardless of whether or not they need to. A more advanced interpreter
would perform escape analysis on the program to identify precisely which
variables need to be heap allocated and which ones do not (ones that are not
closed over).

### Stack Slots
Variables in rjs can be allocated on either the stack or the heap, depending
on how they are used. In particular, if a variable *escapes* the scope in
which it is defined, it must be allocated on the heap, otherwise it can be
allocated on the stack.

As an example, consider the following function:
```js
function basic() {
  var x = 42;
  var y = 99;
  var z = x + y;
  return z;
}
```

Barring the obvious optimizations here, we have three live variables:
`x`, `y`, and `z`. We can observe, however, that none of these variables
*escape the frame in which they are defined* - that is, it is impossible
for these variables to be referenced from outside the `basic` function.

It is the job of the compiler to determine whether or not objects escape.
The compiler observes the following rules for whether or not a local
must be heap-allocated:

1. The local is taken by-reference, most notably by the `with` statement,
2. The local is referenced by a nested function

If either of the two cases are true, the allocation must be done on the heap.
These allocations are tracked by the current lexical environment.

Consider a function such as this:
```js
function heap_alloc() {
  var obj = { x: 42 };
  with (obj) {
    obj++;
  }

  return obj.x;
}
```

The `with` statement forces obj to be allocated on the heap, because it is
utilized by-reference.

If the compiler determines that a variable escapes and must be allocated on
the heap, it must **box** the value saved to that variable. For example,
a function like
```js
function boxing_ints() {
  var x = 42;
  return function() {
    x++;
    return x;
  }
}
```

Might have bytecode like this:
```
boxing_ints:
  ldint 42
  box
  stloc 0
  pop
  ldloc 0
  ldlambda <lambda>
  make_closure 1
  return
```

Every function object maintains a list of memory locations, called *stack slots*,
where locals are loaded and stored from. Each one of these stack slots can
be addressed by index or by name.

## Objects and Property Descriptors
TODO

## Bytecode
TODO!

```js
function nested_scopes() {
  var value = 42;
  function increment() {
    value++;
  }

  increment();
  increment();

  return value;
}
```

```
nested_scopes:
  ldconst 42
  stloc 0
  ldloc 1
  call
  ldloc 1
  call
  ldloc 0
  ret

increment:
  ldfree 0
  ldconst 1
  add
  stfree 0
  ret
```
