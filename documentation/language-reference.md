# Righton Language Reference

## Overview

Righton is a small, expression-oriented language with Python-like blocks and LLVM-based compilation. It features static typing with type inference, algebraic data types, pattern matching, and a borrow checker for memory safety.

## Syntax Basics

- Comments start with `//`
- Statements are separated by newlines
- Blocks follow `:` and use indentation
- The compiler accepts top-level expressions and function definitions
- `import std` pulls in the standard library
- `import "path/to/module.ro"` and `import my.module` pull in other source files before codegen

## Literals

- Integers: `42`
- Floats: `3.14`
- Booleans: `true`, `false`
- Strings: `"hello"`
- Multiline strings: `"""hello\nworld"""`
- F-strings: `f"Hello {name}"`
- Lists: `[1, 2, 3]`
- Tuples: `(1, "hello", true)`

## Variables

```text
let x: i32 = 10
const name = "Righton"
x = 12
```

- `let` declares a mutable variable (all variables are mutable; `let mut` is accepted but ignored)
- `const` declares an immutable variable
- Type annotations are optional
- Supported type names include `i32`, `f64`, `float`, `str` (requires `import std`), `string` (requires `import std`), `ptr`, and user-defined struct/enum names

## Functions

```text
fn add(x: i32, y: i32) -> i32:
    return x + y
```

- Functions are defined with `fn name(params):`
- Parameters can have optional type annotations (`x: i32`)
- Return types are optional and use `-> Type:` syntax
- `main` is treated specially by the compiler and returns `i32`
- Functions can have generic parameters: `fn identity<T>(x: T) -> T:`
- `extern fn name(params) -> Type:` declares an external function

## Expressions

### Arithmetic Operators

- `+`, `-`, `*`, `/`, `%`, `**`
- Unary `+` and `-`
- Implicit multiplication: `2x` or `3(y + 1)`

### Comparison Operators

- `==`, `!=`, `<`, `<=`, `>`, `>=`
- Chained comparisons: `0 < x < 10`

### Logical Operators

- `and`, `or`, `not`

### Compound Assignment

- `+=`, `-=`, `*=`, `/=`, `%=`, `**=`

### Other Operators

- `&name` (immutable borrow), `&mut name` (mutable borrow)
- `..` (range operator in `for i in start..end`)
- `::` (enum variant access: `Color::Red`)

## Control Flow

### If/Else Statements

```text
if x > 0:
    return 1
elif x == 0:
    return 0
else:
    return -1
```

- `if` expressions can include `elif` and `else` blocks
- Nested `if` chains are supported
- `if` can be used as a value expression: `let x = if cond: 1 else: 2`

### While Loops

```text
let counter = 5
while counter > 0:
    counter = counter - 1
```

- `while` loops execute while a condition is true
- The condition is checked at the beginning of each iteration
- Supports `break` and `continue` statements

### For Loops

#### Countdown For Loop

```text
for i = 5:
    // loop body executes 5 times
    // i takes values 5, 4, 3, 2, 1
```

#### For-In Loop

```text
for item in items:
    print(item)
```

#### For-In-Range Loop

```text
for i in 1..5:
    print(i)
```

- The range `1..5` includes 1 and excludes 5
- The loop variable counts up from `start` to `end - 1`
- `for` loops support `break` and `continue` statements

### Break Statement

```text
for i = 10:
    if i == 5:
        break
```

- `break` exits the innermost loop immediately
- Can only be used inside a loop (`while` or `for`)

### Continue Statement

```text
while x > 0:
    x = x - 1
    if x == 3:
        continue
    print(x)
```

- `continue` skips the rest of the current iteration
- Can only be used inside a loop (`while` or `for`)

### Match Expression

```text
match value { 0: "zero", 1: "one", _: "other" }
```

- Match uses curly braces `{ }` with comma-separated `pattern: body` arms
- Supports integer patterns, wildcard `_`, and enum variants
- Enum variants use the unqualified variant name: `Red: 1, Blue: 2, _: 0`
- Enum variants with data use parenthesized bindings: `Some(x): x + 1`
- Match arms must all return the same type
- Match expressions support both integer and string return types

## Structs

```text
struct Point:
    let x = 0
    let y = 0
```

- Structs are defined with `struct Name:`
- Fields use `let name = default_value` syntax (the default value sets the field type)
- Generics are supported: `struct Box<T>:`

### Struct Usage

```text
let p = Point { x: 1, y: 2 }
print(p.x)
p.y = 3
```

- Field access: `p.x`
- Field assignment: `p.y = 3`
- Struct literals: `Point { x: 1, y: 2 }`

## Enums

```text
enum Color:
    Red
    Green
    Blue
```

- Enums are defined with `enum Name:`
- Variants can carry data: `enum Option<T>: Some(T), None`
- Pattern matching in `match` expressions

## Impl Blocks

```text
impl Point:
    fn distance(self) -> f64:
        return sqrt(self.x * self.x + self.y * self.y)
```

- Methods are called via `instance.method(args)` which desugars to `method(instance, args)`
- The `self` parameter is treated as the struct type inside the method body

## Borrowing

```text
let x = 10
let r = &x          // immutable borrow
let m = &mut x      // mutable borrow
```

- `&name` creates an immutable borrow
- `&mut name` creates a mutable borrow
- The borrow checker enforces:
  - Multiple immutable borrows allowed simultaneously
  - Only one mutable borrow at a time
  - Immutable and mutable borrows cannot coexist
  - Borrowed values cannot escape their scope

## Lists

```text
let nums = [1, 2, 3]
let first = nums[0]
nums[1] = 5
```

- Lists are created with `[elem1, elem2, ...]`
- Elements must have the same type
- Indexing: `list[i]`
- Index assignment: `list[i] = value`

## Tuples

```text
let pair = (1, "hello")
let first = pair.0
```

- Tuples are created with `(a, b, c, ...)`
- Tuple access uses numeric fields: `tup.0`, `tup.1`, etc.

## Imports

```text
import std
import utils.math
import "local/module.ro"
```

- `import std` enables the standard library
- `import my.module` converts dots to path separators
- `import "path.ro"` imports by file path

## Extern Functions

```text
extern fn malloc(size: i32) -> ptr
```

- `extern fn` declares external functions with required type annotations
- No function body is needed

## Type Aliases

```text
type String = str
type MyInt = i32
```

- `type Name = ExistingType` creates a type alias
- Only simple type names are supported after `=`

## Calls and Builtins

```text
print("Hello")
asm("nop")
```

### Standard Library

Available after `import std`:

- `print(value)` - print a value
- `print_int(value)` - print an integer
- `print_float(value)` - print a float
- `len(value)` - string length
- `read_file(path)` - read file contents
- `write_file(path, contents)` - write to file
- `exit()` - exit the program
- `is_empty(value)` - check if string is empty

### String Methods

- `contains(s, sub)` - check if string contains substring
- `starts_with(s, prefix)` - check prefix
- `ends_with(s, suffix)` - check suffix
- `substr(s, start, length)` - extract substring
- `trim(s)` - remove whitespace
- `to_uppercase(s)` - convert to uppercase
- `to_lowercase(s)` - convert to lowercase
- `to_string(v)` - convert to string
- `str_repeat(s, count)` - repeat string

### Parsing

- `to_int(s)` - parse integer
- `to_float(s)` - parse float
- `to_hex(n)` - convert to hex string

### Math Functions

- `abs(n)` - absolute value
- `floor(n)`, `ceil(n)`, `round(n)` - rounding
- `sqrt(n)` - square root
- `sin(n)`, `cos(n)`, `tan(n)` - trigonometry
- `min(a, b)`, `max(a, b)` - min/max
- `pow(base, exp)` - exponentiation

### I/O

- `read_line()` - read line from stdin
- `read_int()` - read and parse integer
- `read_float()` - read and parse float

### List Functions

- `list_len(lst)` - list length
- `list_push(lst, val)` - append element
- `list_pop(lst)` - remove last element
- `list_free(lst)` - free list memory

## Notes

- Strings and f-strings compile to `i8*` in LLVM IR
- Mixed int/float arithmetic promotes to float where needed
- The borrow checker is always active
- Generic functions are supported via monomorphization
- The language is still evolving, so some behavior is compiler-driven rather than fully spec-driven
