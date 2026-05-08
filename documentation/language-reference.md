# Righton Language Reference

## Overview

Righton is a small, expression-oriented language with Python-like blocks and LLVM-based compilation.

## Syntax Basics

- Comments start with `//`
- Statements are separated by newlines
- Blocks follow `:` and use indentation in normal source style
- The compiler currently accepts top-level expressions and function definitions
- `import std` pulls in the standard library
- `import "path/to/module.ro"` and `import my.module` pull in other source files before codegen

## Literals

- Integers: `42`
- Floats: `3.14`
- Booleans: `true`, `false`
- Strings: `"hello"`
- Multiline strings: `"""hello\nworld"""`
- F-strings: `f"Hello {name}"`

## Variables

```text
let x: i32 = 10
const name = "Righton"
x = 12
```

- `let` declares a mutable variable
- `const` declares an immutable variable
- Type annotations are optional
- Supported type names include `i32`, `f64`, `float`, `str`, `string`, and `ptr`

## Functions

```text
fn add(x, y):
    return x + y
```

- Functions are defined with `fn name(params):`
- Parameters are untyped in the current frontend
- `main` is treated specially by the compiler and returns `i32`

## Expressions

Supported operators:

- Arithmetic: `+`, `-`, `*`, `/`, `%`, `**`
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Logic: `and`, `or`, `not`
- Parentheses for grouping
- Implicit multiplication such as `2x` or `3(y + 1)`

## Control Flow

### If/Else Statements

```text
if x > 0:
    return 1
else:
    return 0
```

- `if` expressions can include an `else` block
- Nested `if` chains are supported

### While Loops

```text
let counter = 5
while counter > 0:
    counter = counter - 1
```

- `while` loops execute while a condition is true
- The condition is checked at the beginning of each iteration
- Loop variable must be initialized before the loop
- Supports `break` and `continue` statements

### For Loops

```text
for i = 5:
    // loop body executes 5 times
    // i is the loop counter (i = 5, 4, 3, 2, 1)
```

- `for` loops iterate for a fixed number of iterations
- The loop counter (e.g., `i`) is created by the loop
- The counter decrements from the specified value to 1
- The loop variable is only accessible within the loop body
- Supports `break` and `continue` statements

### Break Statement

```text
for i = 10:
    if i == 5:
        break
    // continues here
```

- `break` exits the innermost loop immediately
- Control flow jumps to the statement after the loop
- Can only be used inside a loop (`while` or `for`)
- Commonly used with conditional statements

### Continue Statement

```text
while x > 0:
    x = x - 1
    if x == 3:
        continue
    print(x)
```

- `continue` skips the rest of the current iteration
- Control flow jumps back to the loop condition check
- Can only be used inside a loop (`while` or `for`)
- Useful for skipping certain iterations

### Nested Loops

```text
for i = 3:
    for j = 3:
        if j == 2:
            break  // only breaks inner loop
        j
```

- Loops can be nested inside each other
- `break` and `continue` only affect the innermost loop
- Each loop level maintains its own scope and counter

## Calls and Builtins

```text
print("Hello")
asm("nop")
```

- `print(...)`, `len(...)`, `read_file(...)`, `write_file(...)`, and `exit(...)` are available after `import std`
- `asm(...)` is recognized by the compiler as a builtin hook

## Notes

- Strings and f-strings compile to `i8*` in LLVM IR
- Mixed int/float arithmetic promotes to float where needed
- The language is still evolving, so some behavior is compiler-driven rather than fully spec-driven
