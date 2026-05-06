# Righton Language Reference

## Overview

Righton is a small, expression-oriented language with Python-like blocks and LLVM-based compilation.

## Syntax Basics

- Comments start with `//`
- Statements are separated by newlines
- Blocks follow `:` and use indentation in normal source style
- The compiler currently accepts top-level expressions and function definitions

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

```text
if x > 0:
    return 1
else:
    return 0
```

- `if` expressions can include an `else` block
- Nested `if` chains are supported

## Calls and Builtins

```text
print("Hello")
asm("nop")
```

- `print(...)` emits output for integers, floats, and strings
- `asm(...)` is recognized by the compiler as a builtin hook

## Notes

- Strings and f-strings compile to `i8*` in LLVM IR
- Mixed int/float arithmetic promotes to float where needed
- The language is still evolving, so some behavior is compiler-driven rather than fully spec-driven
