
# Righton Borrow Checker (Simplified Design)

## Overview

This document describes a simplified borrow checking system for the Righton language.  
The goal is to provide Rust-like memory safety without implementing full lifetime inference or complex compiler analysis.

The system prevents:
- use-after-free
- double mutable access
- invalid aliasing of mutable data

It is designed to be implementable in a basic compiler architecture (AST/IR-based).

---

## Core Memory Model

Each value in Righton exists in one of three states:

### 1. OWNED
A single variable owns the value.

- Responsible for deallocation (automatic via scope end)
- Can be moved, invalidating the previous owner

### 2. IMMUTABLE BORROW (`&T`)
Read-only reference.

- Multiple immutable borrows allowed simultaneously
- Prevents mutation of the original value

### 3. MUTABLE BORROW (`&mut T`)
Exclusive mutable reference.

- Only one allowed at a time
- Blocks all other borrows

---

## Ownership Rules

### Rule 1: Single Owner
Each value has exactly one owner at any time.

```rtn
let a = make();
let b = a; // move
// a is now invalid
````

---

### Rule 2: Move Semantics

Assignments of heap-owned values transfer ownership.

* Old variable becomes invalid after move
* No implicit cloning

---

## Borrowing Rules

### Rule 3: Immutable Borrowing

Multiple immutable borrows are allowed.

```rtn
let a = make();
let r1 = &a;
let r2 = &a; // OK
```

---

### Rule 4: Mutable Borrowing is Exclusive

Only one mutable borrow can exist at a time.

```rtn
let a = make();
let m1 = &mut a;
let m2 = &mut a; // ERROR
```

---

### Rule 5: No Mixed Borrows

Immutable and mutable borrows cannot coexist.

```rtn
let a = make();
let r = &a;
let m = &mut a; // ERROR
```

---

### Rule 6: Scope-Based Lifetime (No Lifetimes)

Borrow validity is limited to lexical scope `{}`.

* No explicit lifetime annotations
* Borrows are automatically invalidated at block exit

```rtn
{
    let r = &a;
} // r becomes invalid here
```

---

## Internal Compiler Model

Each variable is tracked using a simple state structure:

```rust
struct VarState {
    is_valid: bool,
    owner_id: Option<ScopeId>,
    imm_borrows: u32,
    has_mut_borrow: bool,
}
```

---

## Compiler Checks

### Move Operation

```rtn
a = b;
```

Checks:

* `b` must be valid and owned
* ownership transferred to `a`
* `b` becomes invalid

---

### Immutable Borrow

```rtn
r = &a;
```

Checks:

* no active mutable borrow on `a`
* increment immutable borrow counter

---

### Mutable Borrow

```rtn
m = &mut a;
```

Checks:

* `imm_borrows == 0`
* `has_mut_borrow == false`
* set mutable borrow active

---

### Scope Exit

On exiting a block:

* all borrows in that scope are invalidated
* counters reset accordingly
* ownership remains unless moved

---

## Safety Guarantees

This system guarantees:

* No use-after-free (within safe code)
* No concurrent mutable access
* No dangling borrows across scopes
* Deterministic memory behavior

---

## Limitations

This simplified model does NOT support:

* Lifetime polymorphism
* Cross-function borrow tracking
* Advanced alias analysis
* Async-aware borrowing
* GC integration

These may be added later as optional extensions.

---

## Design Philosophy

* Keep the model locally checkable (per function/block)
* Avoid global lifetime solving
* Prefer compiler simplicity over expressiveness
* Fail early and clearly at compile time

---

## Future Extensions (Optional)

* Borrow visualization in diagnostics
* Escape analysis for optimization
* Optional GC mode (non-interacting runtime)
* Interior mutability (`Cell`-like behavior, carefully constrained)

---

## Summary

Righton borrow checker is a **scope-based ownership system** inspired by Rust, but simplified:

* No lifetimes
* No global inference
* Strict scope-limited borrows
* Minimal compiler complexity

Goal: memory safety without turning the compiler into a research project.

