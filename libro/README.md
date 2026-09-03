# libro — Righton C Runtime

`libro` is the C runtime for the Righton language. Previously all runtime helpers (`__rt_*`) were emitted as LLVM IR inside `src/compiler.rs` (`emit_runtime_helpers`). Now they live in C (`src/libro.c`) and the compiler only emits **declarations**.

## Layout

```
libro/
  include/libro.h   # public header, RoString definition, declared helpers
  src/libro.c       # implementations (string, file I/O, lists, math)
  Makefile          # builds libro.a
```

`RoString` matches LLVM `%String = { i8*, i32, i32 }` (ptr, len, cap). IR accesses it via `getelementptr %String`, so C field offsets (0,8,12 on 64-bit) match.

## Building

```bash
make -C libro          # produces libro/libro.a
# or
cc -O2 -I libro/include -c libro/src/libro.c -o libro.o
ar rcs libro.a libro.o
```

## Linking

Righton emits object files with undefined `__rt_*` symbols. Link with the runtime:

```bash
./target/release/righton -i example.ro -o example.o
gcc example.o libro/libro.a -o example -lm
./example
```

With `righton` auto-link (if driver is extended):
```bash
righton build example.ro --link
```

## Adding a new helper

1. Declare in `include/libro.h`
2. Implement in `src/libro.c`
3. Add `declare` in `src/compiler.rs::declare_runtime_helpers`
4. Add handling in `get_function_param_types` / `builtin_return_type` if called from Righton.

## Why C?

- Keeps `compiler.rs` focused on code generation, not runtime logic
- Leverages existing C library (strlen, malloc, math) without re-emitting IR
- Easier to test, debug with sanitizers, and optimize with the C compiler
- Produces a reusable static library for any Righton program
