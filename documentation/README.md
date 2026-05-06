# Righton Documentation

Righton is a compiled, Python-like language that lowers to LLVM IR and object files.

## Contents

- `language-reference.md` - syntax and features supported by the current compiler
- `examples.md` - small end-to-end examples

## Quick Start

```bash
cargo build --release
./target/release/righton -i ./example.ro -o ./example.o
gcc ./example.o -o ./example
./example
```
