# Righton Documentation

Righton is a compiled, Python-like language that lowers to LLVM IR and object files.

## Contents

- `language-reference.md` - syntax and features supported by the current compiler
- `examples.md` - small end-to-end examples

The compiler now supports simple source-file imports plus runtime helpers like `len`, `read_file`, `write_file`, and `exit`.

## Quick Start

```bash
cargo build --release
./target/release/righton -i ./example.ro -o ./example.o
gcc ./example.o -o ./example
./example
```
