# Righton

**Righton** is a compiled programming language with Python-like syntax.  
It uses LLVM to generate machine code, making programs fast and efficient.

## Features

- Python-like syntax - easy to learn and read.
- LLVM-based compilation - native-level performance.
- Simple workflow: from source code to executable in seconds.
- Cross-platform: works on Linux and Windows (macOS support in question).

## Installation

```bash
# clone the repository
git clone https://github.com/rightonlang/righton.git
cd righton

# build the project
cargo build --release
```

## Usage

```bash
# compile a source file
./target/release/righton -i ./example.ro -o ./example.o

# link the generated object file to create an executable
# note: GCC (or another C compiler) is required for this step
gcc ./example.o -o ./example

# run
./example 
```

> Righton generates object files (.o) like a C compiler.  
> You need a linker (e.g., GCC) to turn them into a runnable program.  

## Example Code

```
// hello.ro
fn main():
    print("Hello, Righton!")
    return 0
```

## Planned Features

* Type system improvements
* Modules and packages
* Standard library
* Advanced data structures
