# Examples

## Hello World

```text
import std

fn main():
    print("Hello, Righton!")
    return 0
```

## Variables and Arithmetic

```text
import std

fn main():
    let a: i32 = 5
    let b: i32 = 3
    let c = a * b + 2
    print(c)
    return 0
```

## Strings and F-Strings

```text
import std

fn main():
    let name = "World"
    print(f"Hello {name}")
    return 0
```

## Multiline Strings

```text
import std

fn main():
    let poem = """Roses are red,
violets are blue,
sugar is sweet,
and so are you."""
    print(poem)
    return 0
```

## While Loops

### Basic While Loop

```text
import std

fn main():
    let counter = 5
    while counter > 0:
        print(counter)
        counter = counter - 1
    return 0
```

### While Loop with Break

```text
import std

fn main():
    let x = 10
    while x > 0:
        if x == 5:
            break
        x = x - 1
    print("Done")
    return 0
```

### While Loop with Continue

```text
import std

fn main():
    let x = 5
    while x > 0:
        x = x - 1
        if x == 2:
            continue
        print(x)
    return 0
```

## For Loops

### Basic Countdown For Loop

```text
import std

fn main():
    for i = 5:
        print(i)
    return 0
```

### For-In Loop

```text
import std

fn main():
    let items = [1, 2, 3]
    for item in items:
        print(item)
    return 0
```

### For-In-Range Loop

```text
import std

fn main():
    for i in 1..5:
        print(i)
    return 0
```

### For Loop with Break

```text
import std

fn main():
    for i = 10:
        if i == 5:
            break
        print(i)
    return 0
```

### For Loop with Continue

```text
import std

fn main():
    for i = 5:
        if i == 3:
            continue
        print(i)
    return 0
```

## Nested Loops

```text
import std

fn main():
    for i = 3:
        for j = 3:
            print(f"i={i} j={j}")
    return 0
```

## Nested Loops with Break

```text
import std

fn main():
    for i = 3:
        for j = 3:
            if j == 2:
                break  // only breaks inner loop
            print(f"i={i} j={j}")
    return 0
```

## Conditionals

```text
import std

fn main():
    let x = 10
    if x > 10:
        print("greater")
    elif x == 10:
        print("equal")
    else:
        print("less")
    return 0
```

## Match Expression

Match uses curly braces with comma-separated arms:

```text
import std

fn match_example(n: i32) -> i32:
    return match n { 0: 10, 1: 20, _: 30 }

fn main():
    print(match_example(1))
    return 0
```

Match on enum variants uses the unqualified variant name (the variable must be declared in the same function so the type checker can infer the enum type):

```text
import std

enum Color:
    Red
    Green
    Blue

fn main():
    let c = Color::Red
    let desc = match c { Red: 1, Green: 2, Blue: 3, _: 0 }
    print(desc)
    return 0
```

## Structs

Struct fields use `let name = default_value` syntax:

```text
import std

struct Point:
    let x = 0
    let y = 0

fn main():
    let p = Point { x: 1, y: 2 }
    print(p.x)
    p.y = 3
    print(p.y)
    return 0
```

## Enums

```text
import std

enum Option:
    Some(i32)
    None

fn main():
    let val = Option::Some(42)
    print(val)
    return 0
```

## Borrowing

```text
fn main():
    let x = 10
    let r = &x
    let y = 20
    let m = &mut y
    return 0
```

## Lists

```text
import std

fn main():
    let nums = [1, 2, 3]
    print(nums[0])
    nums[1] = 5
    print(nums[1])
    return 0
```

## Tuples

```text
import std

fn main():
    let pair = (1, "hello")
    print(pair.0)
    print(pair.1)
    return 0
```

## Countdown Function

```text
import std

fn countdown(n):
    while n > 0:
        print(n)
        n = n - 1

fn main():
    countdown(5)
    return 0
```

## Sum Function Using Loops

```text
import std

fn sum_up_to(n) -> i32:
    let sum = 0
    let i = 1
    while i <= n:
        sum = sum + i
        i = i + 1
    return sum

fn main():
    let result = sum_up_to(10)
    print(result)  // Prints 55
    return 0
```

## Complex Control Flow

```text
import std

fn find_first_even(start, end):
    while start < end:
        if start % 2 == 0:
            return start
        start = start + 1
    return 0

fn main():
    let result = find_first_even(1, 10)
    print(result)
    return 0
```

## Generic Function

```text
import std

fn identity<T>(x: T) -> T:
    return x

fn main():
    let a = identity(42)
    let b = identity("hello")
    print(a)
    print(b)
    return 0
```

## Type Aliases

```text
import std

type String = str
type MyInt = i32

fn main():
    let s: String = "hello"
    print(s)
    return 0
```

## Extern Function

```text
extern fn malloc(size: i32) -> ptr

fn main():
    let ptr = malloc(100)
    return 0
```

## Not Yet Implemented
