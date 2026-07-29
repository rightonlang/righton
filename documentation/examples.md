# Examples

## Hello World

```text
fn main():
    print("Hello, Righton!")
    return 0
```

## Variables and Arithmetic

```text
fn main():
    let a: i32 = 5
    let b: i32 = 3
    let c = a * b + 2
    print(c)
    return 0
```

## Strings and F-Strings

```text
fn main():
    let name = "World"
    print(f"Hello {name}")
    return 0
```

## Multiline Strings

```text
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
fn main():
    let counter = 5
    while counter > 0:
        print(counter)
        counter = counter - 1
    return 0
```

### While Loop with Break

```text
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
fn main():
    for i = 5:
        print(i)
    return 0
```

### For-In Loop

```text
fn main():
    let items = [1, 2, 3]
    for item in items:
        print(item)
    return 0
```

### For-In-Range Loop

```text
fn main():
    for i in 1..5:
        print(i)
    return 0
```

### For Loop with Break

```text
fn main():
    for i = 10:
        if i == 5:
            break
        print(i)
    return 0
```

### For Loop with Continue

```text
fn main():
    for i = 5:
        if i == 3:
            continue
        print(i)
    return 0
```

## Nested Loops

```text
fn main():
    for i = 3:
        for j = 3:
            print(f"i={i} j={j}")
    return 0
```

## Nested Loops with Break

```text
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

```text
enum Status:
    Pending
    Active
    Done

fn describe(status: Status) -> str:
    match status:
        Pending:
            "not started"
        Active:
            "in progress"
        Done:
            "finished"
        _:
            "unknown"

fn main():
    let s = Status::Active
    print(describe(s))
    return 0
```

```text
enum Option<T>:
    Some(T)
    None

fn get_value(opt: Option<i32>) -> i32:
    match opt:
        Some(x):
            x
        None:
            0

fn main():
    let val = Option::Some(42)
    print(get_value(val))
    return 0
```

## Structs

```text
struct Point:
    x: i32
    y: i32

fn main():
    let p = Point { x: 1, y: 2 }
    print(p.x)
    p.y = 3
    print(p.y)
    return 0
```

## Enums

```text
enum Option<T>:
    Some(T)
    None

fn get_value(opt: Option<i32>) -> i32:
    match opt:
        Some(x):
            x
        None:
            0

fn main():
    let val = Option::Some(42)
    print(get_value(val))
    return 0
```

## Impl Blocks

```text
struct Point:
    x: i32
    y: i32

impl Point:
    fn distance(self) -> f64:
        return sqrt(self.x * self.x + self.y * self.y)

fn main():
    let p = Point { x: 3, y: 4 }
    print(p.distance())  // 5.0
    return 0
```

- `impl` blocks attach methods to structs
- Methods use `self` as the receiver parameter
- `Self` can be used as a return type annotation

## Borrowing

```text
fn main():
    let x = 10
    let r = &x
    print(r)

    let mut y = 20
    let m = &mut y
    print(m)
    return 0
```

## Lists

```text
fn main():
    let nums = [1, 2, 3]
    print(nums[0])
    nums[1] = 5
    print(nums[1])
    return 0
```

## Tuples

```text
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

fn sum_up_to(n):
    let sum = 0
    for i = n:
        sum = sum + i
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
    for i in start..end:
        if i % 2 == 0:
            return i
    return 0

fn main():
    let result = find_first_even(1, 10)
    print(result)
    return 0
```

## Generic Function

```text
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
