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

### Basic For Loop

```text
fn main():
    for i = 5:
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
    for i = end:
        if i < start:
            break
        if i % 2 == 0:
            return i
    return 0

fn main():
    let result = find_first_even(1, 10)
    print(result)
    return 0
```
