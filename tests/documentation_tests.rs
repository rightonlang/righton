#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::process::Command;
    use std::sync::atomic::{AtomicU64, Ordering};

    static NEXT_TEMP_ID: AtomicU64 = AtomicU64::new(0);

    fn temp_root() -> PathBuf {
        let mut root = std::env::temp_dir();
        root.push("righton_doc_tests");
        fs::create_dir_all(&root).expect("failed to create temp root directory");
        root
    }

    fn temp_file(ext: &str) -> PathBuf {
        let mut path = temp_root();
        let id = NEXT_TEMP_ID.fetch_add(1, Ordering::Relaxed);
        path.push(format!("righton_doc_test_{}.{}", id, ext));
        path
    }

    fn run_bin(args: &[&str]) -> std::process::Output {
        Command::new(env!("CARGO_BIN_EXE_righton"))
            .args(args)
            .output()
            .expect("failed to run binary")
    }

    fn run_bin_in_dir(args: &[&str], cwd: &Path) -> std::process::Output {
        Command::new(env!("CARGO_BIN_EXE_righton"))
            .current_dir(cwd)
            .args(args)
            .output()
            .expect("failed to run binary")
    }

    fn assert_compiles(source: &str) {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, source).unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(
            result.status.success(),
            "compilation failed for:\n{}\nstderr: {}",
            source,
            String::from_utf8_lossy(&result.stderr)
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    fn assert_ir_contains(source: &str, pattern: &str) {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, source).unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(
            result.status.success(),
            "compilation failed for:\n{}\nstderr: {}",
            source,
            String::from_utf8_lossy(&result.stderr)
        );

        let ir = fs::read_to_string(&output).unwrap();
        assert!(
            ir.contains(pattern),
            "expected IR to contain '{}', got:\n{}",
            pattern,
            ir
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    fn assert_fails(source: &str, expected: &str) {
        let input = temp_file("ron");
        let output = temp_file("ll");
        fs::write(&input, source).unwrap();

        let result = run_bin(&[
            "-i",
            input.to_str().unwrap(),
            "-o",
            output.to_str().unwrap(),
        ]);

        assert!(
            !result.status.success(),
            "expected compilation to fail for:\n{}",
            source
        );
        let stderr = String::from_utf8_lossy(&result.stderr);
        assert!(
            stderr.contains(expected),
            "expected error containing '{}', got: {}",
            expected,
            stderr
        );

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }

    // ========================
    // HELLO WORLD
    // ========================

    #[test]
    fn test_doc_hello_world() {
        assert_compiles("import std\nfn main():\n    print(\"Hello, Righton!\")\n    return 0");
    }

    // ========================
    // VARIABLES AND ARITHMETIC
    // ========================

    #[test]
    fn test_doc_variables_and_arithmetic() {
        assert_ir_contains(
            "import std\nfn main():\n    let a: i32 = 5\n    let b: i32 = 3\n    let c = a * b + 2\n    print(c)\n    return 0",
            "mul i32",
        );
    }

    // ========================
    // STRINGS AND F-STRINGS
    // ========================

    #[test]
    fn test_doc_fstring() {
        assert_ir_contains(
            "import std\nfn main():\n    let name = \"World\"\n    print(f\"Hello {name}\")\n    return 0",
            "sprintf",
        );
    }

    // ========================
    // MULTILINE STRINGS
    // ========================

    #[test]
    fn test_doc_multiline_string() {
        assert_compiles(
            "import std\nfn main():\n    let poem = \"\"\"Roses are red,\nviolets are blue,\nsugar is sweet,\nand so are you.\"\"\"\n    print(poem)\n    return 0",
        );
    }

    // ========================
    // WHILE LOOPS
    // ========================

    #[test]
    fn test_doc_while_loop_basic() {
        assert_ir_contains(
            "import std\nfn main():\n    let counter = 5\n    while counter > 0:\n        print(counter)\n        counter = counter - 1\n    return 0",
            "while_start",
        );
    }

    #[test]
    fn test_doc_while_loop_break() {
        assert_ir_contains(
            "import std\nfn main():\n    let x = 10\n    while x > 0:\n        if x == 5:\n            break\n        x = x - 1\n    print(\"Done\")\n    return 0",
            "br label %while_end",
        );
    }

    #[test]
    fn test_doc_while_loop_continue() {
        assert_compiles(
            "import std\nfn main():\n    let x = 5\n    while x > 0:\n        x = x - 1\n        if x == 2:\n            continue\n        print(x)\n    return 0",
        );
    }

    // ========================
    // FOR LOOPS
    // ========================

    #[test]
    fn test_doc_for_loop_basic() {
        assert_ir_contains(
            "import std\nfn main():\n    for i = 5:\n        print(i)\n    return 0",
            "for_start",
        );
    }

    #[test]
    fn test_doc_for_in_loop() {
        assert_compiles(
            "import std\nfn main():\n    let items = [1, 2, 3]\n    for item in items:\n        print(item)\n    return 0",
        );
    }

    #[test]
    fn test_doc_for_loop_break() {
        assert_compiles(
            "import std\nfn main():\n    for i = 10:\n        if i == 5:\n            break\n        print(i)\n    return 0",
        );
    }

    #[test]
    fn test_doc_for_loop_continue() {
        assert_compiles(
            "import std\nfn main():\n    for i = 5:\n        if i == 3:\n            continue\n        print(i)\n    return 0",
        );
    }

    // ========================
    // NESTED LOOPS
    // ========================

    #[test]
    fn test_doc_nested_loops() {
        assert_compiles(
            "import std\nfn main():\n    for i = 3:\n        for j = 3:\n            print(f\"i={i} j={j}\")\n    return 0",
        );
    }

    #[test]
    fn test_doc_nested_loops_break() {
        assert_compiles(
            "import std\nfn main():\n    for i = 3:\n        for j = 3:\n            if j == 2:\n                break\n            print(f\"i={i} j={j}\")\n    return 0",
        );
    }

    // ========================
    // CONDITIONALS
    // ========================

    #[test]
    fn test_doc_conditionals() {
        assert_compiles(
            "import std\nfn main():\n    let x = 10\n    if x > 10:\n        print(\"greater\")\n    elif x == 10:\n        print(\"equal\")\n    else:\n        print(\"less\")\n    return 0",
        );
    }

    // ========================
    // STRUCTS
    // ========================

    #[test]
    fn test_doc_structs() {
        assert_compiles(
            "import std\nstruct Point:\n    let x = 0\n    let y = 0\n\nfn main():\n    let p = Point { x: 1, y: 2 }\n    print(p.x)\n    p.y = 3\n    print(p.y)\n    return 0",
        );
    }

    // ========================
    // MATCH EXPRESSION
    // ========================

    #[test]
    fn test_doc_match_integer() {
        assert_compiles(
            "import std\nfn match_example(n: i32) -> i32:\n    return match n { 0: 10, 1: 20, _: 30 }\n\nfn main():\n    print(match_example(1))\n    return 0",
        );
    }

    #[test]
    fn test_doc_match_enum() {
        assert_compiles(
            "import std\n\nenum Color:\n    Red\n    Green\n    Blue\n\nfn main():\n    let c = Color::Red\n    let desc = match c { Red: 1, Green: 2, Blue: 3, _: 0 }\n    print(desc)\n    return 0",
        );
    }

    // ========================
    // ENUMS (basic)
    // ========================

    #[test]
    fn test_doc_enums_basic() {
        assert_compiles(
            "import std\nenum Color:\n    Red\n    Green\n    Blue\n\nfn main():\n    let c = Color::Red\n    print(c)\n    return 0",
        );
    }

    // ========================
    // BORROWING
    // ========================

    #[test]
    fn test_doc_borrowing() {
        assert_ir_contains(
            "fn main():\n    let x = 10\n    let r = &x\n    let y = 20\n    let m = &mut y\n    return 0",
            "bitcast",
        );
    }

    // ========================
    // LISTS
    // ========================

    #[test]
    fn test_doc_lists() {
        assert_compiles(
            "import std\nfn main():\n    let nums = [1, 2, 3]\n    print(nums[0])\n    nums[1] = 5\n    print(nums[1])\n    return 0",
        );
    }

    // ========================
    // TUPLES
    // ========================

    #[test]
    fn test_doc_tuples() {
        assert_compiles(
            "import std\nfn main():\n    let pair = (1, \"hello\")\n    print(pair.0)\n    print(pair.1)\n    return 0",
        );
    }

    // ========================
    // FUNCTIONS
    // ========================

    #[test]
    fn test_doc_countdown_function() {
        assert_compiles(
            "import std\n\nfn countdown(n):\n    while n > 0:\n        print(n)\n        n = n - 1\n\nfn main():\n    countdown(5)\n    return 0",
        );
    }

    #[test]
    fn test_doc_sum_function() {
        assert_compiles(
            "import std\n\nfn sum_up_to(n) -> i32:\n    let sum = 0\n    let i = 1\n    while i <= n:\n        sum = sum + i\n        i = i + 1\n    return sum\n\nfn main():\n    let result = sum_up_to(10)\n    print(result)\n    return 0",
        );
    }

    #[test]
    fn test_doc_complex_control_flow() {
        assert_compiles(
            "import std\n\nfn find_first_even(start, end):\n    while start < end:\n        if start % 2 == 0:\n            return start\n        start = start + 1\n    return 0\n\nfn main():\n    let result = find_first_even(1, 10)\n    print(result)\n    return 0",
        );
    }

    // ========================
    // TYPE ALIASES
    // ========================

    #[test]
    fn test_doc_type_aliases() {
        assert_compiles(
            "import std\ntype String = str\ntype MyInt = i32\n\nfn main():\n    let s: String = \"hello\"\n    print(s)\n    return 0",
        );
    }

    // ========================
    // EXTERN FUNCTION
    // ========================

    #[test]
    fn test_doc_extern_function() {
        assert_ir_contains(
            "extern fn malloc(size: i32) -> ptr\n\nfn main():\n    let ptr = malloc(100)\n    return 0",
            "declare i8* @malloc",
        );
    }

    // ========================
    // LANGUAGE REFERENCE EXAMPLES
    // ========================

    #[test]
    fn test_doc_langref_if_else() {
        assert_compiles(
            "import std\nfn main():\n    let x = 0\n    if x > 0:\n        return 1\n    elif x == 0:\n        return 0\n    else:\n        return -1",
        );
    }

    #[test]
    fn test_doc_langref_while_loop() {
        assert_compiles(
            "fn main():\n    let counter = 5\n    while counter > 0:\n        counter = counter - 1\n    return 0",
        );
    }

    #[test]
    fn test_doc_langref_for_countdown() {
        assert_compiles("fn main():\n    for i = 5:\n        i\n    return 0");
    }

    #[test]
    fn test_doc_langref_for_in() {
        assert_compiles(
            "import std\nfn main():\n    let items = [1, 2, 3]\n    for item in items:\n        print(item)\n    return 0",
        );
    }

    #[test]
    fn test_doc_langref_break() {
        assert_compiles(
            "fn main():\n    for i = 10:\n        if i == 5:\n            break\n    return 0",
        );
    }

    #[test]
    fn test_doc_langref_continue() {
        assert_compiles(
            "import std\nfn main():\n    let x = 5\n    while x > 0:\n        x = x - 1\n        if x == 3:\n            continue\n        print(x)\n    return 0",
        );
    }

    #[test]
    fn test_doc_langref_struct_definition() {
        assert_compiles(
            "import std\nstruct Point:\n    let x = 0\n    let y = 0\n\nfn main():\n    let p = Point { x: 1, y: 2 }\n    print(p.x)\n    p.y = 3\n    return 0",
        );
    }

    #[test]
    fn test_doc_langref_borrowing() {
        assert_compiles("fn main():\n    let x = 10\n    let r = &x\n    return 0");
    }

    #[test]
    fn test_doc_langref_lists() {
        assert_compiles(
            "import std\nfn main():\n    let nums = [1, 2, 3]\n    let first = nums[0]\n    nums[1] = 5\n    return 0",
        );
    }

    #[test]
    fn test_doc_langref_tuples() {
        assert_compiles(
            "import std\nfn main():\n    let pair = (1, \"hello\")\n    let first = pair.0\n    return 0",
        );
    }

    #[test]
    fn test_doc_langref_extern_fn() {
        assert_compiles(
            "extern fn malloc(size: i32) -> ptr\n\nfn main():\n    let ptr = malloc(100)\n    return 0",
        );
    }

    #[test]
    fn test_doc_langref_type_alias() {
        assert_compiles(
            "import std\ntype String = str\ntype MyInt = i32\n\nfn main():\n    let s: String = \"hello\"\n    print(s)\n    return 0",
        );
    }

    // ========================
    // BORROW CHECKER EXAMPLES
    // ========================

    #[test]
    fn test_doc_borrow_single_owner() {
        assert_compiles("fn main():\n    let a = 10\n    let b = a\n    return 0");
    }

    #[test]
    fn test_doc_borrow_multiple_immutable() {
        assert_compiles(
            "fn main():\n    let a = 10\n    let r1 = &a\n    let r2 = &a\n    return 0",
        );
    }

    #[test]
    fn test_doc_borrow_mutable_exclusive() {
        assert_compiles("fn main():\n    let a = 10\n    let m = &mut a\n    return 0");
    }

    #[test]
    fn test_doc_borrow_no_mixed() {
        assert_fails(
            "fn main():\n    let a = 10\n    let r = &a\n    let m = &mut a\n    return 0",
            "mutably borrow",
        );
    }

    #[test]
    fn test_doc_borrow_move_after_borrow_fails() {
        assert_fails(
            "fn main():\n    let s = \"hello\"\n    let r = &s\n    let t = s\n    return 0",
            "move",
        );
    }

    #[test]
    fn test_doc_borrow_double_mutable_fails() {
        assert_fails(
            "fn main():\n    let a = 10\n    let m1 = &mut a\n    let m2 = &mut a\n    return 0",
            "mutably borrow",
        );
    }

    // ========================
    // STD LIB EXAMPLES
    // ========================

    #[test]
    fn test_doc_stdlib_print() {
        assert_ir_contains(
            "import std\nfn main():\n    print(\"Hello\")\n    return 0",
            "@__rt_print_str",
        );
    }

    #[test]
    fn test_doc_stdlib_string_methods() {
        assert_compiles(
            "import std\nfn main():\n    let s = \"hello\"\n    let upper = to_uppercase(s)\n    let lower = to_lowercase(upper)\n    let trimmed = trim(s)\n    let repeated = str_repeat(s, 2)\n    return 0",
        );
    }

    #[test]
    fn test_doc_stdlib_math() {
        assert_compiles(
            "import std\nfn main():\n    let a = abs(-5)\n    let b = sqrt(16.0)\n    let c = min(1, 2)\n    let d = max(1, 2)\n    let e = pow(2, 3)\n    return 0",
        );
    }

    #[test]
    fn test_doc_stdlib_parsing() {
        assert_compiles(
            "import std\nfn main():\n    let i = to_int(\"42\")\n    let f = to_float(\"3.14\")\n    let h = to_hex(255)\n    return 0",
        );
    }

    #[test]
    fn test_doc_stdlib_len() {
        assert_ir_contains(
            "import std\nfn main():\n    let l = len(\"hello\")\n    return 0",
            "@__rt_strlen",
        );
    }

    #[test]
    fn test_doc_stdlib_is_empty() {
        assert_compiles(
            "import std\nfn main():\n    let empty = is_empty(\"\")\n    let not_empty = is_empty(\"hello\")\n    return 0",
        );
    }

    #[test]
    fn test_doc_stdlib_string_search() {
        assert_compiles(
            "import std\nfn main():\n    let s = \"hello world\"\n    let sub = substr(s, 0, 5)\n    let has = contains(s, \"world\")\n    let starts = starts_with(s, \"hello\")\n    let ends = ends_with(s, \"world\")\n    return 0",
        );
    }

    #[test]
    fn test_doc_stdlib_to_string() {
        assert_compiles("import std\nfn main():\n    let s = to_string(42)\n    return 0");
    }

    #[test]
    fn test_doc_stdlib_list_functions() {
        assert_compiles(
            "import std\nfn main():\n    let lst = [1, 2, 3]\n    let len = list_len(lst)\n    return 0",
        );
    }

    // ========================
    // COMPOUND ASSIGNMENT
    // ========================

    #[test]
    fn test_doc_compound_assignment() {
        assert_compiles(
            "fn main():\n    let x = 10\n    x += 5\n    x -= 2\n    x *= 3\n    x /= 2\n    x %= 3\n    return 0",
        );
    }

    // ========================
    // RETURN VALUES
    // ========================

    #[test]
    fn test_doc_function_return_values() {
        assert_compiles(
            "import std\nfn add(x: i32, y: i32) -> i32:\n    return x + y\n\nfn main():\n    let result = add(1, 2)\n    return result",
        );
    }

    // ========================
    // STRING CONCATENATION
    // ========================

    #[test]
    fn test_doc_string_concatenation() {
        assert_compiles(
            "import std\nfn main():\n    let s = \"hello\"\n    let t = s + \" world\"\n    return 0",
        );
    }

    // ========================
    // UNARY OPERATORS
    // ========================

    #[test]
    fn test_doc_unary_operators() {
        assert_compiles(
            "fn main():\n    let x = -5\n    let y = +3\n    let flag = not true\n    return 0",
        );
    }

    // ========================
    // CHAINED COMPARISONS
    // ========================

    #[test]
    fn test_doc_chained_comparisons() {
        assert_compiles("fn main():\n    let x = 5\n    let result = 0 < x < 10\n    return 0");
    }

    // ========================
    // POWER OPERATOR
    // ========================

    #[test]
    fn test_doc_power_operator() {
        assert_compiles(
            "fn main():\n    let a = 2\n    let b = 3\n    let c = a ** b\n    return 0",
        );
    }

    // ========================
    // IMPLICIT MULTIPLICATION
    // ========================

    #[test]
    fn test_doc_implicit_multiplication() {
        assert_compiles(
            "fn main():\n    let x = 2\n    let y = 3*x\n    let z = 2*(x + 1)\n    return 0",
        );
    }

    // ========================
    // LIST INDEX ASSIGNMENT
    // ========================

    #[test]
    fn test_doc_list_index_assignment() {
        assert_compiles(
            "import std\nfn main():\n    let nums = [1, 2, 3]\n    nums[0] = 10\n    nums[1] = 20\n    nums[2] = 30\n    return 0",
        );
    }

    // ========================
    // NESTED FUNCTION CALLS
    // ========================

    #[test]
    fn test_doc_nested_function_calls() {
        assert_compiles(
            "import std\nfn outer(x: i32) -> i32:\n    return inner(x)\n\nfn inner(y: i32) -> i32:\n    return y + 1\n\nfn main():\n    let result = outer(5)\n    print(result)\n    return 0",
        );
    }

    // ========================
    // VOID FUNCTION
    // ========================

    #[test]
    fn test_doc_void_function() {
        assert_compiles(
            "import std\nfn greet():\n    print(\"Hello\")\n\nfn main():\n    greet()\n    return 0",
        );
    }

    // ========================
    // VARIABLE SHADOWING
    // ========================

    #[test]
    fn test_doc_variable_shadowing() {
        assert_compiles(
            "import std\nfn main():\n    let x = 10\n    let x = 20\n    print(x)\n    return 0",
        );
    }

    // ========================
    // STRUCT FIELD UPDATE
    // ========================

    #[test]
    fn test_doc_struct_field_update() {
        assert_compiles(
            "import std\nstruct Point:\n    let x = 0\n    let y = 0\n\nfn main():\n    let p = Point { x: 1, y: 2 }\n    p.x = 10\n    p.y = 20\n    return 0",
        );
    }

    // ========================
    // ENUM WITH DATA
    // ========================

    #[test]
    fn test_doc_enum_with_data() {
        assert_compiles(
            "import std\nenum Result:\n    Ok(i32)\n    Err(str)\n\nfn main():\n    let r = Result::Ok(42)\n    print(r)\n    return 0",
        );
    }

    // ========================
    // TUPLE ACCESS
    // ========================

    #[test]
    fn test_doc_tuple_access_fields() {
        assert_compiles(
            "import std\nfn main():\n    let pair = (1, \"hello\")\n    print(pair.0)\n    print(pair.1)\n    return 0",
        );
    }

    // ========================
    // IMPORT OTHER MODULE
    // ========================

    #[test]
    fn test_doc_multiple_imports() {
        let temp_dir = temp_root();
        let math_mod = temp_dir.join("math.ro");
        let input = temp_dir.join("input.ron");
        let output = temp_dir.join("output.ll");

        fs::write(&math_mod, "fn double(x: i32) -> i32:\n    return x * 2\n").unwrap();
        fs::write(
            &input,
            "import std\nimport \"./math.ro\"\n\nfn main():\n    let result = double(5)\n    print(result)\n    return 0\n",
        )
        .unwrap();

        let result = run_bin_in_dir(
            &[
                "-i",
                input.to_str().unwrap(),
                "-o",
                output.to_str().unwrap(),
            ],
            &temp_dir,
        );

        assert!(
            result.status.success(),
            "multiple imports failed: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        let ir = fs::read_to_string(&output).unwrap();
        assert!(ir.contains("__double(i32"));

        let _ = fs::remove_file(input);
        let _ = fs::remove_file(math_mod);
        let _ = fs::remove_file(output);
    }

    // ========================
    // GENERIC STRUCT
    // ========================

    #[test]
    fn test_doc_generic_struct() {
        assert_compiles(
            "struct Box<T>:\n    let value = 0\n\nfn main():\n    let b = Box { value: 42 }\n    return 0",
        );
    }

    // ========================
    // LIST LITERAL COMPILES
    // ========================

    #[test]
    fn test_doc_list_literal() {
        assert_compiles(
            "import std\nfn main():\n    let nums = [1, 2, 3]\n    let first = nums[0]\n    print(first)\n    return 0",
        );
    }

    // ========================
    // FLOAT LIST
    // ========================

    #[test]
    fn test_doc_list_float() {
        assert_compiles("fn main():\n    let nums = [1.5, 2.5, 3.5]\n    return 0");
    }

    // ========================
    // WHILE AS ONLY STATEMENT
    // ========================

    #[test]
    fn test_doc_while_loop_only() {
        assert_compiles("fn main():\n    while 1:\n        return 0");
    }

    // ========================
    // CONST DECLARATION
    // ========================

    #[test]
    fn test_doc_const_declaration() {
        assert_compiles("import std\nfn main():\n    const x = 42\n    print(x)\n    return 0");
    }

    // ========================
    // INTEGER COMPARISONS
    // ========================

    #[test]
    fn test_doc_int_comparisons() {
        assert_compiles(
            "import std\nfn main():\n    let a = 10\n    let b = 5\n    print(a == b)\n    print(a != b)\n    print(a < b)\n    print(a > b)\n    print(a <= b)\n    print(a >= b)\n    return 0",
        );
    }

    // ========================
    // LOGICAL OPERATORS
    // ========================

    #[test]
    fn test_doc_logical_operators() {
        assert_compiles(
            "import std\nfn main():\n    let p = true\n    let q = false\n    print(p and q)\n    print(p or q)\n    print(not p)\n    return 0",
        );
    }

    // ========================
    // BREAK OUTSIDE LOOP FAILS
    // ========================

    #[test]
    fn test_doc_break_outside_loop() {
        assert_fails("fn main():\n    break", "break outside of loop");
    }

    // ========================
    // CONTINUE OUTSIDE LOOP FAILS
    // ========================

    #[test]
    fn test_doc_continue_outside_loop() {
        assert_fails("fn main():\n    continue", "continue outside of loop");
    }
}
