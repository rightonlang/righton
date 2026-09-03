fn main() {
    // Build libro C runtime into a static library.
    // This is used for Righton programs at link time, not for the
    // compiler itself. We compile it here so `cargo build` always
    // ensures libro/libro.a is up-to-date and also emit it to OUT_DIR
    // for potential embedding.
    let libro_c = "libro/src/libro.c";
    if std::path::Path::new(libro_c).exists() {
        cc::Build::new()
            .file(libro_c)
            .include("libro/include")
            .opt_level(2)
            .flag_if_supported("-Wall")
            .flag_if_supported("-Wextra")
            .compile("ro_runtime");

        // Also ensure libro/libro.a exists for manual gcc linking
        // by invoking `make -C libro` if `cc` succeeded.
        // The `cc` crate already produced a library in OUT_DIR, but
        // users expect `libro/libro.a` for `gcc example.o libro/libro.a`.
        let _ = std::process::Command::new("make")
            .arg("-C")
            .arg("libro")
            .status();
    }

    println!("cargo:rerun-if-changed=libro/src/libro.c");
    println!("cargo:rerun-if-changed=libro/include/libro.h");
    println!("cargo:rerun-if-changed=libro/Makefile");
}
