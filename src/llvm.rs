use crate::Cli;
use llvm_sys::core::{
    LLVMContextCreate, LLVMContextDispose, LLVMCreateMemoryBufferWithMemoryRangeCopy,
    LLVMDisposeMessage, LLVMDisposeModule,
};
use llvm_sys::ir_reader::LLVMParseIRInContext;
use llvm_sys::target_machine::{
    LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine,
    LLVMDisposeTargetMachine, LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple, LLVMRelocMode,
    LLVMTargetMachineEmitToFile,
};
use std::ffi::{CStr, CString};
use std::ptr;

pub(crate) fn compile_object(cli: Cli, ir: String, output_path: String, name: String) {
    let default_triple = unsafe {
        let ptr = LLVMGetDefaultTargetTriple();
        let s = CStr::from_ptr(ptr).to_str().unwrap().to_string();
        LLVMDisposeMessage(ptr);
        s
    };

    let triple_str = cli.target.unwrap_or(default_triple);
    let triple = CString::new(triple_str.clone()).unwrap();

    let cpu = CString::new(cli.cpu.unwrap_or("generic".to_string())).unwrap();
    let features = CString::new("").unwrap();

    let mut target = ptr::null_mut();
    let mut error = ptr::null_mut();

    unsafe {
        if LLVMGetTargetFromTriple(triple.as_ptr(), &mut target, &mut error) != 0 {
            let msg = CStr::from_ptr(error).to_string_lossy();
            eprintln!("Target not found for triple '{}': {}", triple_str, msg);
            LLVMDisposeMessage(error);
            return;
        }
    }

    let opt_level = match cli.codegen_level.as_deref() {
        Some("aggressive") => LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
        Some("less") => LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
        Some("none") => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
        _ => LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
    };

    let reloc_mode = if cli.no_pie {
        LLVMRelocMode::LLVMRelocDefault
    } else {
        LLVMRelocMode::LLVMRelocPIC
    };

    let target_machine = unsafe {
        LLVMCreateTargetMachine(
            target,
            triple.as_ptr(),
            cpu.as_ptr(),
            features.as_ptr(),
            opt_level,
            reloc_mode,
            LLVMCodeModel::LLVMCodeModelDefault,
        )
    };

    if target_machine.is_null() {
        panic!("Failed to create target machine for triple '{}'", triple_str);
    }

    let output_file = CString::new(output_path.clone()).unwrap();

    let context = unsafe { LLVMContextCreate() };
    let mem_buf = unsafe {
        LLVMCreateMemoryBufferWithMemoryRangeCopy(
            ir.as_ptr() as *const i8,
            ir.len(),
            format!("{}\0", name).as_ptr() as *const i8,
        )
    };

    let mut module_ptr = ptr::null_mut();
    let mut parse_error = ptr::null_mut();

    let parse_result =
        unsafe { LLVMParseIRInContext(context, mem_buf, &mut module_ptr, &mut parse_error) };
    if parse_result != 0 {
        let msg = unsafe { CStr::from_ptr(parse_error).to_string_lossy() };
        panic!("Failed to parse IR: {}", msg);
    }

    let mut emit_error = ptr::null_mut();
    let emit_result = unsafe {
        LLVMTargetMachineEmitToFile(
            target_machine,
            module_ptr,
            output_file.as_ptr() as *mut _,
            LLVMCodeGenFileType::LLVMObjectFile,
            &mut emit_error,
        )
    };

    if emit_result != 0 {
        let msg = unsafe { CStr::from_ptr(emit_error).to_string_lossy() };
        eprintln!("Failed to emit object file: {}", msg);
        unsafe { LLVMDisposeMessage(emit_error) };
    }

    unsafe {
        LLVMDisposeModule(module_ptr);
        LLVMContextDispose(context);
        LLVMDisposeTargetMachine(target_machine);
    }
}
