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
    let triple_ptr = unsafe { LLVMGetDefaultTargetTriple() };
    let triple_str = unsafe { CStr::from_ptr(triple_ptr).to_str().unwrap() };
    let triple = CString::new(cli.target.unwrap_or(triple_str.to_string())).unwrap();
    unsafe {
        LLVMDisposeMessage(triple_ptr);
    }

    let cpu = CString::new(cli.cpu.unwrap_or("generic".to_string())).unwrap();
    let features = CString::new("").unwrap();

    let mut target = ptr::null_mut();
    let mut error = ptr::null_mut();
    unsafe {
        if LLVMGetTargetFromTriple(triple.as_ptr(), &mut target, &mut error) != 0 {
            let msg = CStr::from_ptr(error).to_str().unwrap();
            eprintln!("Error getting target: {}", msg);
            LLVMDisposeMessage(error);
            return;
        }
    }

    // I don't know what it's even doing, but if it works, don't touch it
    let target_machine = unsafe {
        LLVMCreateTargetMachine(
            target,
            triple.as_ptr(),
            cpu.as_ptr(),
            features.as_ptr(),
            if cli
                .codegen_level
                .clone()
                .unwrap_or("default".parse().unwrap())
                == "default"
                && cli.codegen_level == None
            {
                LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault
            } else if cli.codegen_level.clone().unwrap() == "aggressive" {
                LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive
            } else if cli.codegen_level.unwrap() == "less" {
                LLVMCodeGenOptLevel::LLVMCodeGenLevelLess
            } else {
                LLVMCodeGenOptLevel::LLVMCodeGenLevelNone
            },
            if cli.no_pie {
                LLVMRelocMode::LLVMRelocDefault
            } else {
                LLVMRelocMode::LLVMRelocPIC
            },
            LLVMCodeModel::LLVMCodeModelDefault,
        )
    };

    if target_machine.is_null() {
        panic!("Failed to create target machine!");
    }

    let output_file = CString::new(output_path.to_string()).unwrap();

    let context = unsafe { LLVMContextCreate() };
    let mem_buf = unsafe {
        LLVMCreateMemoryBufferWithMemoryRangeCopy(
            ir.as_ptr() as *const i8,
            ir.len(),
            format!("{}\0", name).into_bytes().as_ptr() as *const i8,
        )
    };

    let mut module_ptr = ptr::null_mut();
    let mut error_message = ptr::null_mut();

    let parse_result =
        unsafe { LLVMParseIRInContext(context, mem_buf, &mut module_ptr, &mut error_message) };
    if parse_result != 0 {
        let msg = unsafe { CStr::from_ptr(error_message).to_string_lossy() };
        panic!("Failed to parse IR: {}", msg);
    }

    let result = unsafe {
        LLVMTargetMachineEmitToFile(
            target_machine,
            module_ptr,
            output_file.as_ptr() as *mut _, // cast to *mut i8
            LLVMCodeGenFileType::LLVMObjectFile,
            &mut error_message,
        )
    };

    unsafe {
        if result != 0 {
            let error_str = CStr::from_ptr(error_message).to_string_lossy();
            eprintln!("Error emitting object file: {}", error_str);
            LLVMDisposeMessage(error_message);
        }
    }

    unsafe {
        LLVMDisposeModule(module_ptr);
        LLVMContextDispose(context);
        LLVMDisposeTargetMachine(target_machine);
    }
}
