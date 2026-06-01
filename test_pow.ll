; ModuleID = 'righton_test_pow'
declare i32 @printf(i8*, ...)
declare i32 @sprintf(i8*, i8*, ...)
declare double @llvm.pow.f64(double, double)
declare i64 @strlen(i8*)
declare i8* @fopen(i8*, i8*)
declare i32 @fseek(i8*, i64, i32)
declare i64 @ftell(i8*)
declare void @rewind(i8*)
declare i8* @malloc(i64)
declare void @free(i8*)
declare i64 @fread(i8*, i64, i64, i8*)
declare i32 @fclose(i8*)
declare i64 @fwrite(i8*, i64, i64, i8*)
declare void @exit(i32)
declare i8* @strstr(i8*, i8*)
declare i32 @strncmp(i8*, i8*, i64)
declare i64 @strspn(i8*, i8*)
declare i64 @strcspn(i8*, i8*)
declare i32 @toupper(i32)
declare i32 @tolower(i32)
declare i32 @atoi(i8*)
declare double @atof(i8*)
declare double @floor(double)
declare double @ceil(double)
declare double @round(double)
declare i8* @fgets(i8*, i32, i8*)
@stdin = external global i8*
@.global_buffer = private global [1024 x i8] zeroinitializer

@.str0 = private unnamed_addr constant [1 x i8] c"\00"
@.str1 = private unnamed_addr constant [3 x i8] c"\72\62\00"
@.str2 = private unnamed_addr constant [3 x i8] c"\77\62\00"
@.str3 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str4 = private unnamed_addr constant [4 x i8] c"\25\64\0A\00"
@.str5 = private unnamed_addr constant [4 x i8] c"\25\66\0A\00"
@.str6 = private unnamed_addr constant [7 x i8] c"\20\09\0A\0D\0C\0B\00"
@.str7 = private unnamed_addr constant [3 x i8] c"\25\64\00"
@.str8 = private unnamed_addr constant [3 x i8] c"\25\66\00"
@.str16 = private unnamed_addr constant [6 x i8] c"\68\65\6C\6C\6F\00"
@.str18 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
define i32 @__rt_strlen(i8* %s) {
entry:
  %len = call i64 @strlen(i8* %s)
  %tr = trunc i64 %len to i32
  ret i32 %tr
}

define void @__rt_print_str(i8* %s) {
entry:
  %fmt = getelementptr [4 x i8], [4 x i8]* @.str3, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %fmt, i8* %s)
  ret void
}

define void @__rt_print_int(i32 %n) {
entry:
  %fmt = getelementptr [4 x i8], [4 x i8]* @.str4, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %fmt, i32 %n)
  ret void
}

define void @__rt_print_float(double %n) {
entry:
  %fmt = getelementptr [4 x i8], [4 x i8]* @.str5, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %fmt, double %n)
  ret void
}

define void @__rt_exit(i32 %code) {
entry:
  call void @exit(i32 %code)
  ret void
}

define i8* @__rt_read_file(i8* %path) {
entry:
  %mode = getelementptr [3 x i8], [3 x i8]* @.str1, i32 0, i32 0
  %file = call i8* @fopen(i8* %path, i8* %mode)
  %null = icmp eq i8* %file, null
  br i1 %null, label %empty, label %read
empty:
  %buf0 = call i8* @malloc(i64 1)
  store i8 0, i8* %buf0
  ret i8* %buf0
read:
  %seek = call i32 @fseek(i8* %file, i64 0, i32 2)
  %size = call i64 @ftell(i8* %file)
  call void @rewind(i8* %file)
  %alloc = add i64 %size, 1
  %buf = call i8* @malloc(i64 %alloc)
  %readn = call i64 @fread(i8* %buf, i64 1, i64 %size, i8* %file)
  %end = getelementptr i8, i8* %buf, i64 %readn
  store i8 0, i8* %end
  call i32 @fclose(i8* %file)
  ret i8* %buf
}

define i32 @__rt_write_file(i8* %path, i8* %contents) {
entry:
  %mode = getelementptr [3 x i8], [3 x i8]* @.str2, i32 0, i32 0
  %file = call i8* @fopen(i8* %path, i8* %mode)
  %null = icmp eq i8* %file, null
  br i1 %null, label %fail, label %write
fail:
  ret i32 -1
write:
  %len = call i64 @strlen(i8* %contents)
  %written = call i64 @fwrite(i8* %contents, i64 1, i64 %len, i8* %file)
  call i32 @fclose(i8* %file)
  %ok = icmp eq i64 %written, %len
  %res = select i1 %ok, i32 0, i32 -1
  ret i32 %res
}

define i32 @__rt_contains(i8* %s, i8* %sub) {
entry:
  %res = call i8* @strstr(i8* %s, i8* %sub)
  %found = icmp ne i8* %res, null
  %ret = zext i1 %found to i32
  ret i32 %ret
}

define i32 @__rt_starts_with(i8* %s, i8* %prefix) {
entry:
  %plen = call i64 @strlen(i8* %prefix)
  %res = call i32 @strncmp(i8* %s, i8* %prefix, i64 %plen)
  %eq = icmp eq i32 %res, 0
  %ret = zext i1 %eq to i32
  ret i32 %ret
}

define i32 @__rt_ends_with(i8* %s, i8* %suffix) {
entry:
  %slen = call i64 @strlen(i8* %s)
  %sublen = call i64 @strlen(i8* %suffix)
  %tooshort = icmp ult i64 %slen, %sublen
  br i1 %tooshort, label %false, label %check
check:
  %off = sub i64 %slen, %sublen
  %start = getelementptr i8, i8* %s, i64 %off
  %res = call i32 @strncmp(i8* %start, i8* %suffix, i64 %sublen)
  %eq = icmp eq i32 %res, 0
  %ret = zext i1 %eq to i32
  ret i32 %ret
false:
  ret i32 0
}

define i8* @__rt_substr(i8* %s, i32 %start, i32 %length) {
entry:
  %len64 = sext i32 %length to i64
  %alloc = add i64 %len64, 1
  %buf = call i8* @malloc(i64 %alloc)
  %start64 = sext i32 %start to i64
  %src = getelementptr i8, i8* %s, i64 %start64
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %buf, i8* %src, i64 %len64, i1 false)
  %end = getelementptr i8, i8* %buf, i64 %len64
  store i8 0, i8* %end
  ret i8* %buf
}

define i8* @__rt_trim(i8* %s) {
entry:
  %len = call i64 @strlen(i8* %s)
  %ws = getelementptr [6 x i8], [6 x i8]* @.str6, i32 0, i32 0
  %start_off = call i64 @strspn(i8* %s, i8* %ws)
  %remaining = sub i64 %len, %start_off
  %start_ptr = getelementptr i8, i8* %s, i64 %start_off
  %end_off = call i64 @strcspn(i8* %start_ptr, i8* %ws)
  %trim_len = call i64 @llvm.umin.i64(i64 %remaining, i64 %end_off)
  %alloc = add i64 %trim_len, 1
  %buf = call i8* @malloc(i64 %alloc)
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %buf, i8* %start_ptr, i64 %trim_len, i1 false)
  %end = getelementptr i8, i8* %buf, i64 %trim_len
  store i8 0, i8* %end
  ret i8* %buf
}

define i8* @__rt_to_uppercase(i8* %s) {
entry:
  %len = call i64 @strlen(i8* %s)
  %alloc = add i64 %len, 1
  %buf = call i8* @malloc(i64 %alloc)
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %buf, i8* %s, i64 %alloc, i1 false)
  br label %loop
loop:
  %pos = phi i64 [0, %entry], [%next, %body]
  %cmp = icmp eq i64 %pos, %len
  br i1 %cmp, label %done, label %body
body:
  %ptr = getelementptr i8, i8* %buf, i64 %pos
  %ch = load i8, i8* %ptr
  %ch32 = sext i8 %ch to i32
  %up = call i32 @toupper(i32 %ch32)
  %up8 = trunc i32 %up to i8
  store i8 %up8, i8* %ptr
  %next = add i64 %pos, 1
  br label %loop
done:
  ret i8* %buf
}

define i8* @__rt_to_lowercase(i8* %s) {
entry:
  %len = call i64 @strlen(i8* %s)
  %alloc = add i64 %len, 1
  %buf = call i8* @malloc(i64 %alloc)
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %buf, i8* %s, i64 %alloc, i1 false)
  br label %loop
loop:
  %pos = phi i64 [0, %entry], [%next, %body]
  %cmp = icmp eq i64 %pos, %len
  br i1 %cmp, label %done, label %body
body:
  %ptr = getelementptr i8, i8* %buf, i64 %pos
  %ch = load i8, i8* %ptr
  %ch32 = sext i8 %ch to i32
  %lo = call i32 @tolower(i32 %ch32)
  %lo8 = trunc i32 %lo to i8
  store i8 %lo8, i8* %ptr
  %next = add i64 %pos, 1
  br label %loop
done:
  ret i8* %buf
}

define i32 @__rt_to_int(i8* %s) {
entry:
  %res = call i32 @atoi(i8* %s)
  ret i32 %res
}

define double @__rt_to_float(i8* %s) {
entry:
  %res = call double @atof(i8* %s)
  ret double %res
}

define double @__rt_floor(double %n) {
entry:
  %res = call double @floor(double %n)
  ret double %res
}

define double @__rt_ceil(double %n) {
entry:
  %res = call double @ceil(double %n)
  ret double %res
}

define double @__rt_round(double %n) {
entry:
  %res = call double @round(double %n)
  ret double %res
}

define i8* @__rt_read_line() {
entry:
  %buf = call i8* @malloc(i64 1024)
  %stdin_ptr = load i8*, i8** @stdin
  %res = call i8* @fgets(i8* %buf, i32 1024, i8* %stdin_ptr)
  %null = icmp eq i8* %res, null
  br i1 %null, label %empty, label %strip_nl
empty:
  store i8 0, i8* %buf
  ret i8* %buf
strip_nl:
  %len = call i64 @strlen(i8* %buf)
  %last_off = sub i64 %len, 1
  %last_ptr = getelementptr i8, i8* %buf, i64 %last_off
  %last_ch = load i8, i8* %last_ptr
  %is_nl = icmp eq i8 %last_ch, 10
  br i1 %is_nl, label %strip, label %ret
strip:
  store i8 0, i8* %last_ptr
  br label %ret
ret:
  ret i8* %buf
}

define i8* @__rt_to_string_int(i32 %n) {
entry:
  %buf = call i8* @malloc(i64 64)
  %fmt = getelementptr [3 x i8], [3 x i8]* @.str7, i32 0, i32 0
  call i32 (i8*, i8*, ...) @sprintf(i8* %buf, i8* %fmt, i32 %n)
  ret i8* %buf
}

define i8* @__rt_to_string_float(double %n) {
entry:
  %buf = call i8* @malloc(i64 64)
  %fmt = getelementptr [3 x i8], [3 x i8]* @.str8, i32 0, i32 0
  call i32 (i8*, i8*, ...) @sprintf(i8* %buf, i8* %fmt, double %n)
  ret i8* %buf
}

define void @__rt_free(i8* %ptr) {
entry:
  %null = icmp eq i8* %ptr, null
  br i1 %null, label %done, label %free
free:
  call void @free(i8* %ptr)
  br label %done
done:
  ret void
}

define i32 @__rt_list_len(i8* %list) {
entry:
  %len_ptr = getelementptr i8, i8* %list, i32 4
  %len_i32 = bitcast i8* %len_ptr to i32*
  %len = load i32, i32* %len_i32
  ret i32 %len
}

define i8* @__rt_list_push(i8* %list, i32 %val) {
entry:
  %cap_ptr = bitcast i8* %list to i32*
  %cap = load i32, i32* %cap_ptr
  %len_gep = getelementptr i8, i8* %list, i32 4
  %len_ptr = bitcast i8* %len_gep to i32*
  %len = load i32, i32* %len_ptr
  %needs_grow = icmp eq i32 %len, %cap
  br i1 %needs_grow, label %grow, label %store
grow:
  %newcap = mul i32 %cap, 2
  %cap_zero = icmp eq i32 %newcap, 0
  %newcap2 = select i1 %cap_zero, i32 4, i32 %newcap
  %old_data_bytes = mul i32 %cap, 4
  %old_total = add i32 %old_data_bytes, 8
  %old_total_i64 = sext i32 %old_total to i64
  %new_data_bytes = mul i32 %newcap2, 4
  %new_total = add i32 %new_data_bytes, 8
  %new_total_i64 = sext i32 %new_total to i64
  %newlist = call i8* @malloc(i64 %new_total_i64)
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %newlist, i8* %list, i64 %old_total_i64, i1 false)
  call void @free(i8* %list)
  store i32 %newcap2, i32* %newlist
  br label %store
store:
  %cur_list = phi i8* [%list, %entry], [%newlist, %grow]
  %cur_len = phi i32 [%len, %entry], [%len, %grow]
  %elem_off = mul i32 %cur_len, 4
  %elem_off_total = add i32 %elem_off, 8
  %elem_gep = getelementptr i8, i8* %cur_list, i32 %elem_off_total
  %elem_ptr = bitcast i8* %elem_gep to i32*
  store i32 %val, i32* %elem_ptr
  %new_len = add i32 %cur_len, 1
  %len_store_gep = getelementptr i8, i8* %cur_list, i32 4
  %len_store_ptr = bitcast i8* %len_store_gep to i32*
  store i32 %new_len, i32* %len_store_ptr
  ret i8* %cur_list
}

define i32 @__rt_list_pop(i8* %list) {
entry:
  %len_gep = getelementptr i8, i8* %list, i32 4
  %len_ptr = bitcast i8* %len_gep to i32*
  %len = load i32, i32* %len_ptr
  %is_empty = icmp eq i32 %len, 0
  br i1 %is_empty, label %empty, label %pop
empty:
  ret i32 0
pop:
  %new_len = sub i32 %len, 1
  %elem_off = mul i32 %new_len, 4
  %elem_off_total = add i32 %elem_off, 8
  %elem_gep = getelementptr i8, i8* %list, i32 %elem_off_total
  %elem_ptr = bitcast i8* %elem_gep to i32*
  %val = load i32, i32* %elem_ptr
  store i32 %new_len, i32* %len_ptr
  ret i32 %val
}

declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg)

declare i64 @llvm.umin.i64(i64, i64)

define void @std__print_int(i32 %arg0) {
entry:
  %arg0_alloca = alloca i32
  store i32 %arg0, i32* %arg0_alloca
  %v9 = load i32, i32* %arg0_alloca
  call void @__rt_print_int(i32 %v9)
  ret void
}

define void @std__print_float(double %arg0) {
entry:
  %arg0_alloca = alloca double
  store double %arg0, double* %arg0_alloca
  %v10 = load double, double* %arg0_alloca
  call void @__rt_print_float(double %v10)
  ret void
}

define void @std__print(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v11 = load i8*, i8** %arg0_alloca
  call void @__rt_print_str(i8* %v11)
  ret void
}

define i32 @main() {
entry:
  %v12 = alloca i32
  store i32 42, i32* %v12
  %v13 = load i32, i32* %v12
  call void @std__print_int(i32 %v13)
  %v15 = getelementptr [6 x i8], [6 x i8]* @.str16, i32 0, i32 0
  %v14 = alloca i8*
  store i8* %v15, i8** %v14
  %v17 = load i8*, i8** %v14
  %v19 = getelementptr [4 x i8], [4 x i8]* @.str18, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v19, i8* %v17)
  %v20 = alloca double
  store double 3.14, double* %v20
  %v21 = load double, double* %v20
  call void @std__print_float(double %v21)
  ret i32 0
}

