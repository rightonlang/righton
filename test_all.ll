; ModuleID = 'righton_test_all'
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
@.str20 = private unnamed_addr constant [14 x i8] c"\48\65\6C\6C\6F\2C\20\57\6F\72\6C\64\21\00"
@.str22 = private unnamed_addr constant [16 x i8] c"\63\6F\6E\74\61\69\6E\73\20\57\6F\72\6C\64\3A\00"
@.str23 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str27 = private unnamed_addr constant [6 x i8] c"\57\6F\72\6C\64\00"
@.str30 = private unnamed_addr constant [15 x i8] c"\63\6F\6E\74\61\69\6E\73\20\52\75\73\74\3A\00"
@.str31 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str35 = private unnamed_addr constant [5 x i8] c"\52\75\73\74\00"
@.str38 = private unnamed_addr constant [19 x i8] c"\73\74\61\72\74\73\5F\77\69\74\68\20\48\65\6C\6C\6F\3A\00"
@.str39 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str43 = private unnamed_addr constant [6 x i8] c"\48\65\6C\6C\6F\00"
@.str46 = private unnamed_addr constant [19 x i8] c"\73\74\61\72\74\73\5F\77\69\74\68\20\57\6F\72\6C\64\3A\00"
@.str47 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str51 = private unnamed_addr constant [6 x i8] c"\57\6F\72\6C\64\00"
@.str54 = private unnamed_addr constant [18 x i8] c"\65\6E\64\73\5F\77\69\74\68\20\57\6F\72\6C\64\21\3A\00"
@.str55 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str59 = private unnamed_addr constant [7 x i8] c"\57\6F\72\6C\64\21\00"
@.str62 = private unnamed_addr constant [17 x i8] c"\65\6E\64\73\5F\77\69\74\68\20\48\65\6C\6C\6F\3A\00"
@.str63 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str67 = private unnamed_addr constant [6 x i8] c"\48\65\6C\6C\6F\00"
@.str70 = private unnamed_addr constant [12 x i8] c"\73\75\62\73\74\72\20\37\20\35\3A\00"
@.str71 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str78 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str82 = private unnamed_addr constant [16 x i8] c"\20\20\68\65\6C\6C\6F\20\77\6F\72\6C\64\20\20\00"
@.str84 = private unnamed_addr constant [9 x i8] c"\74\72\69\6D\6D\65\64\3A\00"
@.str85 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str90 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str93 = private unnamed_addr constant [11 x i8] c"\75\70\70\65\72\63\61\73\65\3A\00"
@.str94 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str99 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str102 = private unnamed_addr constant [11 x i8] c"\6C\6F\77\65\72\63\61\73\65\3A\00"
@.str103 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str108 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str112 = private unnamed_addr constant [3 x i8] c"\34\32\00"
@.str114 = private unnamed_addr constant [11 x i8] c"\74\6F\5F\69\6E\74\20\34\32\3A\00"
@.str115 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str120 = private unnamed_addr constant [15 x i8] c"\74\6F\5F\66\6C\6F\61\74\20\33\2E\31\34\3A\00"
@.str121 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str124 = private unnamed_addr constant [5 x i8] c"\33\2E\31\34\00"
@.str127 = private unnamed_addr constant [8 x i8] c"\61\62\73\20\2D\35\3A\00"
@.str128 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str132 = private unnamed_addr constant [11 x i8] c"\66\6C\6F\6F\72\20\33\2E\37\3A\00"
@.str133 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str137 = private unnamed_addr constant [10 x i8] c"\63\65\69\6C\20\33\2E\32\3A\00"
@.str138 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str142 = private unnamed_addr constant [11 x i8] c"\72\6F\75\6E\64\20\33\2E\35\3A\00"
@.str143 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str147 = private unnamed_addr constant [11 x i8] c"\6D\69\6E\20\31\30\20\32\30\3A\00"
@.str148 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str152 = private unnamed_addr constant [11 x i8] c"\6D\61\78\20\31\30\20\32\30\3A\00"
@.str153 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str157 = private unnamed_addr constant [10 x i8] c"\70\6F\77\20\32\20\31\30\3A\00"
@.str158 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str165 = private unnamed_addr constant [15 x i8] c"\74\6F\5F\73\74\72\69\6E\67\20\69\6E\74\3A\00"
@.str166 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str169 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str175 = private unnamed_addr constant [17 x i8] c"\74\6F\5F\73\74\72\69\6E\67\20\66\6C\6F\61\74\3A\00"
@.str176 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str179 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str184 = private unnamed_addr constant [7 x i8] c"\64\69\72\65\63\74\00"
@.str186 = private unnamed_addr constant [18 x i8] c"\74\6F\5F\73\74\72\69\6E\67\20\73\74\72\69\6E\67\3A\00"
@.str187 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str190 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str194 = private unnamed_addr constant [1 x i8] c"\00"
@.str196 = private unnamed_addr constant [16 x i8] c"\69\73\5F\65\6D\70\74\79\20\65\6D\70\74\79\3A\00"
@.str197 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str202 = private unnamed_addr constant [18 x i8] c"\69\73\5F\65\6D\70\74\79\20\6C\69\74\65\72\61\6C\3A\00"
@.str203 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str206 = private unnamed_addr constant [1 x i8] c"\00"
@.str209 = private unnamed_addr constant [16 x i8] c"\69\73\5F\65\6D\70\74\79\20\68\65\6C\6C\6F\3A\00"
@.str210 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str213 = private unnamed_addr constant [6 x i8] c"\68\65\6C\6C\6F\00"
@.str216 = private unnamed_addr constant [14 x i8] c"\6C\65\6E\20\67\72\65\65\74\69\6E\67\3A\00"
@.str217 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str223 = private unnamed_addr constant [9 x i8] c"\6E\65\77\6C\69\6E\65\3A\00"
@.str224 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str227 = private unnamed_addr constant [12 x i8] c"\6C\69\6E\65\31\0A\6C\69\6E\65\32\00"
@.str228 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str231 = private unnamed_addr constant [5 x i8] c"\74\61\62\3A\00"
@.str232 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str235 = private unnamed_addr constant [10 x i8] c"\63\6F\6C\31\09\63\6F\6C\32\00"
@.str236 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str239 = private unnamed_addr constant [11 x i8] c"\62\61\63\6B\73\6C\61\73\68\3A\00"
@.str240 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str243 = private unnamed_addr constant [13 x i8] c"\70\61\74\68\5C\74\6F\5C\66\69\6C\65\00"
@.str244 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str247 = private unnamed_addr constant [7 x i8] c"\71\75\6F\74\65\3A\00"
@.str248 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str251 = private unnamed_addr constant [17 x i8] c"\73\68\65\20\73\61\69\64\20\22\68\65\6C\6C\6F\22\00"
@.str252 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
@.str255 = private unnamed_addr constant [18 x i8] c"\41\6C\6C\20\74\65\73\74\73\20\70\61\73\73\65\64\21\00"
@.str256 = private unnamed_addr constant [4 x i8] c"\25\73\0A\00"
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

define void @std__print(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v9 = load i8*, i8** %arg0_alloca
  call void @__rt_print_str(i8* %v9)
  ret void
}

define i8* @std__to_string(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v10 = load i8*, i8** %arg0_alloca
  ret i8* %v10
}

define i8* @std__trim(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v12 = load i8*, i8** %arg0_alloca
  %v13 = call i8* @__rt_trim(i8* %v12)
  %v11 = bitcast i8* %v13 to i8*
  ret i8* %v11
}

define double @std__ceil(double %arg0) {
entry:
  %arg0_alloca = alloca double
  store double %arg0, double* %arg0_alloca
  %v14 = load double, double* %arg0_alloca
  %v15 = call double @__rt_ceil(double %v14)
  ret double %v15
}

define double @std__round(double %arg0) {
entry:
  %arg0_alloca = alloca double
  store double %arg0, double* %arg0_alloca
  %v16 = load double, double* %arg0_alloca
  %v17 = call double @__rt_round(double %v16)
  ret double %v17
}

define i32 @main() {
entry:
  %v19 = getelementptr [14 x i8], [14 x i8]* @.str20, i32 0, i32 0
  %v18 = alloca i8*
  store i8* %v19, i8** %v18
  %v21 = getelementptr [16 x i8], [16 x i8]* @.str22, i32 0, i32 0
  %v24 = getelementptr [4 x i8], [4 x i8]* @.str23, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v24, i8* %v21)
  %v25 = load i8*, i8** %v18
  %v26 = getelementptr [6 x i8], [6 x i8]* @.str27, i32 0, i32 0
  %v28 = call i32 @std__contains(i8* %v25, i8* %v26)
  call void @std__print_int(i32 %v28)
  %v29 = getelementptr [15 x i8], [15 x i8]* @.str30, i32 0, i32 0
  %v32 = getelementptr [4 x i8], [4 x i8]* @.str31, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v32, i8* %v29)
  %v33 = load i8*, i8** %v18
  %v34 = getelementptr [5 x i8], [5 x i8]* @.str35, i32 0, i32 0
  %v36 = call i32 @std__contains(i8* %v33, i8* %v34)
  call void @std__print_int(i32 %v36)
  %v37 = getelementptr [19 x i8], [19 x i8]* @.str38, i32 0, i32 0
  %v40 = getelementptr [4 x i8], [4 x i8]* @.str39, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v40, i8* %v37)
  %v41 = load i8*, i8** %v18
  %v42 = getelementptr [6 x i8], [6 x i8]* @.str43, i32 0, i32 0
  %v44 = call i32 @std__starts_with(i8* %v41, i8* %v42)
  call void @std__print_int(i32 %v44)
  %v45 = getelementptr [19 x i8], [19 x i8]* @.str46, i32 0, i32 0
  %v48 = getelementptr [4 x i8], [4 x i8]* @.str47, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v48, i8* %v45)
  %v49 = load i8*, i8** %v18
  %v50 = getelementptr [6 x i8], [6 x i8]* @.str51, i32 0, i32 0
  %v52 = call i32 @std__starts_with(i8* %v49, i8* %v50)
  call void @std__print_int(i32 %v52)
  %v53 = getelementptr [18 x i8], [18 x i8]* @.str54, i32 0, i32 0
  %v56 = getelementptr [4 x i8], [4 x i8]* @.str55, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v56, i8* %v53)
  %v57 = load i8*, i8** %v18
  %v58 = getelementptr [7 x i8], [7 x i8]* @.str59, i32 0, i32 0
  %v60 = call i32 @std__ends_with(i8* %v57, i8* %v58)
  call void @std__print_int(i32 %v60)
  %v61 = getelementptr [17 x i8], [17 x i8]* @.str62, i32 0, i32 0
  %v64 = getelementptr [4 x i8], [4 x i8]* @.str63, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v64, i8* %v61)
  %v65 = load i8*, i8** %v18
  %v66 = getelementptr [6 x i8], [6 x i8]* @.str67, i32 0, i32 0
  %v68 = call i32 @std__ends_with(i8* %v65, i8* %v66)
  call void @std__print_int(i32 %v68)
  %v69 = getelementptr [12 x i8], [12 x i8]* @.str70, i32 0, i32 0
  %v72 = getelementptr [4 x i8], [4 x i8]* @.str71, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v72, i8* %v69)
  %v75 = load i8*, i8** %v18
  %v76 = call i8* @std__substr(i8* %v75, i32 7, i32 5)
  %v74 = bitcast i8* %v76 to i8*
  %v73 = alloca i8*
  store i8* %v74, i8** %v73
  %v77 = load i8*, i8** %v73
  %v79 = getelementptr [4 x i8], [4 x i8]* @.str78, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v79, i8* %v77)
  %v81 = getelementptr [16 x i8], [16 x i8]* @.str82, i32 0, i32 0
  %v80 = alloca i8*
  store i8* %v81, i8** %v80
  %v83 = getelementptr [9 x i8], [9 x i8]* @.str84, i32 0, i32 0
  %v86 = getelementptr [4 x i8], [4 x i8]* @.str85, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v86, i8* %v83)
  %v88 = load i8*, i8** %v80
  %v89 = call i8* @std__trim(i8* %v88)
  %v87 = bitcast i8* %v89 to i8*
  %v91 = getelementptr [4 x i8], [4 x i8]* @.str90, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v91, i8* %v87)
  %v92 = getelementptr [11 x i8], [11 x i8]* @.str93, i32 0, i32 0
  %v95 = getelementptr [4 x i8], [4 x i8]* @.str94, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v95, i8* %v92)
  %v97 = load i8*, i8** %v18
  %v98 = call i8* @std__to_uppercase(i8* %v97)
  %v96 = bitcast i8* %v98 to i8*
  %v100 = getelementptr [4 x i8], [4 x i8]* @.str99, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v100, i8* %v96)
  %v101 = getelementptr [11 x i8], [11 x i8]* @.str102, i32 0, i32 0
  %v104 = getelementptr [4 x i8], [4 x i8]* @.str103, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v104, i8* %v101)
  %v106 = load i8*, i8** %v18
  %v107 = call i8* @std__to_lowercase(i8* %v106)
  %v105 = bitcast i8* %v107 to i8*
  %v109 = getelementptr [4 x i8], [4 x i8]* @.str108, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v109, i8* %v105)
  %v111 = getelementptr [3 x i8], [3 x i8]* @.str112, i32 0, i32 0
  %v110 = alloca i8*
  store i8* %v111, i8** %v110
  %v113 = getelementptr [11 x i8], [11 x i8]* @.str114, i32 0, i32 0
  %v116 = getelementptr [4 x i8], [4 x i8]* @.str115, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v116, i8* %v113)
  %v117 = load i8*, i8** %v110
  %v118 = call i32 @std__to_int(i8* %v117)
  call void @std__print_int(i32 %v118)
  %v119 = getelementptr [15 x i8], [15 x i8]* @.str120, i32 0, i32 0
  %v122 = getelementptr [4 x i8], [4 x i8]* @.str121, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v122, i8* %v119)
  %v123 = getelementptr [5 x i8], [5 x i8]* @.str124, i32 0, i32 0
  %v125 = call double @std__to_float(i8* %v123)
  call void @std__print_float(double %v125)
  %v126 = getelementptr [8 x i8], [8 x i8]* @.str127, i32 0, i32 0
  %v129 = getelementptr [4 x i8], [4 x i8]* @.str128, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v129, i8* %v126)
  %v130 = call i32 @std__abs(i32 -5)
  call void @std__print_int(i32 %v130)
  %v131 = getelementptr [11 x i8], [11 x i8]* @.str132, i32 0, i32 0
  %v134 = getelementptr [4 x i8], [4 x i8]* @.str133, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v134, i8* %v131)
  %v135 = call double @std__floor(double 3.7)
  call void @std__print_float(double %v135)
  %v136 = getelementptr [10 x i8], [10 x i8]* @.str137, i32 0, i32 0
  %v139 = getelementptr [4 x i8], [4 x i8]* @.str138, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v139, i8* %v136)
  %v140 = call double @std__ceil(double 3.2)
  call void @std__print_float(double %v140)
  %v141 = getelementptr [11 x i8], [11 x i8]* @.str142, i32 0, i32 0
  %v144 = getelementptr [4 x i8], [4 x i8]* @.str143, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v144, i8* %v141)
  %v145 = call double @std__round(double 3.5)
  call void @std__print_float(double %v145)
  %v146 = getelementptr [11 x i8], [11 x i8]* @.str147, i32 0, i32 0
  %v149 = getelementptr [4 x i8], [4 x i8]* @.str148, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v149, i8* %v146)
  %v150 = call i32 @std__min(i32 10, i32 20)
  call void @std__print_int(i32 %v150)
  %v151 = getelementptr [11 x i8], [11 x i8]* @.str152, i32 0, i32 0
  %v154 = getelementptr [4 x i8], [4 x i8]* @.str153, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v154, i8* %v151)
  %v155 = call i32 @std__max(i32 10, i32 20)
  call void @std__print_int(i32 %v155)
  %v156 = getelementptr [10 x i8], [10 x i8]* @.str157, i32 0, i32 0
  %v159 = getelementptr [4 x i8], [4 x i8]* @.str158, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v159, i8* %v156)
  %v160 = call i32 @std__pow(i32 2, i32 10)
  call void @std__print_int(i32 %v160)
  %v163 = call i8* @__rt_to_string_int(i32 123)
  %v162 = bitcast i8* %v163 to i8*
  %v161 = alloca i8*
  store i8* %v162, i8** %v161
  %v164 = getelementptr [15 x i8], [15 x i8]* @.str165, i32 0, i32 0
  %v167 = getelementptr [4 x i8], [4 x i8]* @.str166, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v167, i8* %v164)
  %v168 = load i8*, i8** %v161
  %v170 = getelementptr [4 x i8], [4 x i8]* @.str169, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v170, i8* %v168)
  %v173 = call i8* @__rt_to_string_float(double 4.5600000000000005)
  %v172 = bitcast i8* %v173 to i8*
  %v171 = alloca i8*
  store i8* %v172, i8** %v171
  %v174 = getelementptr [17 x i8], [17 x i8]* @.str175, i32 0, i32 0
  %v177 = getelementptr [4 x i8], [4 x i8]* @.str176, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v177, i8* %v174)
  %v178 = load i8*, i8** %v171
  %v180 = getelementptr [4 x i8], [4 x i8]* @.str179, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v180, i8* %v178)
  %v183 = getelementptr [7 x i8], [7 x i8]* @.str184, i32 0, i32 0
  %v182 = bitcast i8* %v183 to i8*
  %v181 = alloca i8*
  store i8* %v182, i8** %v181
  %v185 = getelementptr [18 x i8], [18 x i8]* @.str186, i32 0, i32 0
  %v188 = getelementptr [4 x i8], [4 x i8]* @.str187, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v188, i8* %v185)
  %v189 = load i8*, i8** %v181
  %v191 = getelementptr [4 x i8], [4 x i8]* @.str190, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v191, i8* %v189)
  %v193 = getelementptr [1 x i8], [1 x i8]* @.str194, i32 0, i32 0
  %v192 = alloca i8*
  store i8* %v193, i8** %v192
  %v195 = getelementptr [16 x i8], [16 x i8]* @.str196, i32 0, i32 0
  %v198 = getelementptr [4 x i8], [4 x i8]* @.str197, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v198, i8* %v195)
  %v199 = load i8*, i8** %v192
  %v200 = call i32 @std__is_empty(i8* %v199)
  call void @std__print_int(i32 %v200)
  %v201 = getelementptr [18 x i8], [18 x i8]* @.str202, i32 0, i32 0
  %v204 = getelementptr [4 x i8], [4 x i8]* @.str203, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v204, i8* %v201)
  %v205 = getelementptr [1 x i8], [1 x i8]* @.str206, i32 0, i32 0
  %v207 = call i32 @std__is_empty(i8* %v205)
  call void @std__print_int(i32 %v207)
  %v208 = getelementptr [16 x i8], [16 x i8]* @.str209, i32 0, i32 0
  %v211 = getelementptr [4 x i8], [4 x i8]* @.str210, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v211, i8* %v208)
  %v212 = getelementptr [6 x i8], [6 x i8]* @.str213, i32 0, i32 0
  %v214 = call i32 @std__is_empty(i8* %v212)
  call void @std__print_int(i32 %v214)
  %v215 = getelementptr [14 x i8], [14 x i8]* @.str216, i32 0, i32 0
  %v218 = getelementptr [4 x i8], [4 x i8]* @.str217, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v218, i8* %v215)
  %v219 = load i8*, i8** %v18
  %v220 = call i64 @__rt_strlen(i8* %v219)
  %v221 = trunc i64 %v220 to i32
  call void @std__print_int(i32 %v221)
  %v222 = getelementptr [9 x i8], [9 x i8]* @.str223, i32 0, i32 0
  %v225 = getelementptr [4 x i8], [4 x i8]* @.str224, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v225, i8* %v222)
  %v226 = getelementptr [12 x i8], [12 x i8]* @.str227, i32 0, i32 0
  %v229 = getelementptr [4 x i8], [4 x i8]* @.str228, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v229, i8* %v226)
  %v230 = getelementptr [5 x i8], [5 x i8]* @.str231, i32 0, i32 0
  %v233 = getelementptr [4 x i8], [4 x i8]* @.str232, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v233, i8* %v230)
  %v234 = getelementptr [10 x i8], [10 x i8]* @.str235, i32 0, i32 0
  %v237 = getelementptr [4 x i8], [4 x i8]* @.str236, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v237, i8* %v234)
  %v238 = getelementptr [11 x i8], [11 x i8]* @.str239, i32 0, i32 0
  %v241 = getelementptr [4 x i8], [4 x i8]* @.str240, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v241, i8* %v238)
  %v242 = getelementptr [13 x i8], [13 x i8]* @.str243, i32 0, i32 0
  %v245 = getelementptr [4 x i8], [4 x i8]* @.str244, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v245, i8* %v242)
  %v246 = getelementptr [7 x i8], [7 x i8]* @.str247, i32 0, i32 0
  %v249 = getelementptr [4 x i8], [4 x i8]* @.str248, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v249, i8* %v246)
  %v250 = getelementptr [17 x i8], [17 x i8]* @.str251, i32 0, i32 0
  %v253 = getelementptr [4 x i8], [4 x i8]* @.str252, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v253, i8* %v250)
  %v254 = getelementptr [18 x i8], [18 x i8]* @.str255, i32 0, i32 0
  %v257 = getelementptr [4 x i8], [4 x i8]* @.str256, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %v257, i8* %v254)
  ret i32 0
}

define i32 @std__ends_with(i8* %arg0, i8* %arg1) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %arg1_alloca = alloca i8*
  store i8* %arg1, i8** %arg1_alloca
  %v258 = load i8*, i8** %arg0_alloca
  %v259 = load i8*, i8** %arg1_alloca
  %v260 = call i32 @__rt_ends_with(i8* %v258, i8* %v259)
  ret i32 %v260
}

define i8* @std__to_lowercase(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v262 = load i8*, i8** %arg0_alloca
  %v263 = call i8* @__rt_to_lowercase(i8* %v262)
  %v261 = bitcast i8* %v263 to i8*
  ret i8* %v261
}

define i32 @std__abs(i32 %arg0) {
entry:
  %arg0_alloca = alloca i32
  store i32 %arg0, i32* %arg0_alloca
  %v264 = load i32, i32* %arg0_alloca
  %v266 = icmp slt i32 %v264, 0
  %v267 = zext i1 %v266 to i32
  %v268 = icmp ne i32 %v267, 0
  br i1 %v268, label %then_0, label %merge_1
then_0:
  %v269 = load i32, i32* %arg0_alloca
  %v270 = sub i32 0, %v269
  ret i32 %v270
merge_1:
  %v271 = load i32, i32* %arg0_alloca
  ret i32 %v271
}

define i32 @std__to_int(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v272 = load i8*, i8** %arg0_alloca
  %v273 = call i32 @__rt_to_int(i8* %v272)
  ret i32 %v273
}

define i32 @std__len(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v274 = load i8*, i8** %arg0_alloca
  %v275 = call i32 @__rt_strlen(i8* %v274)
  ret i32 %v275
}

define i32 @std__min(i32 %arg0, i32 %arg1) {
entry:
  %arg0_alloca = alloca i32
  store i32 %arg0, i32* %arg0_alloca
  %arg1_alloca = alloca i32
  store i32 %arg1, i32* %arg1_alloca
  %v276 = load i32, i32* %arg0_alloca
  %v277 = load i32, i32* %arg1_alloca
  %v279 = icmp slt i32 %v276, %v277
  %v280 = zext i1 %v279 to i32
  %v281 = icmp ne i32 %v280, 0
  br i1 %v281, label %then_0, label %merge_1
then_0:
  %v282 = load i32, i32* %arg0_alloca
  ret i32 %v282
merge_1:
  %v283 = load i32, i32* %arg1_alloca
  ret i32 %v283
}

define double @std__to_float(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v284 = load i8*, i8** %arg0_alloca
  %v285 = call double @__rt_to_float(i8* %v284)
  ret double %v285
}

define i32 @std__pow(i32 %arg0, i32 %arg1) {
entry:
  %arg0_alloca = alloca i32
  store i32 %arg0, i32* %arg0_alloca
  %arg1_alloca = alloca i32
  store i32 %arg1, i32* %arg1_alloca
  %v286 = load i32, i32* %arg0_alloca
  %v287 = load i32, i32* %arg1_alloca
  %v289 = sitofp i32 %v286 to double
  %v290 = sitofp i32 %v287 to double
  %v291 = call double @llvm.pow.f64(double %v289, double %v290)
  %v292 = fptosi double %v291 to i32
  ret i32 %v292
}

define i32 @std__is_empty(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v293 = load i8*, i8** %arg0_alloca
  %v294 = call i32 @__rt_strlen(i8* %v293)
  %v296 = icmp eq i32 %v294, 0
  %v297 = zext i1 %v296 to i32
  ret i32 %v297
}

define i32 @std__max(i32 %arg0, i32 %arg1) {
entry:
  %arg0_alloca = alloca i32
  store i32 %arg0, i32* %arg0_alloca
  %arg1_alloca = alloca i32
  store i32 %arg1, i32* %arg1_alloca
  %v298 = load i32, i32* %arg0_alloca
  %v299 = load i32, i32* %arg1_alloca
  %v301 = icmp sgt i32 %v298, %v299
  %v302 = zext i1 %v301 to i32
  %v303 = icmp ne i32 %v302, 0
  br i1 %v303, label %then_0, label %merge_1
then_0:
  %v304 = load i32, i32* %arg0_alloca
  ret i32 %v304
merge_1:
  %v305 = load i32, i32* %arg1_alloca
  ret i32 %v305
}

define double @std__floor(double %arg0) {
entry:
  %arg0_alloca = alloca double
  store double %arg0, double* %arg0_alloca
  %v306 = load double, double* %arg0_alloca
  %v307 = call double @__rt_floor(double %v306)
  ret double %v307
}

define void @std__print_int(i32 %arg0) {
entry:
  %arg0_alloca = alloca i32
  store i32 %arg0, i32* %arg0_alloca
  %v308 = load i32, i32* %arg0_alloca
  call void @__rt_print_int(i32 %v308)
  ret void
}

define i8* @std__substr(i8* %arg0, i32 %arg1, i32 %arg2) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %arg1_alloca = alloca i32
  store i32 %arg1, i32* %arg1_alloca
  %arg2_alloca = alloca i32
  store i32 %arg2, i32* %arg2_alloca
  %v310 = load i8*, i8** %arg0_alloca
  %v311 = load i32, i32* %arg1_alloca
  %v312 = load i32, i32* %arg2_alloca
  %v313 = call i8* @__rt_substr(i8* %v310, i32 %v311, i32 %v312)
  %v309 = bitcast i8* %v313 to i8*
  ret i8* %v309
}

define i32 @std__contains(i8* %arg0, i8* %arg1) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %arg1_alloca = alloca i8*
  store i8* %arg1, i8** %arg1_alloca
  %v314 = load i8*, i8** %arg0_alloca
  %v315 = load i8*, i8** %arg1_alloca
  %v316 = call i32 @__rt_contains(i8* %v314, i8* %v315)
  ret i32 %v316
}

define i32 @std__starts_with(i8* %arg0, i8* %arg1) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %arg1_alloca = alloca i8*
  store i8* %arg1, i8** %arg1_alloca
  %v317 = load i8*, i8** %arg0_alloca
  %v318 = load i8*, i8** %arg1_alloca
  %v319 = call i32 @__rt_starts_with(i8* %v317, i8* %v318)
  ret i32 %v319
}

define i8* @std__to_uppercase(i8* %arg0) {
entry:
  %arg0_alloca = alloca i8*
  store i8* %arg0, i8** %arg0_alloca
  %v321 = load i8*, i8** %arg0_alloca
  %v322 = call i8* @__rt_to_uppercase(i8* %v321)
  %v320 = bitcast i8* %v322 to i8*
  ret i8* %v320
}

define void @std__print_float(double %arg0) {
entry:
  %arg0_alloca = alloca double
  store double %arg0, double* %arg0_alloca
  %v323 = load double, double* %arg0_alloca
  call void @__rt_print_float(double %v323)
  ret void
}

