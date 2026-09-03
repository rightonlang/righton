#ifndef LIBRO_H
#define LIBRO_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Righton String representation: must match LLVM IR %String = { i8*, i32, i32 }
// Layout: ptr (8 bytes on 64-bit), len (4), cap (4), total 16 with padding.
// The struct is intentionally NOT packed to match LLVM's aligned layout:
//   offset(ptr) = 0, offset(len) = 8, offset(cap) = 12 on 64-bit.
// For 32-bit, offsets are 0,4,8. Using explicit struct lets C compiler
// handle alignment; IR uses getelementptr with struct indexing, which
// respects target alignment. C helpers use field access, so layouts match.
//
// For raw byte-buffer helpers (lists, raw C strings), we use void*/char*.
typedef struct {
    char *ptr;
    int32_t len;
    int32_t cap;
} RoString;

// ---- core runtime ----
int32_t __rt_strlen(RoString *s);
void __rt_print_str(RoString *s);
void __rt_print_int(int32_t n);
void __rt_print_float(double n);
void __rt_exit(int32_t code);
char*   __rt_read_file(const char *path);
int32_t __rt_write_file(const char *path, RoString *contents);
int32_t __rt_contains(RoString *s, RoString *sub);
int32_t __rt_starts_with(RoString *s, RoString *prefix);
int32_t __rt_ends_with(RoString *s, RoString *suffix);
char*   __rt_substr(RoString *s, int32_t start, int32_t length);
char*   __rt_trim(RoString *s);
char*   __rt_to_uppercase(RoString *s);
char*   __rt_to_lowercase(RoString *s);
int32_t __rt_to_int(RoString *s);
double  __rt_to_float(RoString *s);
double  __rt_floor(double n);
double  __rt_ceil(double n);
double  __rt_round(double n);
char*   __rt_read_line(void);
char*   __rt_to_string_int(int32_t n);
char*   __rt_to_string_float(double n);
void    __rt_free(void *ptr);
RoString* __rt_wrap_string(char *s);
void    __rt_panic_bounds(char *msg);

// ---- collection runtime ----
int32_t __rt_list_len(void *list);
void*   __rt_list_push(void *list, int32_t val);
int32_t __rt_list_pop(void *list);

// ---- formatting / math ----
char*   __rt_to_hex(int32_t n);
char*   __rt_str_repeat(RoString *s, int32_t count);
double  __rt_sqrt(double n);
double  __rt_sin(double n);
double  __rt_cos(double n);
double  __rt_tan(double n);
double  __rt_abs(double n);
double  __rt_exp(double n);
double  __rt_log(double n);
double  __rt_tanh(double n);
int32_t __rt_rand(void);
double  __rt_rand_float(void);
void    __rt_srand(int32_t seed);

// ---- float list helpers ----
void*   __rt_list_push_f64(void *list, double val);
double  __rt_list_get_f64(void *list, int32_t index);
void    __rt_list_set_f64(void *list, int32_t index, double val);
int32_t __rt_list_pop_f64(void *list, double *out);

// For internal IR helpers that historically took raw i8* (C string):
// Provide raw variant so old IR `call @__rt_strlen(i8*)` still links
// if we choose to keep it. The preferred API is RoString*.
int32_t __rt_strlen_cstr(const char *s);

#ifdef __cplusplus
}
#endif

#endif // LIBRO_H
