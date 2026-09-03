#include "../include/libro.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <stdint.h>

// ---------------------------------------------------------------------------
// core helpers
// ---------------------------------------------------------------------------

int32_t __rt_strlen(RoString *s) {
    if (!s || !s->ptr) return 0;
    // Use stored length if valid, otherwise compute via strlen
    // Stored len is authoritative for Righton strings (may contain nulls?).
    // Fall back to strlen if len is 0 but ptr non-empty to handle C strings
    // wrapped without len. For now return s->len if set, else strlen.
    if (s->len >= 0) {
        // If s->len was set by __rt_wrap_string, it equals strlen.
        // Use it directly for speed and to support embedded nulls in future.
        // However for safety we could recompute: strlen(s->ptr)
        // We'll prefer stored len when cap is also set (meaning wrapped).
        if (s->cap != 0) return s->len;
    }
    return (int32_t)strlen(s->ptr);
}

int32_t __rt_strlen_cstr(const char *s) {
    if (!s) return 0;
    return (int32_t)strlen(s);
}

void __rt_print_str(RoString *s) {
    if (!s || !s->ptr) {
        printf("\n");
        return;
    }
    printf("%s\n", s->ptr);
}

void __rt_print_int(int32_t n) {
    printf("%d\n", n);
}

void __rt_print_float(double n) {
    printf("%f\n", n);
}

void __rt_exit(int32_t code) {
    exit(code);
}

char* __rt_read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        char *buf = (char*)malloc(1);
        if (buf) buf[0] = '\0';
        return buf;
    }
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    if (size < 0) size = 0;
    rewind(f);
    char *buf = (char*)malloc((size_t)size + 1);
    if (!buf) {
        fclose(f);
        char *empty = (char*)malloc(1);
        if (empty) empty[0] = '\0';
        return empty;
    }
    size_t readn = fread(buf, 1, (size_t)size, f);
    buf[readn] = '\0';
    fclose(f);
    return buf;
}

int32_t __rt_write_file(const char *path, RoString *contents) {
    FILE *f = fopen(path, "wb");
    if (!f) return -1;
    const char *data = (contents && contents->ptr) ? contents->ptr : "";
    size_t len = contents ? (size_t)contents->len : strlen(data);
    // If len ==0 but data non-empty, fallback to strlen
    if (len == 0 && data[0] != '\0') len = strlen(data);
    size_t written = fwrite(data, 1, len, f);
    fclose(f);
    return (written == len) ? 0 : -1;
}

int32_t __rt_contains(RoString *s, RoString *sub) {
    if (!s || !s->ptr || !sub || !sub->ptr) return 0;
    return strstr(s->ptr, sub->ptr) != NULL ? 1 : 0;
}

int32_t __rt_starts_with(RoString *s, RoString *prefix) {
    if (!s || !s->ptr || !prefix || !prefix->ptr) return 0;
    size_t plen = (size_t)(prefix->len >= 0 ? prefix->len : (int32_t)strlen(prefix->ptr));
    if (plen == 0) return 1;
    return strncmp(s->ptr, prefix->ptr, plen) == 0 ? 1 : 0;
}

int32_t __rt_ends_with(RoString *s, RoString *suffix) {
    if (!s || !s->ptr || !suffix || !suffix->ptr) return 0;
    size_t slen = s->len >= 0 ? (size_t)s->len : strlen(s->ptr);
    size_t sublen = suffix->len >= 0 ? (size_t)suffix->len : strlen(suffix->ptr);
    if (sublen > slen) return 0;
    if (sublen == 0) return 1;
    return strncmp(s->ptr + slen - sublen, suffix->ptr, sublen) == 0 ? 1 : 0;
}

char* __rt_substr(RoString *s, int32_t start, int32_t length) {
    if (!s || !s->ptr || length <= 0) {
        char *buf = (char*)malloc(1);
        if (buf) buf[0] = '\0';
        return buf;
    }
    size_t slen = s->len >= 0 ? (size_t)s->len : strlen(s->ptr);
    if (start < 0) start = 0;
    if ((size_t)start > slen) start = (int32_t)slen;
    if ((size_t)length > slen - (size_t)start) length = (int32_t)(slen - (size_t)start);
    char *buf = (char*)malloc((size_t)length + 1);
    if (!buf) return NULL;
    memcpy(buf, s->ptr + start, (size_t)length);
    buf[length] = '\0';
    return buf;
}

char* __rt_trim(RoString *s) {
    if (!s || !s->ptr) {
        char *buf = (char*)malloc(1);
        if (buf) buf[0] = '\0';
        return buf;
    }
    const char *ws = " \t\n\r\f\v";
    size_t len = strlen(s->ptr);
    size_t start = strspn(s->ptr, ws);
    if (start >= len) {
        char *buf = (char*)malloc(1);
        if (buf) buf[0] = '\0';
        return buf;
    }
    const char *start_ptr = s->ptr + start;
    size_t remaining = len - start;
    size_t actual = remaining;
    while (actual > 0 && strchr(ws, start_ptr[actual - 1])) actual--;
    char *buf = (char*)malloc(actual + 1);
    if (!buf) return NULL;
    memcpy(buf, start_ptr, actual);
    buf[actual] = '\0';
    return buf;
}

char* __rt_to_uppercase(RoString *s) {
    if (!s || !s->ptr) {
        char *buf = (char*)malloc(1);
        if (buf) buf[0] = '\0';
        return buf;
    }
    size_t len = strlen(s->ptr);
    char *buf = (char*)malloc(len + 1);
    if (!buf) return NULL;
    memcpy(buf, s->ptr, len + 1);
    for (size_t i = 0; i < len; i++) buf[i] = (char)toupper((unsigned char)buf[i]);
    return buf;
}

char* __rt_to_lowercase(RoString *s) {
    if (!s || !s->ptr) {
        char *buf = (char*)malloc(1);
        if (buf) buf[0] = '\0';
        return buf;
    }
    size_t len = strlen(s->ptr);
    char *buf = (char*)malloc(len + 1);
    if (!buf) return NULL;
    memcpy(buf, s->ptr, len + 1);
    for (size_t i = 0; i < len; i++) buf[i] = (char)tolower((unsigned char)buf[i]);
    return buf;
}

int32_t __rt_to_int(RoString *s) {
    if (!s || !s->ptr) return 0;
    return (int32_t)atoi(s->ptr);
}

double __rt_to_float(RoString *s) {
    if (!s || !s->ptr) return 0.0;
    return atof(s->ptr);
}

double __rt_floor(double n) { return floor(n); }
double __rt_ceil(double n)  { return ceil(n); }
double __rt_round(double n) { return round(n); }

char* __rt_read_line(void) {
    char *buf = (char*)malloc(1024);
    if (!buf) return NULL;
    char *stdin_ptr = stdin ? NULL : NULL;
    (void)stdin_ptr;
    char *res = fgets(buf, 1024, stdin);
    if (!res) {
        buf[0] = '\0';
        return buf;
    }
    size_t len = strlen(buf);
    if (len > 0 && buf[len - 1] == '\n') buf[len - 1] = '\0';
    return buf;
}

char* __rt_to_string_int(int32_t n) {
    char *buf = (char*)malloc(64);
    if (!buf) return NULL;
    snprintf(buf, 64, "%d", n);
    return buf;
}

char* __rt_to_string_float(double n) {
    char *buf = (char*)malloc(64);
    if (!buf) return NULL;
    snprintf(buf, 64, "%f", n);
    return buf;
}

void __rt_free(void *ptr) {
    if (ptr) free(ptr);
}

RoString* __rt_wrap_string(char *s) {
    if (!s) {
        s = (char*)malloc(1);
        if (s) s[0] = '\0';
    }
    RoString *str = (RoString*)malloc(sizeof(RoString));
    if (!str) return NULL;
    str->ptr = s;
    str->len = (int32_t)strlen(s);
    str->cap = str->len;
    return str;
}

void __rt_panic_bounds(char *msg) {
    if (msg) __rt_print_str((RoString*)&(RoString){msg, (int32_t)strlen(msg), (int32_t)strlen(msg)});
    // Fallback direct print
    if (msg) fprintf(stderr, "%s\n", msg);
    __rt_exit(1);
    // unreachable
    exit(1);
}

// ---------------------------------------------------------------------------
// list runtime: layout { int32 cap; int32 len; char data[] } with element size 4
// ---------------------------------------------------------------------------

int32_t __rt_list_len(void *list) {
    if (!list) return 0;
    int32_t *header = (int32_t*)list;
    return header[1]; // len at offset 4
}

void* __rt_list_push(void *list, int32_t val) {
    if (!list) {
        // allocate new list with cap 4
        int32_t cap = 4;
        size_t total = 8 + (size_t)cap * 4;
        void *newlist = malloc(total);
        if (!newlist) return NULL;
        ((int32_t*)newlist)[0] = cap;
        ((int32_t*)newlist)[1] = 0;
        list = newlist;
    }
    int32_t *header = (int32_t*)list;
    int32_t cap = header[0];
    int32_t len = header[1];
    if (len >= cap) {
        int32_t newcap = cap * 2;
        if (newcap == 0) newcap = 4;
        size_t old_total = 8 + (size_t)cap * 4;
        size_t new_total = 8 + (size_t)newcap * 4;
        void *newlist = malloc(new_total);
        if (!newlist) return list;
        memcpy(newlist, list, old_total);
        free(list);
        list = newlist;
        header = (int32_t*)list;
        header[0] = newcap;
    }
    // store element at offset 8 + len*4
    char *base = (char*)list;
    int32_t *elem = (int32_t*)(base + 8 + (size_t)len * 4);
    *elem = val;
    header[1] = len + 1;
    return list;
}

int32_t __rt_list_pop(void *list) {
    if (!list) return 0;
    int32_t *header = (int32_t*)list;
    int32_t len = header[1];
    if (len <= 0) return 0;
    char *base = (char*)list;
    int32_t *elem = (int32_t*)(base + 8 + (size_t)(len - 1) * 4);
    int32_t val = *elem;
    header[1] = len - 1;
    return val;
}

// ---------------------------------------------------------------------------
// formatting / math
// ---------------------------------------------------------------------------

char* __rt_to_hex(int32_t n) {
    char *buf = (char*)malloc(11);
    if (!buf) return NULL;
    snprintf(buf, 11, "%08x", n);
    return buf;
}

char* __rt_str_repeat(RoString *s, int32_t count) {
    if (!s || !s->ptr || count <= 0) {
        char *buf = (char*)malloc(1);
        if (buf) buf[0] = '\0';
        return buf;
    }
    size_t len = strlen(s->ptr);
    if (len == 0) {
        char *buf = (char*)malloc(1);
        if (buf) buf[0] = '\0';
        return buf;
    }
    size_t total = len * (size_t)count;
    char *buf = (char*)malloc(total + 1);
    if (!buf) return NULL;
    for (int32_t i = 0; i < count; i++) {
        memcpy(buf + (size_t)i * len, s->ptr, len);
    }
    buf[total] = '\0';
    return buf;
}

double __rt_sqrt(double n) { return sqrt(n); }
double __rt_sin(double n)  { return sin(n); }
double __rt_cos(double n)  { return cos(n); }
double __rt_tan(double n)  { return tan(n); }

double __rt_abs(double n) {
    return n < 0 ? -n : n;
}

// Legacy raw C-string helpers kept for IR that still calls with i8*.
// They simply forward to the typed versions by wrapping.
int32_t __rt_strlen_raw_compat(RoString *s) { return __rt_strlen(s); }
