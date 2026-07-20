// Tiny fixture shared library for FFI integration tests.
// Build: cc -shared -o libfixture.dylib fixture.c  (macOS)
//        cc -shared -fPIC -o libfixture.so fixture.c  (Linux)
//        cl /LD fixture.c /Fe:fixture.dll  (Windows)

#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>

#if defined(_WIN32)
#include <windows.h>
#else
#include <pthread.h>
#endif

// -- Integer arithmetic -----------------------------------------------------

int add_i32(int a, int b) {
  return a + b;
}

int mul_i32(int a, int b) {
  return a * b;
}

int negate_i32(int a) {
  return -a;
}

int identity_i32(int a) {
  return a;
}

int32_t ffi_v2_signed_i8_to_i32(int8_t value) {
  return value;
}

int32_t ffi_v2_signed_i16_to_i32(int16_t value) {
  return value;
}

// -- Void / no-arg ----------------------------------------------------------

static int call_count = 0;

void increment_counter(void) {
  call_count++;
}

int get_counter(void) {
  return call_count;
}

int get_answer(void) {
  return 42;
}

// -- Floating point ---------------------------------------------------------

double add_f64(double a, double b) {
  return a + b;
}

double mul_f64(double a, double b) {
  return a * b;
}

double pi(void) {
  return 3.14159265358979323846;
}

float add_f32(float a, float b) {
  return a + b;
}

// -- Strings ----------------------------------------------------------------

int string_length(const char* s) {
  if (!s) return -1;
  return (int)strlen(s);
}

// Returns a static string (caller must NOT free)
const char* greeting(void) {
  return "hello from C";
}

// UTF-8 bytes for "h\u00e9llo \U0001f600". Hex escapes avoid depending on the
// fixture source file's execution character set.
const char* greeting_unicode(void) {
  return "h\xC3\xA9llo \xF0\x9F\x98\x80";
}

// A UTF-8 encoding of a lone UTF-16 high surrogate, which is never valid
// Unicode scalar value data.
const char* invalid_utf8(void) {
  static const char value[] = { (char)0xED, (char)0xA0, (char)0x80, '\0' };
  return value;
}

// -- Pointers ---------------------------------------------------------------

int is_null(void* p) {
  return p == NULL ? 1 : 0;
}

void write_i32(int* dst, int value) {
  if (dst) *dst = value;
}

int read_i32(const int* src) {
  if (!src) return 0;
  return *src;
}

// -- Multi-arg (up to 6) ---------------------------------------------------

int sum6(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}

double sum4_f64(double a, double b, double c, double d) {
  return a + b + c + d;
}

// -- Mixed integer/float args -----------------------------------------------

double scale_f64(int factor, double value) {
  return factor * value;
}

double weighted_sum(double a, int weight_a, double b, int weight_b) {
  return a * weight_a + b * weight_b;
}

int round_f64(double value) {
  return (int)(value + 0.5);
}

double index_scale(int index, double scale, int offset) {
  return (index + offset) * scale;
}

double mixed5(int a, double b, int c, double d, int e) {
  return a + b + c + d + e;
}

double mixed6(int a, double b, int c, double d, int e, double f) {
  return a + b + c + d + e + f;
}

double mixed8(int a, double b, int c, double d, int e, double f, int g, double h) {
  return a + b + c + d + e + f + g + h;
}

// -- FFI v2 callbacks ------------------------------------------------------

typedef int32_t (*ffi_v2_i32_callback)(int32_t value);
typedef int8_t (*ffi_v2_i8_callback)(void);
typedef int16_t (*ffi_v2_i16_callback)(void);
typedef void (*ffi_v2_void_callback)(void);
typedef int (*ffi_v2_compare_i32_callback)(const void* left, const void* right);

static ffi_v2_i32_callback ffi_v2_stored_i32_callback = NULL;
static int32_t ffi_v2_callback_progress = 0;

int32_t ffi_v2_call_i32_callback(ffi_v2_i32_callback callback, int32_t value) {
  return callback(value);
}

int32_t ffi_v2_call_i8_callback(ffi_v2_i8_callback callback) {
  return callback();
}

int32_t ffi_v2_call_i16_callback(ffi_v2_i16_callback callback) {
  return callback();
}

void* ffi_v2_callback_then_pointer(ffi_v2_void_callback callback) {
  callback();
  return (void*)get_answer;
}

void ffi_v2_sort_i32(
  int32_t* values,
  int32_t length,
  ffi_v2_compare_i32_callback compare
) {
  qsort(values, (size_t)length, sizeof(int32_t), compare);
}

void ffi_v2_store_i32_callback(ffi_v2_i32_callback callback) {
  ffi_v2_stored_i32_callback = callback;
}

int32_t ffi_v2_invoke_stored_i32_callback(int32_t value) {
  if (!ffi_v2_stored_i32_callback) return -1;
  return ffi_v2_stored_i32_callback(value);
}

void ffi_v2_clear_stored_i32_callback(void) {
  ffi_v2_stored_i32_callback = NULL;
}

int32_t ffi_v2_call_i32_callback_twice(
  ffi_v2_i32_callback callback,
  int32_t value
) {
  int32_t first;
  int32_t second;

  ffi_v2_callback_progress = 0;
  first = callback(value);
  ffi_v2_callback_progress = 1;
  second = callback(value + 1);
  ffi_v2_callback_progress = 2;
  return first + second;
}

int32_t ffi_v2_get_callback_progress(void) {
  return ffi_v2_callback_progress;
}

// -- FFI v2 aggregate types ------------------------------------------------

typedef struct {
  double x;
  double y;
} ffi_v2_point;

typedef struct {
  double first;
  double second;
  double third;
} ffi_v2_large_vector;

typedef struct {
  uint8_t tag;
  double value;
  uint16_t code;
} ffi_v2_aligned_record;

typedef struct {
  double value;
  int32_t tag;
} ffi_v2_mixed_record;

typedef struct {
  uint8_t values[4];
} ffi_v2_bytes4;

typedef union {
  uint32_t as_u32;
  uint8_t as_bytes[4];
} ffi_v2_word;

typedef union {
  double as_double;
  double alternate;
} ffi_v2_double_union;

typedef struct {
  float first;
  float second;
  float third;
} ffi_v2_float3;

typedef struct {
  float value;
} ffi_v2_float1;

typedef struct {
  ffi_v2_bytes4 header;
  ffi_v2_word payload;
  ffi_v2_point point;
} ffi_v2_composite;

typedef struct {
  void* pointer;
} ffi_v2_pointer_holder;

typedef struct {
  uint8_t bytes[8192];
} ffi_v2_large_byte_array;

ffi_v2_point ffi_v2_add_points(ffi_v2_point left, ffi_v2_point right) {
  ffi_v2_point result;
  result.x = left.x + right.x;
  result.y = left.y + right.y;
  return result;
}

double ffi_v2_point_distance_squared(ffi_v2_point left, ffi_v2_point right) {
  double dx = right.x - left.x;
  double dy = right.y - left.y;
  return dx * dx + dy * dy;
}

void ffi_v2_translate_point(
  ffi_v2_point* point,
  double delta_x,
  double delta_y
) {
  point->x += delta_x;
  point->y += delta_y;
}

ffi_v2_large_vector ffi_v2_make_large_vector(double seed) {
  ffi_v2_large_vector result;
  result.first = seed;
  result.second = seed + 1.0;
  result.third = seed + 2.0;
  return result;
}

double ffi_v2_sum_large_vector(ffi_v2_large_vector value) {
  return value.first + value.second + value.third;
}

double ffi_v2_aligned_record_checksum(ffi_v2_aligned_record value) {
  return (double)value.tag + value.value + (double)value.code;
}

ffi_v2_mixed_record ffi_v2_make_mixed_record(double value, int32_t tag) {
  ffi_v2_mixed_record result;
  result.value = value;
  result.tag = tag;
  return result;
}

double ffi_v2_mixed_record_under_register_pressure(
  int32_t first,
  int32_t second,
  int32_t third,
  int32_t fourth,
  int32_t fifth,
  int32_t sixth,
  int32_t seventh,
  ffi_v2_mixed_record value
) {
  return first + second + third + fourth + fifth + sixth + seventh +
    value.value + value.tag;
}

int32_t ffi_v2_aligned_record_size(void) {
  return (int32_t)sizeof(ffi_v2_aligned_record);
}

int32_t ffi_v2_aligned_record_alignment(void) {
  struct ffi_v2_alignment_probe {
    char prefix;
    ffi_v2_aligned_record value;
  };
  return (int32_t)offsetof(struct ffi_v2_alignment_probe, value);
}

int32_t ffi_v2_aligned_record_value_offset(void) {
  return (int32_t)offsetof(ffi_v2_aligned_record, value);
}

int32_t ffi_v2_aligned_record_code_offset(void) {
  return (int32_t)offsetof(ffi_v2_aligned_record, code);
}

ffi_v2_bytes4 ffi_v2_reverse_bytes4(ffi_v2_bytes4 value) {
  ffi_v2_bytes4 result;
  result.values[0] = value.values[3];
  result.values[1] = value.values[2];
  result.values[2] = value.values[1];
  result.values[3] = value.values[0];
  return result;
}

ffi_v2_word ffi_v2_add_to_word(ffi_v2_word value, uint32_t amount) {
  value.as_u32 += amount;
  return value;
}

uint8_t ffi_v2_word_first_byte(ffi_v2_word value) {
  return value.as_bytes[0];
}

ffi_v2_double_union ffi_v2_add_to_double_union(
  ffi_v2_double_union value,
  double amount
) {
  value.as_double += amount;
  return value;
}

double ffi_v2_point_after_seven_doubles(
  double first,
  double second,
  double third,
  double fourth,
  double fifth,
  double sixth,
  double seventh,
  ffi_v2_point value
) {
  return first + second + third + fourth + fifth + sixth + seventh +
    value.x + value.y;
}

double ffi_v2_compact_spilled_hfas(
  ffi_v2_double_union first,
  ffi_v2_double_union second,
  ffi_v2_double_union third,
  ffi_v2_double_union fourth,
  ffi_v2_double_union fifth,
  ffi_v2_double_union sixth,
  ffi_v2_float3 value,
  ffi_v2_float1 tail
) {
  return first.as_double + second.as_double + third.as_double +
    fourth.as_double + fifth.as_double + sixth.as_double + value.first +
    value.second + value.third + tail.value;
}

ffi_v2_composite ffi_v2_transform_composite(ffi_v2_composite value) {
  value.header.values[0] += 10;
  value.payload.as_u32 += 20;
  value.point.x += 30.0;
  value.point.y += 40.0;
  return value;
}

ffi_v2_pointer_holder ffi_v2_get_answer_pointer_holder(void) {
  ffi_v2_pointer_holder result;
  result.pointer = (void*)get_answer;
  return result;
}

int32_t ffi_v2_sum_large_byte_array(ffi_v2_large_byte_array value) {
  return value.bytes[0] + value.bytes[4096] + value.bytes[8191];
}

int32_t ffi_v2_composite_header_offset(void) {
  return (int32_t)offsetof(ffi_v2_composite, header);
}

int32_t ffi_v2_composite_payload_offset(void) {
  return (int32_t)offsetof(ffi_v2_composite, payload);
}

int32_t ffi_v2_composite_point_offset(void) {
  return (int32_t)offsetof(ffi_v2_composite, point);
}

// -- FFI v2 callback ABI coverage -----------------------------------------

typedef ffi_v2_point (*ffi_v2_point_callback)(ffi_v2_point value);
typedef ffi_v2_mixed_record (*ffi_v2_mixed_record_callback)(
  ffi_v2_mixed_record value
);
typedef ffi_v2_large_vector (*ffi_v2_large_vector_callback)(
  ffi_v2_large_vector value
);
typedef ffi_v2_double_union (*ffi_v2_double_union_callback)(
  ffi_v2_double_union value
);
typedef double (*ffi_v2_exhausted_hfa_callback)(
  double first,
  double second,
  double third,
  double fourth,
  double fifth,
  double sixth,
  double seventh,
  ffi_v2_point value
);
typedef double (*ffi_v2_compact_spilled_hfa_callback)(
  ffi_v2_double_union first,
  ffi_v2_double_union second,
  ffi_v2_double_union third,
  ffi_v2_double_union fourth,
  ffi_v2_double_union fifth,
  ffi_v2_double_union sixth,
  ffi_v2_float3 value,
  ffi_v2_float1 tail
);

static double ffi_v2_last_large_callback_sum = 0.0;
static double ffi_v2_last_large_callback_input_sum = 0.0;

ffi_v2_point ffi_v2_call_point_callback(
  ffi_v2_point_callback callback,
  ffi_v2_point value
) {
  return callback(value);
}

ffi_v2_mixed_record ffi_v2_call_mixed_record_callback(
  ffi_v2_mixed_record_callback callback,
  ffi_v2_mixed_record value
) {
  return callback(value);
}

ffi_v2_double_union ffi_v2_call_double_union_callback(
  ffi_v2_double_union_callback callback,
  ffi_v2_double_union value
) {
  return callback(value);
}

double ffi_v2_call_exhausted_hfa_callback(
  ffi_v2_exhausted_hfa_callback callback
) {
  ffi_v2_point value = { 8.0, 9.0 };
  return callback(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, value);
}

double ffi_v2_call_compact_spilled_hfa_callback(
  ffi_v2_compact_spilled_hfa_callback callback
) {
  ffi_v2_double_union first;
  ffi_v2_double_union second;
  ffi_v2_double_union third;
  ffi_v2_double_union fourth;
  ffi_v2_double_union fifth;
  ffi_v2_double_union sixth;
  ffi_v2_float3 value = { 7.0f, 8.0f, 9.0f };
  ffi_v2_float1 tail = { 10.0f };
  first.as_double = 1.0;
  second.as_double = 2.0;
  third.as_double = 3.0;
  fourth.as_double = 4.0;
  fifth.as_double = 5.0;
  sixth.as_double = 6.0;
  return callback(first, second, third, fourth, fifth, sixth, value, tail);
}

ffi_v2_large_vector ffi_v2_call_large_vector_callback(
  ffi_v2_large_vector_callback callback,
  ffi_v2_large_vector value
) {
  return callback(value);
}

void ffi_v2_capture_large_callback_result(
  ffi_v2_large_vector_callback callback,
  ffi_v2_large_vector value
) {
  ffi_v2_large_vector result = callback(value);
  ffi_v2_last_large_callback_input_sum =
    value.first + value.second + value.third;
  ffi_v2_last_large_callback_sum =
    result.first + result.second + result.third;
}

double ffi_v2_get_last_large_callback_sum(void) {
  return ffi_v2_last_large_callback_sum;
}

double ffi_v2_get_last_large_callback_input_sum(void) {
  return ffi_v2_last_large_callback_input_sum;
}

typedef struct {
  ffi_v2_i32_callback callback;
  int32_t value;
  int32_t result;
} ffi_v2_foreign_callback_call;

typedef struct {
  ffi_v2_i32_callback callback;
  int32_t value;
} ffi_v2_callback_holder;

typedef struct {
  ffi_v2_large_vector_callback callback;
  ffi_v2_large_vector value;
  ffi_v2_large_vector result;
} ffi_v2_foreign_large_callback_call;

#if defined(_WIN32)
static DWORD WINAPI ffi_v2_foreign_callback_thread(void* context) {
  ffi_v2_foreign_callback_call* call = (ffi_v2_foreign_callback_call*)context;
  call->result = call->callback(call->value);
  return 0;
}

static DWORD WINAPI ffi_v2_foreign_large_callback_thread(void* context) {
  ffi_v2_foreign_large_callback_call* call =
    (ffi_v2_foreign_large_callback_call*)context;
  call->result = call->callback(call->value);
  return 0;
}
#else
static void* ffi_v2_foreign_callback_thread(void* context) {
  ffi_v2_foreign_callback_call* call = (ffi_v2_foreign_callback_call*)context;
  call->result = call->callback(call->value);
  return NULL;
}

static void* ffi_v2_foreign_large_callback_thread(void* context) {
  ffi_v2_foreign_large_callback_call* call =
    (ffi_v2_foreign_large_callback_call*)context;
  call->result = call->callback(call->value);
  return NULL;
}
#endif

int32_t ffi_v2_call_i32_callback_on_foreign_thread(
  ffi_v2_i32_callback callback,
  int32_t value
) {
  ffi_v2_foreign_callback_call call = { callback, value, -1 };

#if defined(_WIN32)
  HANDLE thread = CreateThread(
    NULL,
    0,
    ffi_v2_foreign_callback_thread,
    &call,
    0,
    NULL
  );
  if (thread == NULL) return -2;
  WaitForSingleObject(thread, INFINITE);
  CloseHandle(thread);
#else
  pthread_t thread;
  if (pthread_create(&thread, NULL, ffi_v2_foreign_callback_thread, &call) != 0)
    return -2;
  pthread_join(thread, NULL);
#endif

  return call.result;
}

int32_t ffi_v2_call_callback_then_foreign_thread(
  ffi_v2_i32_callback callback,
  int32_t value
) {
  callback(value);
  return ffi_v2_call_i32_callback_on_foreign_thread(callback, value + 1);
}

int32_t ffi_v2_call_callback_holder_on_foreign_thread(
  ffi_v2_callback_holder holder
) {
  ffi_v2_foreign_callback_call call = { holder.callback, holder.value, -1 };

#if defined(_WIN32)
  HANDLE thread = CreateThread(
    NULL,
    0,
    ffi_v2_foreign_callback_thread,
    &call,
    0,
    NULL
  );
  if (thread == NULL) return -2;
  WaitForSingleObject(thread, INFINITE);
  CloseHandle(thread);
#else
  pthread_t thread;
  if (pthread_create(&thread, NULL, ffi_v2_foreign_callback_thread, &call) != 0)
    return -2;
  pthread_join(thread, NULL);
#endif

  return call.result;
}

void ffi_v2_capture_large_callback_result_on_foreign_thread(
  ffi_v2_large_vector_callback callback,
  ffi_v2_large_vector value
) {
  ffi_v2_foreign_large_callback_call call;
  call.callback = callback;
  call.value = value;
  memset(&call.result, 0xff, sizeof(call.result));

#if defined(_WIN32)
  HANDLE thread = CreateThread(
    NULL,
    0,
    ffi_v2_foreign_large_callback_thread,
    &call,
    0,
    NULL
  );
  if (thread == NULL) {
    ffi_v2_last_large_callback_sum = -2.0;
    return;
  }
  WaitForSingleObject(thread, INFINITE);
  CloseHandle(thread);
#else
  pthread_t thread;
  if (pthread_create(
    &thread,
    NULL,
    ffi_v2_foreign_large_callback_thread,
    &call
  ) != 0) {
    ffi_v2_last_large_callback_sum = -2.0;
    return;
  }
  pthread_join(thread, NULL);
#endif

  ffi_v2_last_large_callback_sum =
    call.result.first + call.result.second + call.result.third;
}
