// Tiny fixture shared library for FFI integration tests.
// Build: cc -shared -o libfixture.dylib fixture.c  (macOS)
//        cc -shared -fPIC -o libfixture.so fixture.c  (Linux)
//        cl /LD fixture.c /Fe:fixture.dll  (Windows)

#include <string.h>
#include <stdlib.h>

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
