#ifndef VECTOR_H_
#define VECTOR_H_

#include <stddef.h>

#define DEFINE_VECTOR(Data, Typename) \
typedef struct Vector_##Typename { \
  size_t sz, cap; \
  Data *arr; \
} Vector_##Typename; \
void Vector_##Typename##_init(Vector_##Typename* v) { \
  v->sz = v->cap = 0; \
  v->arr = NULL; \
} \
void Vector_##Typename##_reserve(Vector_##Typename* v, size_t sz) { \
  if (sz <= v->cap) return; \
  v->arr = realloc(v->arr, sz * sizeof(Data)); \
  v->cap = sz; \
} \
void Vector_##Typename##_push(Vector_##Typename* v, Data x) { \
  if (v->sz == v->cap) Vector_##Typename##_reserve(v, v->cap * 3 / 2 + 2); \
  v->arr[v->sz++] = x; \
} \
void Vector_##Typename##_pop(Vector_##Typename* v) { \
  v->sz--; \
} \
void Vector_##Typename##_clear(Vector_##Typename* v) { \
  v->sz = 0; \
} \
size_t Vector_##Typename##_size(const Vector_##Typename* v) { \
  return v->sz; \
} \
Data* Vector_##Typename##_at(Vector_##Typename* v, size_t x) { \
  return v->arr + x; \
} \
Data* Vector_##Typename##_back(Vector_##Typename* v) { \
  return v->arr + (v->sz - 1); \
} \
void Vector_##Typename##_destroy(Vector_##Typename* v) { \
  if (v->arr) free(v->arr); \
}

#endif
