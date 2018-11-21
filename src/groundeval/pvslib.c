/**************************************
 *  WRAPPERS FOR MALLOC/REALLOC/FREE  *
 *************************************/

#include <assert.h>
#include <gmp.h>
#include "pvslib.h"

/*
 * Fatal error: out of memory
 */
void out_of_memory() {
  fprintf(stderr, "Out of memory\n");
  exit(PVS2C_EXIT_OUT_OF_MEMORY);
}


/*
 * Local malloc: abort if out of memory.
 *
 * Special case: if size = 0, malloc(size) may
 * return NULL on some systems, but that does not
 * mean we're out of memory.
 */
void *safe_malloc(size_t size) {
  void *tmp;

  tmp = malloc(size);
  if (tmp == NULL && size > 0) {
    out_of_memory();
  }

  return tmp;
}

/*
 * Safer realloc to support lazy allocation.
 * If ptr == NULL, call malloc otherwise call realloc.
 * Abort if out of memory.
 *
 * NOTE: C99 specifies that realloc should behave like
 * malloc if ptr is NULL. This is what the Linux default
 * malloc does, but it's not clear whether other malloc 
 * implementations (e.g., on MacOSX) follow the standard.
 * It's safer to check whether ptr is NULL and 
 * call malloc or realloc accordingly.
 *
 * size must be positive: realloc(p, 0) is the same as free(ptr).
 */
void *safe_realloc(void *ptr, size_t size) {
  void *tmp;

  assert(size > 0);
  if (ptr == NULL) {
    tmp = malloc(size);
  } else {
    tmp = realloc(ptr, size);
  }
  if (tmp == NULL) out_of_memory();

  return tmp;
}

//computes CPU time in seconds - from Yices
#include <time.h>

double get_cpu_time(void) {
  return ((double) clock())/CLOCKS_PER_SEC;
}

//hash functions
uint32_t mpz_hash(mpz_t x){
  uint64_t y;
  y = (uint64_t) mpz_get_ui(x);
  return uint64_hash(y);
}

uint32_t uint64_hash(uint64_t x){
  uint64_t y = x;
  y = ((y >> 30) ^ y) * UINT64_C(0xbf58476d1ce4e5b9);
  y = ((y >> 27) ^ y) * UINT64_C(0x94d049bb133111eb);
  y = (y >> 31);
  return y ^ 4294967295;
}

uint32_t uint32_hash(uint32_t x){
  uint32_t y = x;
  y = ((y >> 16) ^ y) * 0x45d9f3b;
  y = ((y >> 16) ^ y) * 0x45d9f3b;
  y = (y >> 16) ^ y;
  return y;
}

void mpz_add_si(mpz_t x, mpz_t y, int64_t i){
  if (i < 0) {
      mpz_add_ui(x, y, -i);
    } else {
    mpz_sub_ui(x, y, -i);
  }
}

uint32_t div_uint32_uint32(uint32_t x, uint32_t y){
  return x/y;
}

int32_t div_int32_uint32(int32_t x, uint32_t y){
  if (x < 0){
    int32_t q;
    q = -x/y;
    if (q*y < -x){
      return (-q)-1;
    } else {
      return -q;
    }
  }
  return x/y;
}

uint64_t div_uint64_uint64(int64_t x, uint64_t y){
  return x/y;
}

int64_t div_int64_uint64(int64_t x, uint64_t y){
  if (x < 0){
    int64_t q;
    q = -x/y;
    if (q*y < -x){
      return (-q)-1;
    } else {
      return -q;
    }
  }
  return x/y;
}

int64_t div_int64_uint32(int64_t x, uint32_t y){
  if (x < 0){
    int64_t q;
    q = -x/y;
    if (q*y < -x){
      return (-q)-1;
    } else {
      return -q;
    }
  }
  return x/y;
}

uint128_t div_uint128_uint128(int128_t x, uint128_t y){
  return x/y;
}

int128_t div_int128_uint128(int128_t x, uint128_t y){
  if (x < 0){
    int128_t q;
    q = -x/y;
    if (q*y < -x){
      return (-q)-1;
    } else {
      return -q;
    }
  }
  return x/y;
}


uint32_t rem_uint32_uint32(uint32_t x, uint32_t y){
  return x%y;
}

uint32_t rem_int32_uint32(int32_t x, uint32_t y){
  if (x < 0){
    int32_t r;
    r = (-x)%y;
    if (r == 0){
      return r;
    } else {
      return y - r;
    }
  }
  return x%y;
}

uint64_t rem_uint64_uint64(int64_t x, uint64_t y){
  return x%y;
}

int64_t rem_int64_uint64(int64_t x, uint64_t y){
  if (x < 0){
    int64_t r;
    r = (-x)%y;
    if (r == 0){
      return r;
    } else {
      return y - r;
    }
  }
  return x%y;
}


int64_t rem_int64_uint32(int64_t x, uint32_t y){
  if (x < 0){
    int64_t r;
    r = (-x)%y;
    if (r == 0){
      return r;
    } else {
      return y - r;
    }
  }
  return x%y;
}

uint128_t rem_uint128_uint128(int128_t x, uint128_t y){
  return x%y;
}

int128_t rem_int128_uint128(int128_t x, uint128_t y){
  if (x < 0){
    int128_t r;
    r = (-x)%y;
    if (r == 0){
      return r;
    } else {
      return y - r;
    }
  }
  return x%y;
}


