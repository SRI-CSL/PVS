#ifndef _pvslib_h 
#define _pvslib_h

#include<stdio.h>

#include<stdlib.h>

#include<inttypes.h>

#include<stdbool.h>

#include<gmp.h>

//exit codes
#define PVS2C_EXIT_OUT_OF_MEMORY   16
#define PVS2C_EXIT_SYNTAX_ERROR    17
#define PVS2C_EXIT_FILE_NOT_FOUND  18
#define PVS2C_EXIT_USAGE           19
#define PVS2C_EXIT_ERROR           20

#define PVS2C_EXIT_SUCCESS         EXIT_SUCCESS

typedef bool bool_t;
typedef __int128_t int128_t;
typedef  __uint128_t uint128_t;
typedef mpz_ptr mpz_ptr_t;

/*
 * Print an error message then call exit(MCSAT_EXIT_OUT_OF_MEMORY)
 */
extern void out_of_memory(void) __attribute__ ((noreturn));

/*
 * Wrappers for malloc/realloc.
 */
extern void *safe_malloc(size_t size) __attribute__ ((malloc)); 
extern void *safe_realloc(void *ptr, size_t size) __attribute__ ((malloc));
/*
 * Safer free: check whether ptr is NULL before calling free.
 *
 * NOTE: C99 specifies that free shall have no effect if ptr
 * is NULL. It's safer to check anyway.
 */
static inline void safe_free(void *ptr) {
  if (ptr != NULL) free(ptr);
}

extern double get_cpu_time(void);

extern uint32_t mpz_hash(mpz_t x);
extern uint32_t uint64_hash(uint64_t x);
extern uint32_t uint32_hash(uint32_t x);
extern void mpz_add_si(mpz_t x, mpz_t y, int64_t i);
extern uint32_t div_uint32_uint32(uint32_t x, uint32_t y);
extern int32_t div_int32_uint32(int32_t x, uint32_t y);
extern uint64_t div_uint64_uint64(int64_t x, uint64_t y);
extern int64_t div_int64_uint64(int64_t x, uint64_t y);
extern uint128_t div_uint128_uint128(int128_t x, uint128_t y);
extern int128_t div_int128_uint128(int128_t x, uint128_t y);
extern uint32_t rem_uint32_uint32(uint32_t x, uint32_t y);
extern uint32_t rem_int32_uint32(int32_t x, uint32_t y);
extern uint64_t rem_uint64_uint64(int64_t x, uint64_t y);
extern int64_t rem_int64_uint64(int64_t x, uint64_t y);
extern uint128_t rem_uint128_uint128(int128_t x, uint128_t y);
extern int128_t rem_int128_uint128(int128_t x, uint128_t y);


#define HTBL_DEFAULT_SIZE 32
#define HTBL_MAX_SIZE (UINT32_MAX/8)


#endif
