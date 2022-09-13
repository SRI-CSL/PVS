#ifndef _pvslib_h 
#define _pvslib_h

#include<stdio.h>

#include<stdlib.h>

#include<inttypes.h>

#include<stdbool.h>

#include<gmp.h>

#include<string.h>

#include <sys/types.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>


//exit codes
#define PVS2C_EXIT_OUT_OF_MEMORY   16
#define PVS2C_EXIT_SYNTAX_ERROR    17
#define PVS2C_EXIT_FILE_NOT_FOUND  18
#define PVS2C_EXIT_USAGE           19
#define PVS2C_EXIT_ERROR           20

#define PVS2C_EXIT_SUCCESS         EXIT_SUCCESS

#define _mk_name(module, op) module ## _ ##  op


typedef bool bool_t;
typedef uint32_t char_t; //was char now uint32_t for expanded character code set
typedef __int128_t int128_t;
typedef  __uint128_t uint128_t;
typedef mpz_ptr mpz_ptr_t;
typedef mpq_ptr mpq_ptr_t;

#define pvs2cerror(msg, code) printf("\n%s", msg); exit(code)

/*
 * Print an error message then call exit(PVS2C_EXIT_OUT_OF_MEMORY)
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

static inline void release_mpz(mpz_ptr_t x){
  mpz_clear(x);
  safe_free(x);
}

static inline void release_mpq(mpq_ptr_t x){
  mpq_clear(x);
  safe_free(x);
}

#define q_init(var, stmt) var = (mpq_ptr)safe_malloc(sizeof(mpq_t)); mpq_init(var); stmt

#define z_init(var, stmt) var = (mpz_ptr)safe_malloc(sizeof(mpz_t)); mpz_init(var); stmt

extern double get_cpu_time(void);

static inline bool_t u_undef_quant_expr(void){
  return false;
};

extern uint32_t mpz_hash(mpz_t x);
extern uint32_t uint64_hash(uint64_t x);
extern uint32_t uint32_hash(uint32_t x);
extern void mpz_add_si(mpz_t x, mpz_t y, int64_t i);
extern void mpz_sub_si(mpz_t x, mpz_t y, int64_t i);
extern void mpz_si_sub(mpz_t x, int64_t i, mpz_t y);
extern uint32_t div_uint32_uint32(uint32_t x, uint32_t y);
extern uint64_t div_uint64_uint32(uint64_t x, uint32_t y);
extern uint32_t div_uint32_uint64(uint32_t x, uint64_t y);
extern int32_t div_int32_uint32(int32_t x, uint32_t y);
extern uint64_t div_uint64_uint64(int64_t x, uint64_t y);
extern int64_t div_int64_uint64(int64_t x, uint64_t y);
extern int64_t div_int64_uint32(int64_t x, uint32_t y);
extern uint128_t div_uint128_uint128(int128_t x, uint128_t y);
extern int128_t div_int128_uint128(int128_t x, uint128_t y); //deprecated
extern uint32_t rem_uint32_uint32(uint32_t x, uint32_t y);
extern uint32_t rem_int32_uint32(int32_t x, uint32_t y);
extern uint64_t rem_uint64_uint64(uint64_t x, uint64_t y);
extern int64_t rem_int64_uint64(int64_t x, uint64_t y);
extern int64_t rem_int64_uint32(int64_t x, uint32_t y);
extern uint128_t rem_uint128_uint128(int128_t x, uint128_t y); //deprecated
extern int128_t rem_int128_uint128(int128_t x, uint128_t y); //deprecated
extern uint32_t rem_mpz_uint32(mpz_t x, uint32_t y); 
extern mpz_ptr_t pvsfloor_q_z(mpq_t x);
extern mpq_ptr_t pvsfloor_q_q(mpq_t x);
extern int64_t pvsfloor_q_i64(mpq_t x);
extern uint64_t pvsfloor_q_u64(mpq_t x);
extern mpz_ptr_t pvsceiling_q_z(mpq_t x);
extern mpq_ptr_t pvsceiling_q_q(mpq_t x);
extern int64_t pvsceiling_q_i64(mpq_t x);
extern uint64_t pvsceiling_q_u64(mpq_t x);
extern uint64_t mpq_get_ui(mpq_t x);
extern uint64_t mpq_get_si(mpq_t x);
extern mpz_ptr_t mpz_add_q(mpz_t ret, mpz_t x, mpq_t y);
extern mpz_ptr_t mpz_sub_q(mpz_t ret, mpz_t x, mpq_t y);
extern mpz_ptr_t mpz_mul_q(mpz_t ret, mpz_t x, mpq_t y);
extern mpq_ptr_t mpq_add_si(mpq_t ret, mpq_t x, int64_t y);
extern mpq_ptr_t mpq_sub_si(mpq_t ret, mpq_t x, int64_t y);
extern mpq_ptr_t mpq_mul_si(mpq_t ret, mpq_t x, int64_t y);
extern mpq_ptr_t mpq_add_ui(mpq_t ret, mpq_t x, uint64_t y);
extern mpq_ptr_t mpq_sub_ui(mpq_t ret, mpq_t x, uint64_t y);
extern mpq_ptr_t mpq_mul_ui(mpq_t ret, mpq_t x, uint64_t y);
extern mpq_ptr_t mpq_add_z(mpq_t ret, mpq_t x, mpz_t y);
extern mpq_ptr_t mpq_sub_z(mpq_t ret, mpq_t x, mpz_t y);
extern mpq_ptr_t mpq_mul_z(mpq_t ret, mpq_t x, mpz_t y);

//------------------------------------------------------------------
#define mpz_mk_set(ret, x)   z_init(ret, mpz_set(ret, x))


#define mpz_mk_add(ret, x, y)  z_init(ret, mpz_add(ret, x, y));


#define mpz_mk_sub(ret, x, y)  z_init(ret, mpz_sub(ret, x, y));


#define mpz_mk_mul(ret, x, y)  z_init(ret, mpz_mul(ret, x, y));


#define mpz_mk_set_si(ret, x)  z_init(ret, mpz_set_si(ret, x));


#define mpz_mk_add_si(ret, x, y)  z_init(ret, mpz_add_si(ret, x, y));


#define mpz_mk_sub_si(ret, x, y)  z_init(ret, mpz_sub_si(ret, x, y));


#define mpz_mk_mul_si(ret, x, y)  z_init(ret, mpz_mul_si(ret, x, y));



#define mpz_mk_set_ui(ret, x)  z_init(ret, mpz_set_ui(ret, x));


#define mpz_mk_add_ui(ret, x, y)  z_init(ret, mpz_add_ui(ret, x, y));


#define mpz_mk_sub_ui(ret, x, y)  z_init(ret, mpz_sub_ui(ret, x, y));


#define mpz_mk_mul_ui(ret, x, y)  z_init(ret, mpz_mul_ui(ret, x, y));


#define mpz_mk_set_q(ret,  x)  z_init(ret, mpz_set_q(ret, x));


#define mpz_mk_add_q(ret, x,  y)  z_init(ret, mpz_add_q(ret, x, y));


#define mpz_mk_sub_q(ret, x,  y)  z_init(ret, mpz_sub_q(ret, x, y));


#define mpz_mk_mul_q(ret, x,  y)  z_init(ret, mpz_mul_q(ret, x, y));

#define mpz_mk_fdiv_q(ret, x, y) z_init(ret, mpz_fdiv_q(ret, x, y));

#define mpz_mk_fdiv_r(ret, x, y) z_init(ret, mpz_fdiv_r(ret, x, y));

#define mpz_mk_fdiv_q_ui(ret, x, y) z_init(ret, mpz_fdiv_q_ui(ret, x, y));

#define mpz_mk_fdiv_r_ui(ret, x, y) z_init(ret, mpz_fdiv_r_ui(ret, x, y));



//--------------------------------------------------------------
#define mpq_mk_set(ret,  x)  q_init(ret, mpq_set(ret, x));


#define mpq_mk_add(ret,  x, y)  q_init(ret, mpq_add(ret, x, y));


#define mpq_mk_sub(ret,  x, y)  q_init(ret, mpq_sub(ret, x, y));


#define mpq_mk_mul(ret,  x, y)  q_init(ret, mpq_mul(ret, x, y));


#define mpq_mk_set_si(ret, x)  q_init(ret, mpq_set_si(ret, x, 1));


#define mpq_mk_add_si(ret,  x, y)  q_init(ret, mpq_add_si(ret, x, y));


#define mpq_mk_sub_si(ret,  x, y)  q_init(ret, mpq_sub_si(ret, x, y));


#define mpq_mk_mul_si(ret,  x, y)  q_init(ret, mpq_mul_si(ret, x, y));



#define mpq_mk_set_ui(ret, x)  q_init(ret, mpq_set_ui(ret, x, 1));


#define mpq_mk_add_ui(ret,  x, y)  q_init(ret, mpq_add_ui(ret, x, y));


#define mpq_mk_sub_ui(ret,  x, y)  q_init(ret, mpq_sub_ui(ret, x, y));


#define mpq_mk_mul_ui(ret,  x, y)  q_init(ret, mpq_mul_ui(ret, x, y));


#define mpq_mk_set_z(ret, x)  q_init(ret, mpq_set_z(ret, x));


#define mpq_mk_add_z(ret,  x, y)  q_init(ret, mpq_add_z(ret, x, y));


#define mpq_mk_sub_z(ret,  x, y)  q_init(ret, mpq_sub_z(ret, x, y));


#define mpq_mk_mul_z(ret,  x, y)  q_init(ret, mpq_mul_z(ret, x, y));

#define mpq_mk_div(ret,  x, y)  q_init(ret, mpq_div(ret, x, y));





#define HTBL_DEFAULT_SIZE 32
#define HTBL_MAX_SIZE (UINT32_MAX/8)

/* struct funtable_s { */
/*   void * (* copyptr)(void *); */
/*   void (* relptr)(void *); */
/*   bool_t (* eqptr)(void *, void *); */
/* }; */
/* typedef struct funtable_s * funtable_t; */

typedef void * any_t;

struct stringliteral_s {
  uint32_t count;
  uint32_t size;
  uint32_t max;
  uint32_t elems[];
} stringliteral_s;
typedef struct stringliteral_s * stringliteral_t;

uint32_t code(uint32_t x);

extern stringliteral_t mk_string(uint32_t length, uint32_t * instring);

extern char * byte2cstring(uint32_t length, uint8_t * bstring);

/* struct bytestring_s { */
/*   uint32_t count; */
/*   uint32_t length; */
/*   uint8_t strval[]; */
/* } bytestring_s; */
/* typedef struct bytestring_s * bytestring_t; */

/* extern bytestring_t mk_bytestring(char * instring); */

//enum mode_t {r, w, a};

struct stream_s {
  uint32_t count;
  FILE * strm;
};

struct file_s {
  uint32_t count; //reference count
  uint64_t fd; //file descriptor
  uint32_t size;
  uint32_t capacity;
  char * name; 
  char * contents; 
} file_s;
typedef struct file_s * file_t;

extern void release_file__file(file_t file);

extern bool_t equal_file__file(file_t file1, file_t file2);

struct pointer_s {uint32_t count;};
typedef struct pointer_s * pointer_t;
typedef struct type_actual_s * type_actual_t;
typedef bool_t (*equal_ptr_t)(pointer_t x, pointer_t y, type_actual_t T);
typedef void (*release_ptr_t)(pointer_t x, type_actual_t T);


/* extern bool_t equal_uint64(pointer_t x, pointer_t y, ...); */

/* extern void release_uint64(pointer_t x, ...); */

 struct type_actual_s { 
   equal_ptr_t equal_ptr; 
   release_ptr_t release_ptr;
 };


 /* actual_uint64 = {(*equal_uint64), (*release_uint64)}; */


/* equal_ptr_t equal_uint64_ptr = (*equal_uint64); */
/* release_ptr_t release_uint64_ptr = (*release_uint64); */
/* struct type_actual_s actual_uint64 = (struct type_actual_s) */



//------------------------------------------------------------------
/* static inline uint8_t u8plus(uint8_t x, uint8_t y){return (uint8_t)(x + y);}; */
/* static inline uint16_t u16plus(uint16_t x, uint16_t y){return (uint16_t)(x + y);}; */
/* static inline uint32_t u32plus(uint32_t x, uint32_t y){return (uint32_t)(x + y);}; */
/* static inline uint64_t u64plus(uint64_t x, uint64_t y){return (uint64_t)(x + y);}; */

/* static inline uint8_t u8minus(uint8_t x, uint8_t y){return (uint8_t)(x - y);}; */
/* static inline uint16_t u16minus(uint16_t x, uint16_t y){return (uint16_t)(x - y);}; */
/* static inline uint32_t u32minus(uint32_t x, uint32_t y){return (uint32_t)(x - y);}; */
/* static inline uint64_t u64minus(uint64_t x, uint64_t y){return (uint64_t)(x - y);}; */

/* static inline  uint8_t u8times(uint8_t x, uint8_t y){return (uint8_t)(x * y);}; */
/* static inline uint16_t u16times(uint16_t x, uint16_t y){return (uint16_t)(x * y);}; */
/* static inline uint32_t u32times(uint32_t x, uint32_t y){return (uint32_t)(x * y);}; */
/* static inline uint64_t u64times(uint64_t x, uint64_t y){return (uint64_t)(x * y);}; */

/* static inline uint8_t u8div(uint8_t x, uint8_t y){return (uint8_t)(x/y);}; */
/* static inline uint16_t u16div(uint16_t x, uint16_t y){return (uint16_t)(x/y);}; */
/* static inline uint32_t u32div(uint32_t x, uint32_t y){return (uint32_t)(x/y);}; */
/* static inline uint64_t u64div(uint64_t x, uint64_t y){return (uint64_t)(x/y);}; */

/* static inline uint8_t u8rem(uint8_t x, uint8_t y){return (uint8_t)(x%y);}; */
/* static inline uint16_t u16rem(uint16_t x, uint16_t y){return (uint16_t)(x%y);}; */
/* static inline uint32_t u32rem(uint32_t x, uint32_t y){return (uint32_t)(x%y);}; */
/* static inline uint64_t u64rem(uint64_t x, uint64_t y){return (uint64_t)(x%y);}; */

/* static inline uint8_t u8pow2(uint8_t x){return (uint8_t)1<<x;}; */
/* static inline uint16_t u16pow2(uint16_t x){return (uint16_t)1<<x;}; */
/* static inline uint32_t u32pow2(uint32_t x){return (uint32_t)1<<x;}; */
/* static inline uint64_t u64pow2(uint64_t x){return (uint64_t)(1<<x);}; */

/* static inline uint8_t u8lshift(uint8_t x, uint8_t n){return (uint8_t)x<<n;}; */
/* static inline uint16_t u16lshift(uint16_t x, uint8_t n){return (uint16_t)x<<n;}; */
/* static inline uint32_t u32lshift(uint32_t x, uint8_t n){return (uint32_t)x<<n;}; */
/* static inline uint64_t u64lshift(uint64_t x, uint8_t n){return (uint64_t)(x<<n);}; */

/* static inline uint8_t u8rshift(uint8_t x, uint8_t n){return (uint8_t)x>>n;}; */
/* static inline uint16_t u16rshift(uint16_t x, uint8_t n){return (uint16_t)x>>n;}; */
/* static inline uint32_t u32rshift(uint32_t x, uint8_t n){return (uint32_t)x>>n;}; */
/* static inline uint64_t u64rshift(uint64_t x, uint8_t n){return (uint64_t)(x>>n);}; */


#endif
