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
  uint64_t y = x + 1;
  y = ((y >> 30) ^ y) * UINT64_C(0xbf58476d1ce4e5b9);
  y = ((y >> 27) ^ y) * UINT64_C(0x94d049bb133111eb);
  y = (y >> 31);
  return y ^ 4294967295;
}

uint32_t uint32_hash(uint32_t x){
  uint32_t y = x  + 1;
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

void mpz_sub_si(mpz_t x, mpz_t y, int64_t i){
  if (i < 0) {
      mpz_add_ui(x, y, -i);
    } else {
    mpz_sub_ui(x, y, -i);
  }
}

void mpz_si_sub(mpz_t x, int64_t i, mpz_t y){
    mpz_set_si(x, i);
    mpz_sub(x, x, y);
}



//-------------------------------------------------------

uint32_t div_uint32_uint32(uint32_t x, uint32_t y){
  return x/y;
}

uint64_t div_uint64_uint32(uint64_t x, uint32_t y){
  return x/y;
}

uint32_t div_uint32_uint64(uint32_t x, uint64_t y){
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

uint64_t rem_uint64_uint64(uint64_t x, uint64_t y){
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

uint32_t rem_mpz_uint32(mpz_t x, uint32_t y){
  return mpz_fdiv_ui(x, y);  
}

mpz_ptr_t pvsfloor_q_z(mpq_t x){
        mpz_ptr_t result;
        result = safe_malloc(sizeof(mpz_t));
        mpz_init(result);
        mpz_set_q(result, x);

        return result;
}

mpq_ptr_t pvsfloor_q_q(mpq_t x){
        mpq_ptr_t result;
        result = safe_malloc(sizeof(mpq_t));
        mpq_init(result);
	mpz_ptr_t tmp;
	tmp = pvsfloor_q_z(x);
	mpq_set_z(result, tmp);
	mpz_clear(tmp);
        return result;
}

int64_t pvsfloor_q_i64(mpq_t x){
        int64_t result;
	mpz_ptr_t tmp;
	tmp = pvsfloor_q_z(x);
	result = mpz_get_si(tmp);
	mpz_clear(tmp);
        return result;
}

uint64_t pvsfloor_q_u64(mpq_t x){
        uint64_t result;
	mpz_ptr_t tmp;
	tmp = pvsfloor_q_z(x);
	result = mpz_get_si(tmp);
	mpz_clear(tmp);
        return result;
}


mpz_ptr_t pvsceiling_q_z(mpq_t x){
  mpz_ptr_t result;
  result = safe_malloc(sizeof(mpz_t));
  mpz_init(result);
  mpz_cdiv_q(result, mpq_numref(x), mpq_denref(x));
  //  gmp_printf("\npvsceiling_q_z(%Qd) = %Zd", x, result);
  return result;
}

mpq_ptr_t pvsceiling_q_q(mpq_t x){
  mpq_ptr_t result;
  result = safe_malloc(sizeof(mpq_t));
  mpq_init(result);
  mpz_ptr_t tmp = pvsceiling_q_z(x);
  mpq_set_z(result, tmp);
  mpz_clear(tmp);
  return result;
}

int64_t pvsceiling_q_i64(mpq_t x){
  int64_t result;
  mpz_ptr_t tmp = pvsceiling_q_z(x);
  result = mpz_get_si(tmp);
  mpz_clear(tmp);
  return result;
}

uint64_t pvsceiling_q_u64(mpq_t x){
  uint64_t result;
  mpz_ptr_t tmp = pvsceiling_q_z(x);
  result = mpz_get_ui(tmp);
  mpz_clear(tmp);
  return result;
}

uint64_t mpq_get_ui(mpq_t x){
  uint64_t result;
  mpz_t tmp;
  mpz_init(tmp);
  mpz_set_q(tmp, x);
  result =  mpz_get_ui(tmp);
  mpz_clear(tmp);
  return result;
}

uint64_t mpq_get_si(mpq_t x){
  uint64_t result;
  mpz_t tmp;
  mpz_init(tmp);
  mpz_set_q(tmp, x);
  result =  mpz_get_si(tmp);
  mpz_clear(tmp);
  return result;
}

mpz_ptr_t mpz_add_q(mpz_t ret, mpz_t x, mpq_t y){
  mpz_set_q(ret, y);
  mpz_add(ret, ret, x);
  return ret;
}

mpz_ptr_t mpz_sub_q(mpz_t ret, mpz_t x, mpq_t y){
  mpz_set_q(ret, y);
  mpz_sub(ret, x, ret);
  return ret;
}

mpz_ptr_t mpz_mul_q(mpz_t ret, mpz_t x, mpq_t y){
  mpz_set_q(ret, y);
  mpz_mul(ret, ret, x);
  return ret;
}

mpq_ptr_t mpq_add_si(mpq_t ret, mpq_t x, int64_t y){
  mpq_set_si(ret, y, 1);
  mpq_add(ret, ret, x);
  return ret;
}

mpq_ptr_t mpq_sub_si(mpq_t ret, mpq_t x, int64_t y){
  mpq_set_si(ret, -y, 1);
  mpq_add(ret, ret, x);
  return ret;
}

mpq_ptr_t mpq_mul_si(mpq_t ret, mpq_t x, int64_t y){
  mpq_set_si(ret, y, 1);
  mpq_mul(ret, ret, x);
  return ret;
}

mpq_ptr_t mpq_add_ui(mpq_t ret, mpq_t x, uint64_t y){
  mpq_set_ui(ret, y, 1);
  mpq_add(ret, ret, x);
  return ret;
}

mpq_ptr_t mpq_sub_ui(mpq_t ret, mpq_t x, uint64_t y){
  mpq_t tmp;
  mpq_init(tmp);
  mpq_set_ui(tmp, y, 1);
  mpq_sub(ret, x, tmp);
  mpq_clear(tmp);
  return ret;
}

mpq_ptr_t mpq_mul_ui(mpq_t ret, mpq_t x, uint64_t y){
  mpq_set_ui(ret, y, 1);
  mpq_mul(ret, ret, x);
  return ret;
}

mpq_ptr_t mpq_add_z(mpq_t ret, mpq_t x, mpz_t y){
  mpq_set_z(ret, y);
  mpq_add(ret, ret, x);
  return ret;
}

mpq_ptr_t mpq_sub_z(mpq_t ret, mpq_t x, mpz_t y){
  mpq_set_z(ret, y);
  mpq_sub(ret, x, ret);
  return ret;
}

mpq_ptr_t mpq_mul_z(mpq_t ret, mpq_t x, mpz_t y){
  mpq_set_z(ret, y);
  mpq_mul(ret, ret, x);
  return ret;
}



//---------------------------------------------------------------






/* struct type_actuals; */
/* typedef struct type_actual_s * type_actual_t; */

/* struct type_actual_s { */
/*   char tag;  //type tag: either `z' (mpz), `q' (mpq), `i' (uint_64 or int64_t), */
/*             //`p' (pointer, the only one for which the release pointer isn't NULL) */
/*   void (* release_ptr)(void *, type_actual_t[]); */
/*   bool_t (* eq_ptr)(void *, void *, type_actual_t[]); */
/*   uint32_t numparams; //This could fail if the number of parameters exceeds 2^32-1! */
/*   type_actual_t params[];//These are the further free parameters in the type of the actual.   */
/* }; */

/* void do_release(type_actual_t t, void * arg, type_actual_t * params){ */
/*   t->release_ptr(arg, params); */
/* }; */

/* bool_t do_equal(type_actual_t t, void * arg1, void * arg2, type_actual_t * params){ */
/*   return t->eq_ptr(arg1, arg2, params); */
/* }; */

//------------------------------------------------------------------




uint32_t code(uint32_t x){
  return x;
};



stringliteral_t mk_string(uint32_t length, uint32_t * instring){ 
  stringliteral_t result = (stringliteral_t) safe_malloc(sizeof(struct stringliteral_s) + (length  * sizeof(uint32_t)));
  //  printf("\nmk_string input =");
  //for (uint32_t i = 0; i < length; i++) printf(" %"PRIu32",", instring[i]); 

   result->count = 1;
   result->size = length;
   result->max = length;
   memcpy(result->elems, (uint32_t *) instring, 4 * length);
   //printf("\nmk_string output: count = %"PRIu32", size = %"PRIu32", max = %"PRIu32"\n",
   //	  result->count, result->size, result->max);
   //for (uint32_t j = 0; j < length; j++) printf(" %"PRIu32",", result->elems[j]);    
   return result; 
 };

char * byte2cstring(uint32_t length, uint8_t * bstring){
  uint32_t slength = length++;
  char * outstring = (char *) safe_malloc(slength++);
  memcpy(outstring, bstring, length);
  outstring[length] = '\0';
  return outstring;
};

bool_t equal_uint64(pointer_t x, pointer_t y, ...){
  uint64_t ux = (uint64_t)x;
  uint64_t uy = (uint64_t)y;
  return (ux == uy);
};

void release_uint64(pointer_t x, ...){
};

void release_file__file(file_t file){
  if (file->count <= 1){
    munmap(file->contents, file->capacity);
    safe_free(file);
  } else
    {
      file->count--;
    }
}

bool_t equal_file__file(file_t file1, file_t file2){
  bool_t result =  (file1 == file2);
  release_file__file(file1);
  release_file__file(file2);
  return result;
}
//------------------------------------------------------------------
//json printing routines for the various types

//safe_strcat allocates enough memory to concat s1 and s2.  It possibly mutates/frees s1 and does not free s2. 
char * safe_strcat(char * s1, char * s2){
  size_t l1 = strlen(s1);
  size_t l2 = strlen(s2);
  size_t slack2 = (SIZE_MAX - l1)/2;
  if (slack2 < l2) out_of_memory();//should this be a different error message
  char * new_s1 = safe_realloc(s1, l1 + 2*l2);
  strcat(new_s1, s2);
  return new_s1;
}

//

char * json_char(uint32_t x){
  char * out = safe_malloc(8);
  sprintf(out, "%"PRIu32"", x);
  return out;
}

char * json_uint8(uint8_t x){
  char * out = safe_malloc(8);
  sprintf(out, "%"PRIu8"", x);
  return out;
}

char * json_uint16(uint16_t x){
  char * out = safe_malloc(8);
  sprintf(out, "%"PRIu16"", x);
  return out;
}

char * json_uint32(uint32_t x){
  char * out = safe_malloc(8);
  sprintf(out, "%"PRIu32"", x);
  return out;
}

char * json_uint64(uint64_t x){
  char * out = safe_malloc(16);
  sprintf(out, "%"PRIu64"", x);
  return out;
}

char * json_int8(int8_t x){
  char * out = safe_malloc(8);
  sprintf(out, "%"PRId8"", x);
  return out;
}

char * json_int16(int16_t x){
  char * out = safe_malloc(8);
  sprintf(out, "%"PRId16"", x);
  return out;
}

char * json_int32(int32_t x){
  char * out = safe_malloc(8);
  sprintf(out, "%"PRId32"", x);
  return out;
}

char * json_int64(int64_t x){
  char * out = safe_malloc(16);
  sprintf(out, "%"PRId64"", x);
  return out;
}

char * json_mpz(mpz_t x){
  return mpz_get_str(NULL, 10, x);//base 10
}

char * json_mpq(mpq_t x){
  return mpq_get_str(NULL, 10, x);//base 10
}

/* char * json_bytestrings__bytestring(bytestrings__bytestring_t b){ */
/*   char * out = byte2cstring(b->length, b->seq); */
/*   return out; */
/* } */

char * json_list_with_sep(char ** input, uint32_t length, char open, char sep, char close){
  char * op = safe_malloc(8);
  sprintf(op, "%c", open);
  char * tmp = op;
  if (length > 0){
		  tmp = safe_strcat(tmp, input[0]);
  };
  char * sepstring = safe_malloc(8);
  sprintf(sepstring, "%c ", sep);
  for (uint32_t i = 1; i < length; i++){
    tmp = safe_strcat(safe_strcat(tmp, sepstring), input[i]);
  };
  //does not free input, it has to be freed by the caller
  char * closestring = safe_malloc(8);
  sprintf(closestring, "%c", close);
  tmp = safe_strcat(tmp, closestring);
  return tmp;
}
