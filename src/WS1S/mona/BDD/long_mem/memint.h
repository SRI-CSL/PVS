/* Memory management internal definitions */

/* #define USE_MALLOC_FREE */
#define USE_MALLOC_FREE 

#if !defined(_MEMINTH)
#define _MEMINTH


/* All user-visible stuff */

#include "memuser.h"

/* >>> Potentially system dependent configuration stuff */
/* See memuser.h as well. */

/* The storage management library can either use system-provided */
/* versions of malloc, free and friends, or it can implement a buddy */
/* scheme based on something like sbrk.  If you want to do the former, */
/* define USE_MALLOC_FREE. */

/* #define USE_MALLOC_FREE */

/* Now we need macros for routines to copy and zero-fill blocks of */
/* memory, and to either do malloc/free/whatever or to do an sbrk.  Since */
/* different systems have different types that these routines expect, we */
/* wrap everything in macros. */

#if defined(USE_MALLOC_FREE)
#if defined(__STDC__)
extern void *malloc(unsigned long);
extern void free(void *);
extern void *realloc(void *, unsigned long);
#define MALLOC(size) ((pointer)malloc((unsigned long)(size)))
#define FREE(p) (free((void *)(p)))
#define REALLOC(p, size) ((pointer)realloc((void *)(p), (unsigned long)(size)))
#else
extern char *malloc();
extern void free();
extern char *realloc();
#define MALLOC(size) ((pointer)malloc((int)(size)))
#define FREE(p) (free((char *)(p)))
#define REALLOC(p, size) ((pointer)realloc((char *)(p), (int)(size)))
#endif
#else
#if defined(__STDC__)
extern char *sbrk(int);
#define SBRK(size) ((pointer)sbrk((int)(size)))
#else
extern char *sbrk();
#define SBRK(size) ((pointer)sbrk((int)(size)))
#endif
#endif

/* You may need to muck with these depending on whether you have */
/* bcopy or memcpy. */

#if defined(__STDC__)
extern void *memcpy(const void *, const void *, size_t);
extern void *memset(void *, int, size_t);
#define MEM_COPY(dest, src, size) (void)memcpy((void *)(dest), (const void *)(src), (unsigned long)(size))
#define MEM_ZERO(ptr, size) (void)memset((void *)(ptr), 0, (unsigned long)(size))
#else
extern void bcopy();
extern void bzero();
#define MEM_COPY(dest, src, size) bcopy((char *)(src), (char *)(dest), (int)(size))
#define MEM_ZERO(ptr, size) bzero((char *)(ptr), (int)(size))
#endif


#if defined(__STDC__)
#define ARGS(args) args
#else
#define ARGS(args) ()
#endif


/* >>> System independent stuff here. */

struct segment_
{
  pointer base_address;
  SIZE_T limit;
};

typedef struct segment_ *segment;


struct block_
{
  int used;
  int size_index;
  struct block_ *next;
  struct block_ *prev;
  segment seg;
};

typedef struct block_ *block;


#define HEADER_SIZE ((SIZE_T)ROUNDUP(sizeof(struct block_)))
#define MAX_SIZE_INDEX (8*sizeof(SIZE_T)-2)
#define MAX_SEG_SIZE ((SIZE_T)1 << MAX_SIZE_INDEX)
#define MAX_SIZE ((SIZE_T)(MAX_SEG_SIZE-HEADER_SIZE))
#define MIN_ALLOC_SIZE_INDEX 15

#define NICE_BLOCK_SIZE ((SIZE_T)4096-ROUNDUP(sizeof(struct block_)))


extern void mem_fatal ARGS((char *));


#undef ARGS

#endif
