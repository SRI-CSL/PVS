/* Memory management user-visible definitions */


#if !defined(_MEMUSERH)
#define _MEMUSERH


#include <stdio.h>


#if defined(__STDC__)
#define ARGS(args) args
#else
#define ARGS(args) ()
#endif


/* >>> Potentially machine dependent stuff */
/* See memint.h as well. */

typedef unsigned long INT_PTR;	/* Integral type that can hold a pointer */
typedef unsigned long SIZE_T;	/* Integral type that can hold the maximum */
				/* size of an object */

/* REQUIRED_ALIGNMENT is the alignment required by the machine hardware; */
/* it is provided for user use. */

#define REQUIRED_ALIGNMENT 4


/* Types */

#if defined(__STDC__)
typedef void *pointer;
#else
typedef char *pointer;
#endif


typedef struct rec_mgr_ *rec_mgr;


/* ALLOC_ALIGNMENT is the alignment for all storage returned by the */
/* storage allocation routines. */

/* #define ALLOC_ALIGNMENT 8
 * 
 */

#define ALLOC_ALIGNMENT 4

/* Round a size up for alignment */

#define ROUNDUP(size) ((((size)+ALLOC_ALIGNMENT-1)/ALLOC_ALIGNMENT)*ALLOC_ALIGNMENT)
#define ALIGN(size) ((((size)+REQUIRED_ALIGNMENT-1)/REQUIRED_ALIGNMENT)*REQUIRED_ALIGNMENT)


/* Block storage management routines */

extern "C" pointer mem_get_block ARGS((SIZE_T));
extern "C" void mem_free_block ARGS((pointer));
extern "C" pointer mem_resize_block ARGS((pointer, SIZE_T));
extern "C" void mem_copy ARGS((pointer, pointer, SIZE_T));
extern "C" void mem_zero ARGS((pointer, SIZE_T));
extern "C" SIZE_T mem_allocation ARGS((void));
extern "C" char *mem_version ARGS((void));


/* Record manager routines */

extern pointer mem_new_rec ARGS((rec_mgr));
extern void mem_free_rec ARGS((rec_mgr, pointer));
extern rec_mgr mem_new_rec_mgr ARGS((int));
extern void mem_free_rec_mgr ARGS((rec_mgr));


#undef ARGS

#endif
