/*
 DOCUMENTATION INFORMATION			     module: MEMORY ALLOCATION
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : alloc.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1988-1995 G.L.J.M. Janssen
 date	   : 26-NOV-1995
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef ALLOC_H
#define ALLOC_H

extern void *MA_Malloc (long nr_bytes, const char* r, const char* fn, long ln);
extern void *MA_Calloc (long nr_elems, long bytes_per_elem, const char* r,
			const char* fn, long ln);
extern void *MA_Realloc (void *p, long nr_bytes, const char* r,
			 const char* fn, long ln);
extern void *MA_Realloc2 (void *p, long nr_bytes, long old, const char* r,
			  const char* fn, long ln);
extern char *MA_CopyString (const char *cp, const char* r,
			    const char* fn, long ln);
extern char *MA_CopyString1 (const char *cp, long len, const char* r,
			     const char* fn, long ln);

extern void MA_Free (void *p, long nr_bytes, const char* r,
		     const char* fn, long ln);

#ifdef HAVE_ALLOCA
#include <alloca.h>
#endif

/* Allocates n consecutive bytes, returns pointer of type. */
#define MALLOC_BYTES(n, type) \
	((type) MA_Malloc (n, "MALLOC_BYTES", __FILE__, __LINE__))

#define MA_MALLOC_BYTES		MALLOC_BYTES

/* Allocates n consecutive bytes (zeroed), returns pointer of type. */
#define CALLOC_BYTES(n, type) \
	((type) MA_Calloc (n, sizeof (char), "CALLOC_BYTES", \
			   __FILE__, __LINE__))

#define MA_CALLOC_BYTES		CALLOC_BYTES

/* Reallocates p, to point to a stretch of n bytes. */
#define REALLOC_BYTES(p, n, type) \
	((type) MA_Realloc (p, (n) * sizeof (char), "REALLOC_BYTES", \
			    __FILE__, __LINE__))

/* Reallocates p, to point to a stretch of n bytes (m bytes originally). */
#define MA_REALLOC_BYTES(p, n, type, m) \
	((type) MA_Realloc2 (p, (n) * sizeof (char), (m) * sizeof (char), \
			     "MA_REALLOC_BYTES", \
			     __FILE__, __LINE__))

/* Allocates n elements of type. */
#define MALLOC_ARRAY(n, type) \
	((type *) MA_Malloc ((n) * sizeof (type), "MALLOC_ARRAY", \
			     __FILE__, __LINE__))

#define MA_MALLOC_ARRAY		MALLOC_ARRAY

/* Allocates n elements of type, all elements zeroed. */
#define CALLOC_ARRAY(n, type) \
	((type *) MA_Calloc (n, sizeof (type), "CALLOC_ARRAY", \
			     __FILE__, __LINE__))

#define MA_CALLOC_ARRAY		CALLOC_ARRAY

#ifdef HAVE_ALLOCA
/* Allocates n elements of type, on stack. */
#define ALLOCA_ARRAY(n, type) \
  (type *) (alloca ((n) * sizeof (type)))
#endif

/* Reallocates array of type to n elements. */
#define REALLOC_ARRAY(arr, n, type) \
	((type *) MA_Realloc (arr, (n) * sizeof (type), "REALLOC_ARRAY", \
			      __FILE__, __LINE__))

/* Reallocates array of type to n elements (m elements originally). */
#define MA_REALLOC_ARRAY(arr, n, type, m) \
	((type *) MA_Realloc2 (arr, (n) * sizeof (type), (m) * sizeof (type), \
		       "MA_REALLOC_ARRAY", \
		       __FILE__, __LINE__))

/* Allocates string of len chars (takes care of extra null-terminator byte). */
#define CALLOC_STRING(len) \
	((char *) MA_Calloc ((len)+1, sizeof (char), "CALLOC_STRING", \
			     __FILE__, __LINE__))

#define COPY_STRING(s) \
	MA_CopyString (s, "COPY_STRING", __FILE__, __LINE__)

/* Synonym for COPY_STRING: */
#define NEW_STRING(s) \
	MA_CopyString (s, "NEW_STRING", __FILE__, __LINE__)

/* Synonym for COPY_STRING: */
#ifndef Strdup
#define Strdup(s) \
	MA_CopyString (s, "Strdup", __FILE__, __LINE__)
#endif

#define COPY_STRING_1(s, l) \
	MA_CopyString1 (s, l, "COPY_STRING_1", __FILE__, __LINE__)

/* Allocates struct of type. */
#define MALLOC_STRUCT(type) \
	((type *) MA_Malloc (sizeof (type), "MALLOC_STRUCT", \
			     __FILE__, __LINE__))

/* Allocates struct of type, all fields zeroed. */
#define CALLOC_STRUCT(type) \
	((type *) MA_Calloc (1, sizeof (type), "CALLOC_STRUCT", \
			     __FILE__, __LINE__))

#define MA_FREE_BYTES(p, n) \
	MA_Free ((void *) p, n, "MA_FREE_BYTES", \
		 __FILE__, __LINE__)

#define MA_FREE_STRING(s) \
	MA_Free ((void *) s, strlen (s)+1, "MA_FREE_STRING", \
		 __FILE__, __LINE__)

#define MA_FREE_STRUCT(p, type) \
	MA_Free ((void *) p, sizeof (type), "MA_FREE_STRUCT", \
		 __FILE__, __LINE__)

#define MA_FREE_ARRAY(p, n, type) \
	MA_Free ((void *) p, (n) * sizeof (type), "MA_FREE_ARRAY", \
		 __FILE__, __LINE__)

#ifdef HAVE_ALLOCA
/* Allocates struct of type, on stack. */
#define ALLOCA_STRUCT(type) \
  (type *) (alloca (sizeof (type)))
#endif

extern long MA_bytes_allocated (void);

void MA_set_memsize_limit_and_handler (long limit,
				       long (*handler) (long nr_bytes,
							const char* fn,
							long ln));
long MA_memsize_limit (void);

extern void print_message (const char *code, const char *format, ...);

#ifdef DEBUG
#define print_debug0(code,Format)	print_message(code,Format)
#define print_debug1(code,Format,arg1)	print_message(code,Format,arg1)
#define print_debug2(code,Format,arg1,arg2) \
	print_message(code,Format,arg1,arg2)
#define print_debug3(code,Format,arg1,arg2,arg3) \
	print_message(code,Format,arg1,arg2,arg3)
#define print_debug4(code,Format,arg1,arg2,arg3,arg4) \
	print_message(code,Format,arg1,arg2,arg3,arg4)
#else					/* NO Debug  */
#define print_debug0(code,Format) 
#define print_debug1(code,Format,arg1) 
#define print_debug2(code,Format,arg1,arg2) 
#define print_debug3(code,Format,arg1,arg2,arg3) 
#define print_debug4(code,Format,arg1,arg2,arg3,arg4) 
#endif /* DEBUG */

#endif /* ALLOC_H */
