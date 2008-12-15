/*
 DOCUMENTATION INFORMATION			     module: MEMORY ALLOCATION
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : alloc.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1988-1995 G.L.J.M. Janssen
 date	   :  5-NOV-1995
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>

#include "alloc.h"

/* Total bytes allocated via Memory Allocator package: */
static long bytes_allocated = 0;

long MA_bytes_allocated (void)
{
  return bytes_allocated;
}

/* User settable limit for package memory size: */
static long MA_allowed_memsize = INT_MAX;

/* Default function to call in case memory limit is exceeded: */
static long MA_default_memfull_handler (long nr_bytes, const char* fn,
					long ln)
{
  fprintf (stderr, "Memory limit (%ld Kb) exceeded. Exiting program...\n",
	   MA_allowed_memsize >> 10);
  fprintf (stderr, "(Last requested %ld bytes in file `%s' at line %ld.)\n",
	   nr_bytes, fn, ln);
  exit (0);
}

/* User settable variable for memory-exceeded-function: */
static long (*MA_memfull_handler) (long nr_bytes, const char* fn, long ln)
     = MA_default_memfull_handler;

void MA_set_memsize_limit_and_handler (long limit,
				       long (*handler) (long nr_bytes,
							const char* fn,
							long ln))
{
  if (handler)
    MA_memfull_handler = handler;

  MA_allowed_memsize = limit;
}

long MA_memsize_limit (void)
{
  return MA_allowed_memsize;
}

/* See if space limit will not be exceeded: */
static long MA_assure_alloc (long nr_bytes, const char* fn, long ln)
{
  if (bytes_allocated + nr_bytes > MA_allowed_memsize)
    return MA_memfull_handler (nr_bytes, fn, ln);
  return 0;
}

void *MA_Malloc (long nr_bytes, const char* r, const char* fn, long ln)
{
  void *p;

  if (nr_bytes < 0) {
    print_message ("F004",
		   "[%s]: Allocating nr. bytes < 0 in file `%s' at line %ld.",
		   r, fn, ln);
    exit (1);
  }

  if (!nr_bytes)
    print_message ("W002",
		   "[%s]: Allocating 0 bytes in file `%s' at line %ld.",
		   r, fn, ln);

  MA_assure_alloc (nr_bytes, fn, ln);

  if (!(p = malloc (nr_bytes))) {
    print_message ("F001",
		   "[%s]: Memory allocation failed in file `%s' at line %ld.",
		   r, fn, ln);
    exit (1);
  }
  bytes_allocated += nr_bytes;
  return p;
}

void *MA_Calloc (long nr_elems, long bytes_per_elem, const char* r,
		 const char* fn, long ln)
{
  long nr_bytes = nr_elems * bytes_per_elem;
  void *p = MA_Malloc (nr_bytes, r, fn, ln);
  return memset(p, 0, nr_bytes);
}

#if COMMENT
void *MA_Calloc (long nr_elems, long bytes_per_elem, const char* r,
		 const char* fn, long ln)
{
  long nr_bytes = nr_elems * bytes_per_elem;
  void *p;

  if (!nr_bytes)
    print_message ("W002",
		   "[%s]: Allocating 0 bytes in file `%s' at line %ld.",
		   r, fn, ln);

  MA_assure_alloc (nr_bytes, fn, ln);

  if (!(p = calloc (nr_elems, bytes_per_elem))) {
    print_message ("F002",
	     "[%s]: Memory allocation failed in file `%s' at line %ld.",
	     r, fn, ln);
    exit (1);
  }

  bytes_allocated += nr_bytes;
  return p;
}
#endif

void *MA_Realloc (void *p, long nr_bytes, const char* r, const char* fn,
		  long ln)
{
  if (!nr_bytes)
    print_message ("W003",
		   "[%s]: Reallocating to 0 bytes in file `%s' at line %ld.",
		   r, fn, ln);

  MA_assure_alloc (nr_bytes, fn, ln);

  if (!(p = realloc (p, nr_bytes))) {
    print_message ("F003",
	     "[%s]: Memory allocation failed in file `%s' at line %ld.",
	     r, fn, ln);
    exit (1);
  }
  return p;
}

void *MA_Realloc2 (void *p, long nr_bytes, long old, const char* r,
		   const char* fn, long ln)
{
  if (nr_bytes > old) {
    MA_assure_alloc (nr_bytes - old, fn, ln);

    if (!(p = realloc (p, nr_bytes))) {
      print_message ("F003",
	     "[%s]: Memory allocation failed in file `%s' at line %ld.",
		     r, fn, ln);
      exit (1);
    }
    bytes_allocated += (nr_bytes - old);
  }
  return p;
}

char *MA_CopyString (const char *cp, const char* r, const char* fn, long ln)
{
  return strcpy (MA_Malloc (strlen (cp)+1, r, fn, ln), cp);
}

char *MA_CopyString1 (const char *cp, long len, const char* r, const char* fn,
		      long ln)
{
  char *p = MA_Malloc (len + 1, r, fn, ln);

  if (len) /*strncpy (p, cp, len);*/
    memcpy (p, cp, len);
  p[len] = '\0';
  return p;
}

void MA_Free (void *p, long nr_bytes, const char* r, const char* fn, long ln)
{
  if (p) {
    if (!nr_bytes)
      print_message ("W001",
     "[%s]: Freeing 0 bytes for non-NULL pointer in file `%s' at line %ld.",
		     r, fn, ln);
    free (p);
    bytes_allocated -= nr_bytes;
  }
}

void print_message (const char *code, const char *format, ...)
{
  va_list ap;

  va_start (ap, format);
  if (code) fprintf (stderr, "%s ", code);
  vfprintf (stderr, format, ap);
  fprintf (stderr, "\n");
  va_end (ap);
}
