/* Memory block management routines */


#include "memint.h"


int memlimit = 0;

#if defined(__STDC__)
extern void exit(int);
#else
extern void exit();
#endif


/* Amount of memory allocated */

static SIZE_T block_allocation;


/* mem_copy(dest, src, size) copies a block of memory. */

void
#if defined(__STDC__)
mem_copy(pointer dest, pointer src, SIZE_T size)
#else
mem_copy(dest, src, size)
     pointer dest;
     pointer src;
     SIZE_T size;
#endif
{
  MEM_COPY(dest, src, size);
}


/* mem_zero(ptr, size) zeros a block of memory. */

void
#if defined(__STDC__)
mem_zero(pointer ptr, SIZE_T size)
#else
mem_zero(ptr, size)
     pointer ptr;
     SIZE_T size;
#endif
{
  MEM_ZERO(ptr, size);
}


/* mem_fatal(message) prints an error message and exits. */

void
#if defined(__STDC__)
mem_fatal(char *message)
#else
mem_fatal(message)
     char *message;
#endif
{
  if (memlimit)
    fprintf(stderr, "\n\n-----\n"
	    "Interactive Demo memory limit exceeded, execution stopped.\n");
  else
    fprintf(stderr, "\nMemory management library: error: %s\n", message);
  exit(1);
}


SIZE_T
#if defined(__STDC__)
mem_allocation(void)
#else
mem_allocation()
#endif
{
  /* This will always returns zero when we're using malloc and free, */
  /* but you can maybe change it depending on your system. */
  return (block_allocation);
}


char *
#if defined(__STDC__)
mem_version(void)
#else
mem_version()
#endif
{
  return ("1.0");
}


/* This code used if we're going to do our own memory management. */

#if !defined(USE_MALLOC_FREE)
/* Free lists of various sizes */

static block avail[MAX_SIZE_INDEX+1];


/* Bogus segment for initialization */

static struct segment_ dummy_seg={(pointer)0, (SIZE_T)0};


/* Current segment */

static segment curr_seg= &dummy_seg;


static
int
#if defined(__STDC__)
ceiling_log_2(SIZE_T i)
#else
ceiling_log_2(i)
     SIZE_T i;
#endif
{
  SIZE_T j;
  int result;

  for (result=0, j=1; j < i; ++result, j*=2);
  return (result);
}


/* block_size_index(size) return the coded size for a block. */

static
int
#if defined(__STDC__)
block_size_index(SIZE_T size)
#else
block_size_index(size)
     SIZE_T size;
#endif
{
  if (size < 1)
    return (-1);
  if (size > MAX_SIZE)
    mem_fatal("block_size_index: block size too large");
  else
    size+=HEADER_SIZE;
  return (ceiling_log_2(size));
}


/* add_to_free_list(b) adds b to the appropriate free list. */

static
void
#if defined(__STDC__)
add_to_free_list(block b)
#else
add_to_free_list(b)
     block b;
#endif
{
  int i;

  i=b->size_index;
  if (!avail[i])
    {
      b->next=b;
      b->prev=b;
      avail[i]=b;
    }
  else
    {
      b->next=avail[i]->next;
      avail[i]->next->prev=b;
      avail[i]->next=b;
      b->prev=avail[i];
    }
  b->used=0;
}


/* remove_from_free_list(b) removes b from the free list which it */
/* is on. */

static
block
#if defined(__STDC__)
remove_from_free_list(block b)
#else
remove_from_free_list(b)
     block b;
#endif
{
  int i;

  i=b->size_index;
  if (b->next == b)
    avail[i]=0;
  else
    {
      b->next->prev=b->prev;
      b->prev->next=b->next;
      if (avail[i] == b)
	avail[i]=b->next;
    }
  b->used=1;
  return (b);
}

 
/* buddy(b) returns the buddy block of b, or null if there is no */
/* buddy. */

static
block
#if defined(__STDC__)
buddy(block b)
#else
buddy(b)
     block b;
#endif
{
  SIZE_T buddy_offset;

  buddy_offset=(SIZE_T)(((INT_PTR)b-(INT_PTR)b->seg->base_address) ^ ((SIZE_T)1 << b->size_index));
  if (buddy_offset < b->seg->limit)
    return ((block)((INT_PTR)b->seg->base_address+buddy_offset));
  else
    return ((block)0);
}


/* trim_to_size(b, size_index) repeatedly splits b until it has */
/* the indicated size.  Blocks which are split off are added to the */
/* appropriate free list. */

static
void
#if defined(__STDC__)
trim_to_size(block b, int size_index)
#else
trim_to_size(b, size_index)
     block b;
     int size_index;
#endif
{
  block bb;

  while (b->size_index > size_index)
    {
      b->size_index--;
      bb=buddy(b);
      bb->size_index=b->size_index;
      bb->seg=b->seg;
      add_to_free_list(bb);
    }
}


/* merge_and_free(b) repeatedly merges b its buddy until b has no */
/* buddy or the buddy isn't free, then adds the result to the */
/* appropriate free list. */

static
void
#if defined(__STDC__)
merge_and_free(block b)
#else
merge_and_free(b)
     block b;
#endif
{
  block bb;

  for (bb=buddy(b); bb && !bb->used && bb->size_index == b->size_index; bb=buddy(b))
    {
      remove_from_free_list(bb);
      if ((INT_PTR)bb < (INT_PTR)b)
	b=bb;
      b->size_index++;
    }
  add_to_free_list(b);
}


/* mem_get_block(size) allocates a new block of the specified size. */

pointer
#if defined(__STDC__)
mem_get_block(SIZE_T size)
#else
mem_get_block(size)
     SIZE_T size;
#endif
{
  int i;
  int size_index;
  int alloc_size_index;
  int new_seg;
  SIZE_T alloc_size;
  pointer sbrk_ret;
  block b;

  if ((size_index=block_size_index(size)) < 0)
    return ((pointer)0);
  /* Find smallest free block which is large enough. */
  for (i=size_index; i <= MAX_SIZE_INDEX && !avail[i]; ++i);
  if (i > MAX_SIZE_INDEX)
    {
      /* We must get more storage; don't allocate less than */
      /* 2^MIN_ALLOC_SIZE_INDEX. */
      if (size_index < MIN_ALLOC_SIZE_INDEX)
	alloc_size_index=MIN_ALLOC_SIZE_INDEX;
      else
	alloc_size_index=size_index;
      alloc_size=((SIZE_T)1 << alloc_size_index);
      /* Pad current segment to be a multiple of 2^alloc_size_index in */
      /* length. */
      alloc_size+=((curr_seg->limit+alloc_size-1) & ~(alloc_size-1))-curr_seg->limit;
      if ((sbrk_ret=(pointer)SBRK(0)) != (pointer)((INT_PTR)curr_seg->base_address+curr_seg->limit) ||
	  alloc_size+curr_seg->limit > MAX_SEG_SIZE)
	{
	  if (sbrk_ret == (pointer)-1)
	    mem_fatal("mem_get_block: allocation failed");
	  /* Segment is too large or someone else has moved the break. */
	  /* Pad to get to appropriate boundary. */
	  alloc_size=ROUNDUP((INT_PTR)sbrk_ret)-(INT_PTR)sbrk_ret;
	  /* Pad allocation request with storage for new segment */
	  /* information and indicate that a new segment must be */
	  /* created. */
	  alloc_size+=((SIZE_T)1 << alloc_size_index)+ROUNDUP(sizeof(struct segment_));
	  new_seg=1;
	}
      else {
	if (sbrk_ret == (pointer)-1)
	  mem_fatal("mem_get_block: allocation failed");
	new_seg=0;
      }
      sbrk_ret=(pointer)SBRK(alloc_size);
      if (sbrk_ret == (pointer)-1)
	mem_fatal("mem_get_block: allocation failed");
      block_allocation+=alloc_size;
      if (new_seg)
	{
	  curr_seg=(segment)ROUNDUP((INT_PTR)sbrk_ret);
	  curr_seg->base_address=(pointer)((INT_PTR)curr_seg+ROUNDUP(sizeof(struct segment_)));
	  curr_seg->limit=0;
	  /* Readjust allocation size. */
	  alloc_size=(1l << alloc_size_index);
	}
      /* Carve allocated space up into blocks and add to free lists. */
      while (alloc_size)
	{
	  size=alloc_size-(alloc_size & (alloc_size-1));
	  b=(block)((INT_PTR)curr_seg->base_address+curr_seg->limit);
	  b->size_index=ceiling_log_2(size);
	  b->seg=curr_seg;
	  add_to_free_list(b);
	  curr_seg->limit+=size;
	  alloc_size-=size;
	}
      /* Find free block of appropriate size. */
      for (i=size_index; i <= MAX_SIZE_INDEX && !avail[i]; ++i);
    }
  b=remove_from_free_list(avail[i]);
  trim_to_size(b, size_index);
  return ((pointer)((INT_PTR)b+HEADER_SIZE));
}


/* mem_free_block(p) frees the block indicated by p. */

void
#if defined(__STDC__)
mem_free_block(pointer p)
#else
mem_free_block(p)
     pointer p;
#endif
{
  block b;

  if (!p)
    return;
  b=(block)((INT_PTR)p-HEADER_SIZE);
  if (!b->used)
    mem_fatal("mem_free_block: block not in use");
  if (b->size_index < 0 || b->size_index > MAX_SIZE_INDEX)
    mem_fatal("mem_free_block: invalid block header");
  merge_and_free(b);
}


/* mem_resize_block(p, new_size) expands or contracts the block */
/* indicated by p to a new size.  We try to avoid moving the block if */
/* possible. */

pointer
#if defined(__STDC__)
mem_resize_block(pointer p, SIZE_T new_size)
#else
mem_resize_block(p, new_size)
     pointer p;
     SIZE_T new_size;
#endif
{
  int new_size_index;
  block b;
  block bb;
  pointer q;
  SIZE_T old_size;

  if (!p)
    return (mem_get_block(new_size));
  b=(block)((INT_PTR)p-HEADER_SIZE);
  if (!b->used)
    mem_fatal("mem_resize_block: block not in use");
  if (b->size_index < 0 || b->size_index > MAX_SIZE_INDEX)
    mem_fatal("mem_resize_block: invalid block header");
  if ((new_size_index=block_size_index(new_size)) < 0)
    {
      mem_free_block(p);
      return ((pointer)0);
    }
  if (b->size_index >= new_size_index)
    {
      /* Shrink block. */
      trim_to_size(b, new_size_index);
      return (p);
    }
  old_size=(1l << b->size_index)-HEADER_SIZE;
  /* Try to expand by adding buddies at higher addresses. */
  for (bb=buddy(b);
       bb && (INT_PTR)b < (INT_PTR)bb && !bb->used && bb->size_index == b->size_index;
       bb=buddy(b))
    {
      remove_from_free_list(bb);
      if (++(b->size_index) == new_size_index)
	return (p);
    }
  /* Couldn't expand all the way to needed size; allocate a new block */
  /* and move the contents of the old one. */
  q=mem_get_block(new_size);
  mem_copy(q, p, old_size);
  merge_and_free(b);
  return (q);
}
#endif


/* This code used if we're using malloc and free. */

#if defined(USE_MALLOC_FREE)
pointer
#if defined(__STDC__)
mem_get_block(SIZE_T size)
#else
mem_get_block(size)
     SIZE_T size;
#endif
{
  pointer result;

  if (size <= 0)
    return ((pointer)0);
  result=MALLOC(size);
  if (!result)
    mem_fatal("mem_get_block: allocation failed");
  return (result);
}


void
#if defined(__STDC__)
mem_free_block(pointer p)
#else
mem_free_block(p)
     pointer p;
#endif
{
  if (!p)
    return;
  FREE(p);
}


pointer
#if defined(__STDC__)
mem_resize_block(pointer p, SIZE_T new_size)
#else
mem_resize_block(p, new_size)
     pointer p;
     SIZE_T new_size;
#endif
{
  pointer t;

  if (!p)
    return (mem_get_block(new_size));
  if (new_size <= 0)
    {
      mem_free_block(p);
      return ((pointer)0);
    }
  if ((t = REALLOC(p, new_size)) == 0)
    mem_fatal("reallocation failed");

  return t;
}
#endif
