/* Record manager routines */


#include "memint.h"


#define ALLOC_SIZE NICE_BLOCK_SIZE


#define DEBUG_MEM
#define MAGIC_COOKIE 0x34f21ab3l
#define MAGIC_COOKIE1 0x432fa13bl


struct list_
{
  struct list_ *next;
};

typedef struct list_ *list;


struct rec_mgr_
{
  int size;
  int recs_per_block;
  list free;
  list blocks;
};


/* mem_new_rec(mgr) allocates a record from the specified record */
/* manager. */

pointer
#if defined(__STDC__)
mem_new_rec(rec_mgr mgr)
#else
mem_new_rec(mgr)
     rec_mgr mgr;
#endif
{
  int i;
  pointer p;
  list new;

  if (!mgr->free)
    {
      /* Allocate a new block. */
      new=(list)mem_get_block(ALLOC_SIZE);
      new->next=mgr->blocks;
      mgr->blocks=new;
      mgr->free=(list)((INT_PTR)new+ROUNDUP(sizeof(struct list_)));
      p=(pointer)(mgr->free);
      /* Carve the block into pieces. */
      for (i=1; i < mgr->recs_per_block; ++i)
	{
	  ((list)p)->next=(list)((INT_PTR)p+mgr->size);
#if defined(DEBUG_MEM)
	  if (mgr->size >= sizeof(long)+sizeof(struct list_))
	    *(long *)(sizeof(struct list_)+(INT_PTR)p)=MAGIC_COOKIE;
#endif
	  p=(pointer)((INT_PTR)p+mgr->size);
	}
      ((list)p)->next=0;
#if defined(DEBUG_MEM)
      if (mgr->size >= sizeof(long)+sizeof(struct list_))
	*(long *)(sizeof(struct list_)+(INT_PTR)p)=MAGIC_COOKIE;
#endif
    }
  new=mgr->free;
#if defined(DEBUG_MEM)
  if (mgr->size >= sizeof(long)+sizeof(struct list_)) {
    if (*(long *)(sizeof(struct list_)+(INT_PTR)new) != MAGIC_COOKIE)
      fprintf(stderr, "record at 0x%lx may be in use\n", (INT_PTR)new);
    else
      *(long *)(sizeof(struct list_)+(INT_PTR)new)=MAGIC_COOKIE1;
  }
#endif
  mgr->free=mgr->free->next;
  return ((pointer)new);
}


/* mem_free_rec(mgr, rec) frees a record managed by the indicated */
/* record manager. */

void
#if defined(__STDC__)
mem_free_rec(rec_mgr mgr, pointer rec)
#else
mem_free_rec(mgr, rec)
     rec_mgr mgr;
     pointer rec;
#endif
{
#if defined(DEBUG_MEM)
  if (mgr->size >= sizeof(long)+sizeof(struct list_))
    if (*(long *)(sizeof(struct list_)+(INT_PTR)rec) == MAGIC_COOKIE)
      fprintf(stderr, "record at 0x%lx may already be freed\n", (INT_PTR)rec);
#endif
  ((list)rec)->next=mgr->free;
#if defined(DEBUG_MEM)
  if (mgr->size >= sizeof(long)+sizeof(struct list_))
    *(long *)(sizeof(struct list_)+(INT_PTR)rec)=MAGIC_COOKIE;
#endif
  mgr->free=(list)rec;
}


/* mem_new_rec_mgr(size) creates a new record manager with the given */
/* record size. */

rec_mgr
#if defined(__STDC__)
mem_new_rec_mgr(int size)
#else
mem_new_rec_mgr(size)
     int size;
#endif
{
  rec_mgr mgr;

  if (size < sizeof(struct list_))
    size=sizeof(struct list_);
  size=ROUNDUP(size);
  if (size > ALLOC_SIZE-ROUNDUP(sizeof(struct list_)))
    mem_fatal("mem_new_rec_mgr: record size too large");
  mgr=(rec_mgr)mem_get_block((SIZE_T)sizeof(struct rec_mgr_));
  mgr->size=size;
  mgr->recs_per_block=(ALLOC_SIZE-ROUNDUP(sizeof(struct list_)))/size;
  mgr->free=0;
  mgr->blocks=0;
  return (mgr);
}


/* mem_free_rec_mgr(mgr) frees all the storage associated with the */
/* specified record manager. */

void
#if defined(__STDC__)
mem_free_rec_mgr(rec_mgr mgr)
#else
mem_free_rec_mgr(mgr)
     rec_mgr mgr;
#endif
{
  list p, q;

  for (p=mgr->blocks; p; p=q)
    {
      q=p->next;
      mem_free_block((pointer)p);
    }
  mem_free_block((pointer)mgr);
}
