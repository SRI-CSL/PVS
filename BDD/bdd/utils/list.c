/*
 DOCUMENTATION INFORMATION				          module: LIST
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : list.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1988-1994 G.L.J.M. Janssen
 date	   : 21-OCT-1994
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>

#include "alloc.h"
#include "list.h"

/* Globals for list and list element allocation: */
LIST           all_lists = NULL;
const LIST_REC null_list = {0};
LIST           temp_list;
int total_lists = 0;

LIST_ELEM_PTR       all_list_elems = NULL;
const LIST_ELEM     null_list_elem = {0};
LIST_ELEM_PTR       temp_list_elem;
int total_list_elems = 0;

#ifdef COMMENT
static LIST_ELEM_PTR make_elem(void *p)
{
  register LIST_ELEM_PTR elem;

  elem = CALLOC_LIST_ELEM();
  elem->cont = p;

  return elem;
}
#endif

#ifdef COMMENT
static LIST make_list(void)
{
  LIST list;

  list = CALLOC_LIST();
  /* list->size == 0 */

  return list;
}
#endif

int null_contents(void *c)
{
  return !c;
}

void free_list(LIST list, void (*free_cont)(void *))
{
  register LIST_ELEM_PTR p;
  register LIST_ELEM_PTR save;

  if (!list)
    /* No action for empty list. */
    return;

  if (!(p = LIST_FIRST(list))) {
    /* start_p should never be NULL for proper LIST;
       it is NULL when the list header is part of all_lists.
    */
    print_message("ELST001", "Attempt at freeing already freed list.");
    return;
  }

  if (free_cont)
    while (p) {
      (*free_cont)(ELEM_CONTENTS(p));
      save = p;
      p = LIST_NEXT(p);
      FREE_LIST_ELEM(save);
    }
  else
    while (p) {
      save = p;
      p = LIST_NEXT(p);
      FREE_LIST_ELEM(save);
    }
  FREE_LIST(list);
}

LIST copy_list(LIST list, void *(*func)(void *))
{
  LIST new_list;
  register LIST_ELEM_PTR copy, p, *tail;

  if (!list) return NULL_LIST;

  new_list = make_list();
  new_list->size = list->size;
  LIST_INFO(new_list) = LIST_INFO(list);
  p = LIST_FIRST(list);

  if (!p) {
    /* start_p should never be NULL for proper LIST;
       it is NULL when the list header is part of all_lists.
    */
    print_message("ELST002", "Attempt at copying bad(already freed) list.");
    return NULL_LIST;
  }

  tail = &LIST_FIRST(new_list);

  if (func)
    do {
      copy = MALLOC_LIST_ELEM();

      ELEM_CONTENTS(copy) = (* func)(ELEM_CONTENTS(p));
      *tail = copy;
      tail = &LIST_NEXT(copy);

      p = LIST_NEXT(p);
    } while (p);
  else
    do {
      copy = MALLOC_LIST_ELEM();

      ELEM_CONTENTS(copy) = ELEM_CONTENTS(p);
      *tail = copy;
      tail = &LIST_NEXT(copy);

      p = LIST_NEXT(p);
    } while (p);

  *tail = NULL;
  LIST_LAST(new_list) = copy;

  return new_list;
}

LIST reverse_list(LIST list)
{
  register LIST_ELEM_PTR b, save, r;

  if (!list) return NULL_LIST;

  LIST_LAST(list) = r = LIST_FIRST(list);

  /* Reverse all next pointers: */
  b = NULL;
  do {
    save = r;
    r = LIST_NEXT(r);
    LIST_NEXT(save) = b;
    b = save;
  } while (r);

  LIST_FIRST(list) = b;
  return list;
}

LIST append_cont(void *p, LIST list)
{
  register LIST_ELEM_PTR new_elem;

  if (!list) list = make_list();

  new_elem = CALLOC_LIST_ELEM();
  ELEM_CONTENTS(new_elem) = p;

  if (LIST_FIRST(list))
    LIST_NEXT(LIST_LAST(list)) = new_elem;
  else
    LIST_FIRST(list) = new_elem;
  LIST_LAST(list) = new_elem;
  list->size++;

  return list;
}

LIST push_cont(void *p, LIST list)
{
  register LIST_ELEM_PTR new_elem;

  if (!list) list = make_list();

  new_elem = CALLOC_LIST_ELEM();
  ELEM_CONTENTS(new_elem) = p;

  LIST_NEXT(new_elem) = LIST_FIRST(list);
  if (!LIST_FIRST(list)) LIST_LAST(list) = new_elem;
  LIST_FIRST(list) = new_elem;
  list->size++;

  return list;
}

#ifdef COMMENT
/*
 * LIST add_cont_sorted(p, list) 
 *
 * Creates an element with contents `p', and adds that element to the sorted
 * list `list' unless p is already present in the list.
 * Keeps elements sorted according less_than which when 0 defaults to '<'.
 * Returns the(possibly) modified list.
 */
LIST add_cont_sorted(void *p, LIST list, ITEM_COMP_FUNC less_than)
{
  register LIST_ELEM_PTR *q;

  if (!list)
    return push_cont(p, list);

  for (q = &LIST_FIRST(list); *q; q = &LIST_NEXT(*q)) {
    if (less_than ? (*less_than)(p, ELEM_CONTENTS(*q))
		  : (p < ELEM_CONTENTS(*q))) {
      LIST_ELEM_PTR new_elem = CALLOC_LIST_ELEM();

      ELEM_CONTENTS(new_elem) = p;
      LIST_NEXT(new_elem) = *q;
      *q = new_elem;
      list->size++;

      return list;
    }
  }
  return append_cont(p, list);
}
#endif

void *pop_cont(LIST *listp)
{
  register void *p;
  LIST list = *listp;
  LIST_ELEM_PTR next;

  if (!list) return NULL;

  list->size--;

  p = ELEM_CONTENTS(LIST_FIRST(list));
  next = LIST_NEXT(LIST_FIRST(list));
  FREE_LIST_ELEM(LIST_FIRST(list));

  if (!(LIST_FIRST(list) = next)) {
    FREE_LIST(list);
    (*listp) = NULL_LIST;
  }

  return p;
}

LIST remove_elements(LIST list,
		      int  (*predicate)(void *),
		      void (*free_cont)(void *),
		      int *deletions)
{
  register LIST_ELEM_PTR prevp, p;

  if (deletions) *deletions = 0;

  if (!list) return NULL_LIST;

  prevp = NULL;
  p = LIST_FIRST(list);

  while (p) {
    if (!predicate || (* predicate)(ELEM_CONTENTS(p))) {
      if (deletions) (*deletions)++;
      if (free_cont) (* free_cont)(ELEM_CONTENTS(p));
      list->size--;
      if (!prevp) {
	/* Removing first(perhaps also last) element. */
	LIST_FIRST(list) = LIST_NEXT(p);
	FREE_LIST_ELEM(p);
	p = LIST_FIRST(list);
      }
      else {
	/* Bypass this(perhaps last) element: */
	/* If deleting last elem must reset end_p: */
	LIST_NEXT(prevp) = LIST_NEXT(p);
	FREE_LIST_ELEM(p);
	if (!(p = LIST_NEXT(prevp)))
	  LIST_LAST(list) = prevp;
      }
    }
    else {
      prevp = p;
      p = LIST_NEXT(p);
    }
  }

  if (!LIST_FIRST(list)) {
    FREE_LIST(list);
    list = NULL_LIST;
  }
  return list;
}

LIST_ELEM_PTR in_list(void *item, LIST list, int (*test)(void *, void *))
{
  register LIST_ELEM_PTR elem;

  if (!list) return NULL;

  if (test) {
    for (elem = LIST_FIRST(list); elem; elem = LIST_NEXT(elem))
      if ((*test)(item, ELEM_CONTENTS(elem)))
	return elem;
  }
  else
    for (elem = LIST_FIRST(list); elem; elem = LIST_NEXT(elem))
      if (item == ELEM_CONTENTS(elem))
	return elem;

  return NULL;
}

void for_each_cont_do(LIST list, void (*func)(void *))
{
  register LIST_ELEM_PTR p;

  if (!list) return;

  for (p = LIST_FIRST(list); p; p = LIST_NEXT(p))
   (* func)(ELEM_CONTENTS(p));
}

#ifdef COMMENT
/* Applies the func to every element of list and creates a
   list of the individual application results.
   List and its elements are not affected.
   If list is empty, the NULL_LIST is returned.
*/
LIST map_list(LIST list, void *(*func)(void *))
{
  LIST result;
  register LIST_ELEM_PTR p;
  register LIST_ELEM_PTR *tail;
  register LIST_ELEM_PTR last_elem;

  if (!list) return NULL_LIST;

  result = make_list();
  tail = &LIST_FIRST(result);

  for (p = LIST_FIRST(list); p; p = LIST_NEXT(p)) {
    last_elem = MALLOC_LIST_ELEM();

    ELEM_CONTENTS(last_elem) = (* func)(ELEM_CONTENTS(p));
    *tail = last_elem;
    tail = &(last_LIST_NEXT(elem));
  }
  *tail = NULL;
  LIST_LAST(result) = last_elem;
  result->size  = list->size;

  return result;
}
#endif

LIST concat_lists(LIST list1, LIST list2)
{
  if (!list1) return list2;
  if (!list2) return list1;

  /* Here both non-empty. */

  LIST_NEXT(LIST_LAST(list1)) = LIST_FIRST(list2);
  LIST_LAST(list1)            = LIST_LAST(list2);

  list1->size += list2->size;

  FREE_LIST(list2);

  return list1;
}

#ifdef COMMENT
/* LIST merge_lists(list1, list2, test_equal, free_cont) 

   Destructively merges `list2' into `list1' and returns that new list.
   Merging means that duplicate elements are not included in the final result.
   Elements, or better their contents, are equal when `test_equal' on them
   returns a non-zero value. `free_cont' when non-zero is called to free any
   duplicate contents. If `test_equal' is 0, the test defaults to `=='.
   `free_cont' when non-zero is called to free any duplicate contents.
   Afterwards `list2' should be considered undefined and not be used.
   Allows either or both lists to be the NULL_LIST.
   Worst-case performance: O(n^2), where n is sum of length of lists.
*/
LIST merge_lists(LIST list1, LIST list2,
		  int  (*test_equal)(void *, void *),
		  void (*free_cont)(void *))
{
  register LIST_ELEM_PTR p, q;
  int i, nr_elems;

  list1 = concat_lists(list1, list2);

  if (!list1)
    return NULL_LIST;

  /* here at least 1 elem in list1 */

  /* move first elem to result, save rest: */
  p = LIST_NEXT(LIST_FIRST(list1));
  LIST_LAST(list1) = LIST_FIRST(list1);
  nr_elems = 1;

 restart:
  while (p) {
    for (i = 0, q = LIST_FIRST(list1); i < nr_elems; i++, q = LIST_NEXT(q))
      if (test_equal ? (*test_equal)(ELEM_CONTENTS(p), ELEM_CONTENTS(q))
	  	     : (ELEM_CONTENTS(p) == ELEM_CONTENTS(q))) {
	if (free_cont)
	  (*free_cont)(ELEM_CONTENTS(p));
	q = p;
	p = LIST_NEXT(p);
	FREE_LIST_ELEM(q);
	goto restart;
      } /*if-for*/

    /* here: p not in q list; append it: */
    LIST_NEXT(LIST_LAST(list1)) = p;
    LIST_LAST(list1)            = p;
    nr_elems++;

    p = LIST_NEXT(p);
  } /* while */

  list1->size = nr_elems;

  LIST_NEXT(LIST_LAST(list1)) = NULL;

  return list1;
}
#endif

/* Merges two sorted(<=) lists into one sorted list(<=).
   Unless `remove_duplicates' is non-0 will keep any duplicate elements;
   otherwise, the contents of any duplicates will be freed by `free_func'
   is that is non-0.
   Does its work by destructive modification; both arguments are to
   be considered undefined afterwards.
   If comparison = 0 uses '<=' test on elements' contents.
*/
static LIST merge_sorted_lists(LIST list1, LIST list2,
				int (*comparison)(void *, void *),
				int remove_duplicates,
				void (*free_func)(void *))
{
  register LIST_ELEM_PTR p, q;
  LIST_ELEM_PTR result = NULL;
  LIST_ELEM_PTR *tail = &result;

  if (!list1)
    return list2;
  if (!list2)
    return list1;

  p = LIST_FIRST(list1);
  q = LIST_FIRST(list2);

  while (p && q) {
    int comp = comparison ? (*comparison)(ELEM_CONTENTS(p), ELEM_CONTENTS(q))
      			  : (int) ELEM_CONTENTS(p) - (int) ELEM_CONTENTS(q);

    if (!comp) {		/* ==> p = q */
      if (remove_duplicates) {
	LIST_ELEM_PTR save = q;

	if (free_func) (*free_func)(ELEM_CONTENTS(q));
	q = LIST_NEXT(q);
	FREE_LIST_ELEM(save);
	continue;
      }
      goto resume;
    }
    else
    if (comp < 0) {		/* ==> p < q */
    resume:
      *tail = p;
      p = LIST_NEXT(p);
    }
    else {			/* ==> p > q */
      *tail = q;
      q = LIST_NEXT(q);
    }
    tail = &LIST_NEXT(*tail);
  } /*while*/

  /* here: p == NULL or q == NULL or both */
  /* if both NULL, result properly reflects the correct result. */

  if (!p) {			/* perhaps also q == NULL */
    /* Pass remainder of q list onto result: */
    *tail = q;
    LIST_LAST(list1) = LIST_LAST(list2);
  }
  else				/* p != NULL ==> q == NULL */
    /* Pass remainder of p list onto result: */
    *tail = p;

  LIST_FIRST(list1) = result;
  list1->size += list2->size;

  FREE_LIST(list2);

  return list1;
}

/* Pre: LIST_SIZE(list)) >= 2 */
static LIST bisect_list_aux(LIST list)
{
  register LIST list2;
  register LIST_ELEM_PTR p;
  register int i, n, ndiv2, nminndiv2;

  n = LIST_SIZE(list);
  /* n >= 2 */

  /* Find middle of list: */
  ndiv2 = n / 2;
  nminndiv2 = n - ndiv2; /* >= ndiv2 */
  /* ndiv2 >= 1 */
  p = LIST_FIRST(list);		/* != NULL */
  for (i = 1; i < nminndiv2; i++, p = LIST_NEXT(p)); /* n-ndiv2-1 times */
  /* p != NULL */

  /* Now divide `list' in 2 non-empty lists: */
  list2 = make_list();
  LIST_FIRST(list2) = LIST_NEXT(p);	/* != NULL */
  LIST_LAST(list2) = LIST_LAST(list);
  list2->size = ndiv2;
  /* Close of the first list: */
  LIST_NEXT(p) = NULL;		/* head != NULL */
  LIST_LAST(list) = p;
  list->size = nminndiv2;
  return list2;
}

LIST bisect_list(LIST list)
{
  return LIST_SIZE(list) < 2 ? NULL_LIST : bisect_list_aux(list);
}

LIST mergeSort(LIST list, int (*comparison)(void *, void *))
{
  LIST list2;

  if (LIST_SIZE(list) < 2)
    return list;

  list2 = bisect_list_aux(list);

  return merge_sorted_lists(mergeSort(list,  comparison),
			     mergeSort(list2, comparison),
			     comparison, 0, 0);
}

void print_list(FILE *fp,
		 char *list_start,
		 LIST list,
		 void (*func)(FILE *, void *),
		 char *separator,
		 char *list_end)
{
  register LIST_ELEM_PTR p;

  fprintf(fp, "%s", list_start);

  if (list) {
    p = LIST_FIRST(list);

    (*func)(fp, ELEM_CONTENTS(p));
    p = LIST_NEXT(p);

    while (p) {
      fprintf(fp, "%s", separator);
      (*func)(fp, ELEM_CONTENTS(p));
      p = LIST_NEXT(p);
    } /*while*/
  }

  fprintf(fp, "%s", list_end);
}
