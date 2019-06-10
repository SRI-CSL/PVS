/*
 DOCUMENTATION INFORMATION				          module: LIST
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : list.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1988-1997 G.L.J.M. Janssen
 date	   :  4-DEC-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef LIST_H
#define LIST_H

#include <stdio.h>

#ifndef NULL
#define NULL			0
#endif
#define NULL_LIST		((LIST) 0)

/* Returns the contents of an element: */
#define ELEM_CONTENTS(elem) ((elem)->cont)
/* Basic list access functions: */
#define LIST_FIRST(list) ((list)->start_p)
#define LIST_LAST(list)  ((list)->end_p)
#define LIST_INFO(list)  ((list)->info)
#define LIST_NEXT(elem)  ((elem)->next)

typedef struct LIST_ELEM LIST_ELEM, *LIST_ELEM_PTR;
struct LIST_ELEM {
  void *cont;
  struct LIST_ELEM *next;
};

typedef struct LIST_REC LIST_REC, *LIST;
struct LIST_REC {
  LIST_ELEM_PTR start_p;	/* pointer to first element */
  LIST_ELEM_PTR end_p;		/* pointer to last element */
  int size;			/* number of elements */
  int info;			/* user defined field */
};

extern LIST           all_lists;
extern const LIST_REC null_list;
extern LIST           temp_list;
extern int total_lists;

extern LIST_ELEM_PTR all_list_elems;
extern const LIST_ELEM     null_list_elem;
extern LIST_ELEM_PTR temp_list_elem;
extern int total_list_elems;

/*#define CALLOC_LIST()			CALLOC_STRUCT (LIST_REC) */
#define CALLOC_LIST() \
	( \
	 all_lists ? (temp_list = all_lists, \
		      all_lists = (LIST) LIST_LAST(all_lists), \
		      *temp_list = null_list, \
		      temp_list) \
	 : (total_lists++, CALLOC_STRUCT (LIST_REC)) \
	)

/*#define FREE_LIST(l)		free (l) */
/* Setting start_p to NULL allows for testing multiple frees on same list. */
#define FREE_LIST(l) \
	do { \
	  LIST _xyz_l = l; \
          \
          LIST_FIRST(_xyz_l) = NULL; \
          LIST_LAST(_xyz_l) = (LIST_ELEM_PTR) all_lists; \
	  all_lists = _xyz_l; \
	} while (0)

/*#define CALLOC_LIST_ELEM()		CALLOC_STRUCT (LIST_ELEM) */
#define CALLOC_LIST_ELEM() \
	( \
	 all_list_elems ? (temp_list_elem = all_list_elems, \
			   all_list_elems = LIST_NEXT(all_list_elems), \
			   *temp_list_elem = null_list_elem, \
			   temp_list_elem) \
	 : (total_list_elems++, CALLOC_STRUCT (LIST_ELEM)) \
	)

/*#define MALLOC_LIST_ELEM()		MALLOC_STRUCT (LIST_ELEM) */
#define MALLOC_LIST_ELEM() \
	( \
	 all_list_elems ? (temp_list_elem = all_list_elems, \
			   all_list_elems = LIST_NEXT(all_list_elems), \
			   temp_list_elem) \
	 : (total_list_elems++, MALLOC_STRUCT (LIST_ELEM)) \
	)

/*#define FREE_LIST_ELEM(e)		free (e) */
#define FREE_LIST_ELEM(e) \
	do { \
	  LIST_ELEM_PTR _xyz_e = e; \
	  \
          LIST_NEXT(_xyz_e) = all_list_elems; \
	  all_list_elems = _xyz_e; \
	} while (0)

#define make_list()		CALLOC_LIST ()

#define make_null_list(l)	do {l = NULL_LIST;} while (0)

#define LIST_SIZE(list)		((list) ? (list)->size : 0)

#define FOR_EACH_LIST_ELEM(list, elem) \
	{ \
	  LIST xyz_list = list; \
	  register LIST_ELEM_PTR elem; \
	\
	  if (xyz_list) \
	    for (elem = LIST_FIRST(xyz_list); elem; elem = LIST_NEXT(elem)) {

#define END_FOR_EACH_LIST_ELEM	}}

/* Checks whether `c' (usually a list element's contents) is NULL. */
extern int null_contents (void *c);

/* Deallocates storage taken in by `list'. No action when NULL_LIST.
   `free_cont' when not 0 is function that is called for contents of
   each element, e.g. to deallocate that too.
*/
extern void free_list (LIST list, void (*free_cont) (void *));

/* Returns copy of `list'. Returns NULL_LIST when argument list
   is NULL_LIST. When func is not 0, then is should be a function
   that accepts an element's contents as argument and returns a copy.
*/
extern LIST copy_list (LIST list, void *(*func) (void *));

/* Returns `list' with all elements in reverse order. Does not make copy!
   Returns NULL_LIST when argument list is NULL_LIST.
*/
extern LIST reverse_list (LIST list);

/* Creates an element with contents `p', and appends that element
   onto the end of list `list' and returns the thus modified list.
*/ 
extern LIST append_cont (void *p, LIST list);

/* Creates an element with contents `p', and pushes that element
   onto the list `list' and returns the thus modified list.
*/ 
extern LIST push_cont (void *p, LIST list);

/* Returns the contents of the element popped from the list, i.e.
   removed at the front.
   `listp' must be the address of a LIST typed variable.
*/
extern void *pop_cont (LIST *listp);

/* Removes all the elements whose contents satisfy the function
   `predicate', i.e. when predicate (cont) returns a non-zero value
   that element is deleted from the `list'. For such removed elements
   the function `free_cont' when non-zero is called, usually to free
   the contents space.
   If `predicate' is 0, it defaults to the TRUE function.
   Returns the modified list, when no elements left this is the NULL_LIST.
   `deletions' must be the address of an int variable or 0.
   When an address of an int is passed, it will be used to return the
   number of deletions that took place.
*/
extern LIST remove_elements (LIST list,
			     int  (*predicate) (void *),
			     void (*free_cont) (void *),
			     int *deletions);

/* Traverses `list' from start to end and calls `test (item, elem->cont)'
   for each elem. When the call to `test' returns a value != 0 (true)
   then `in_list' returns this elem, otherwise it returns NULL.
   If `test' is 0, the test defaults to a `==' comparison.
*/
extern LIST_ELEM_PTR in_list (void *item, LIST list,
			      int (*test) (void *, void *));

/* Calls the function `func' for the contents of each element in `list'.
*/
extern void for_each_cont_do (LIST list, void (*func) (void *));

/* Destructively appends `list2' onto the end of `list1' and
   returns that new list (`list1').
   Afterwards `list2' should be considered undefined.
   Allows either or both lists to be the NULL_LIST.
*/
extern LIST concat_lists (LIST list1, LIST list2);

/* LIST mergeSort (list, comparison) 

   Sorts the `list' according the comparison function `comparison'. An
   element a is taken to be less or equal to an element b when the call
   comparison(a, b) returns a non-positive value. If `comparison' is 0, it
   defaults to a `<=' comparison. 
*/
extern LIST mergeSort (LIST list, int (*comparison) (void *, void *));

/* Bisects the list `list'. Destructively modifies its argument to achieve
   this. List returned is second halve of original list, in case of an
   odd number of elements, the returned list is 1 element smaller than
   the the first halve.
   Returns NULL_LIST for an empty list and a singleton list argument.
*/     
extern LIST bisect_list (LIST list);

/* Prints the `list', writing to the file indicated by the file pointer `fp'.
   For the contents of each element `func' is called.
   In effect, prints `list_start' then prints the list elements separated
   by `separator' and terminates by printing `list_end'.
   The print function is called as `func (fp, item)' for each element's
   contents.
*/
extern void print_list (FILE *fp,
			char *list_start,
			LIST list,
			void (*func) (FILE *, void *),
			char *separator,
			char* list_end);

#endif /* LIST_H */
