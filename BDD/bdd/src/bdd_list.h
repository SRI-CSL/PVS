/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : bdd_list.h
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1996 G.L.J.M. Janssen
 date	   : 10-APR-1996
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef BDD_LIST_H
#define BDD_LIST_H

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* Types: */
#define BDD_LIST			LIST
#define BDD_ELEM			LIST_ELEM_PTR

/* Data access macros: */
#define BDD_LIST_NULL			NULL_LIST
#define BDD_LIST_FOR_EACH_ELEM		FOR_EACH_LIST_ELEM
#define BDD_LIST_END_FOR_EACH_ELEM	END_FOR_EACH_LIST_ELEM
#define BDD_ELEM_CALLOC			CALLOC_LIST_ELEM
#define BDD_ELEM_CONTENTS(e)		((BDDPTR) ELEM_CONTENTS(e))
#define BDD_ELEM_CONTENTS_I(e)		((int) ELEM_CONTENTS(e))
#define BDD_ELEM_SET_CONTENTS(e,v)	(ELEM_CONTENTS(e) = (void *) (v))
#define BDD_LIST_FIRST			LIST_FIRST
#define BDD_LIST_NEXT			LIST_NEXT
#define BDD_LIST_LAST			LIST_LAST
#define BDD_LIST_SIZE			LIST_SIZE
#define BDD_LIST_SET_SIZE(l,s)		((l)->size = (s))
#define BDD_LIST_CALLOC			CALLOC_LIST
#define BDD_LIST_FREE			FREE_LIST

/* Functions: */
#define bdd_list_print			print_list
#define bdd_list_free			free_list
#define bdd_list_copy			copy_list
#define bdd_list_bisect			bisect_list
#define bdd_list_concat			concat_lists
#define bdd_list_reverse		reverse_list
#define bdd_list_mergesort		mergeSort
#define bdd_list_append_cont		append_cont
#define bdd_list_push_cont		push_cont
#define bdd_list_pop_cont(la)		((BDDPTR) pop_cont(la))
#define bdd_list_present		in_list

#endif /* BDD_LIST_H */
