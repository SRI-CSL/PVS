/////////////////////////////////////////////////////////////////////////////
//                PVS to C translator
//
//     Author: Gaspard ferey
//
//  -> https://github.com/Gaspi/pvs2c.git
//
/////////////////////////////////////////////////////////////////////////////

// Hashtable code from :
// https://gist.github.com/tonious/1377667


#define _XOPEN_SOURCE 500 // Enable certain library functions (strdup) on linux.
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include "GC.h"


/* Create a new hashtable. */
hashtable_t *ht_create( int size ) {
  int i;
  if( size < 1 ) return NULL;
  
  hashtable_t *res = NULL;
  
  /* Allocate the table itself. */
  if( ( res = malloc( sizeof( hashtable_t ) ) ) == NULL )
    return NULL;
  
  /* Allocate pointers to the head nodes. */
  if( ( res->table = malloc( sizeof( entry_t * ) * size ) ) == NULL )
    return NULL;
  
  res->size = size;
  for( i = 0; i < size; i++ )
    res->table[i] = NULL;
  
  return res;	
}
 
/* Hash a string for a particular hash table. */
int ht_hash( hashtable_t *hashtable, void* pointer ) {
  unsigned long int hashval = (unsigned long int) pointer;
  return hashval % hashtable->size;
}
 
/* Create a key-value pair. */
entry_t *ht_newpair( void* pointer ) {
  entry_t *newpair;
  if( ( newpair = malloc( sizeof( entry_t ) ) ) == NULL )
    return NULL;
  
  newpair->pointer  = pointer;
  newpair->counter  = 1;     // Default value
  newpair->next = NULL;
  return newpair;
}

void GC_start() {
  GC_hashtable = ht_create( 65536 );
}

void GC_quit() {
  // Should also free the list in each cell
  free(GC_hashtable);
}

entry_t* GC_get_entry( void* pointer ) {
  int bin = ht_hash( GC_hashtable, pointer);
  entry_t* next = GC_hashtable->table[ bin ];
  while( next != NULL && next->pointer != NULL && pointer != next->pointer )
    next = next->next;
  if( next != NULL && next->pointer != NULL )
    return next;
  else
    return NULL;
}

void GC_add_entry( entry_t* e) {
  int bin = ht_hash( GC_hashtable, e->pointer );
  e->next = GC_hashtable->table[bin];
  GC_hashtable->table[bin] = e;
}

void GC_new( void* pointer ) {
  GC_add_entry( ht_newpair(pointer) );
}

void* GC( void* pointer ) {
  entry_t* entry = GC_get_entry( pointer);
  if (! entry)
    GC_new( pointer );
  else
    entry->counter++;
  return pointer;
}


int GC_count( void* pointer ) {
  entry_t* entry = GC_get_entry( pointer);
  if ( entry == NULL )
    return -1;
  else
    return entry->counter;
}


void* GC_malloc( int length, int size ) {
  void* res = malloc( length * size );
  GC_new( res );
  return res;
}

// 1 if the pointer needs to be freed, 0 otherwise
int GC_free(void* pointer) {
  int i;
  entry_t* e = GC_get_entry(pointer);
  e->counter--;
  return (e->counter == 0);
  /* if (e->counter == 0) { */
  /*   free(pointer); */
  /*   return 0; */
  /* } else */
  /*   return 1; */
}


 
