
struct entry_s {
  void* pointer;
  int counter;
  struct entry_s *next;
};
typedef struct entry_s entry_t;

struct hashtable_s {
  int size;
  entry_t** table;	
}; 
typedef struct hashtable_s hashtable_t;

hashtable_t *ht_create( int size );
 
int ht_hash( hashtable_t *hashtable, void* pointer );

entry_t *ht_newpair( void* pointer );

hashtable_t *GC_hashtable;

void     GC_start();
void     GC_quit();
entry_t* GC_get_entry( void* pointer );
void     GC_add_entry( entry_t* e);
void     GC_new( void* pointer );
void*    GC( void* pointer );
int      GC_count( void* pointer );
void*    GC_malloc( int length, int size );
int      GC_free(void* pointer);


 
