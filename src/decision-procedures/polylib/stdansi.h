/* standard ansi definitions */
/* Comment out what you don't need */

char *malloc(unsigned size);
int free(void *ptr);

/* int    printf(              char *format, ...); */
/* int   fprintf(FILE *stream, char *format, ...); */
/* char *sprintf(char *s,      char *format, ...); */
/* int     scanf(              char *format, ...); */
/* int    fscanf(FILE *stream, char *format, ...); */
/* int    sscanf(char *s,      char *format, ...); */

char *memccpy(void *s1, void *s2, int c, int n);
char *memchr (void *s,            int c, int n);
/* int   memcmp (void *s1, void *s2, int n); */
/* char *memcpy (void *s1, void *s2, int n); */
char *memset (void *s,            int c, int n);

long clock ( );
