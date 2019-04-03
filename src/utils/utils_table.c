/*
 * BD: adding function declarations to avoid compiler warnings
 * (These functions are implemented in file_utils.c)
 */

extern int file_exists_p(const char *filename);
extern int directory_p(const char *filename);
extern int read_permission_p(const char *filename);
extern int write_permission_p(const char *filename);
extern long int file_write_time(const char *filename);
extern int getfileinfo(const char *filename, int *i);

int
fileutils___file_exists_p(const char *filename)
{
  return(file_exists_p(filename));
}

int
fileutils___directory_p(const char *filename)
{
  return(directory_p(filename));
}

int
fileutils___read_permission_p(const char *filename)
{
  return(read_permission_p(filename));
}  

int
fileutils___write_permission_p(const char *filename)
{
  return(write_permission_p(filename));
}

long int
fileutils___file_write_time(const char *filename)
{
  return(file_write_time(filename));
}

int
fileutils___getfileinfo(const char *filename, int *i)
{
  return(getfileinfo(filename, i));
}
