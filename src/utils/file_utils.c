#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

static struct stat finfo;

int
file_exists_p(char *filename)
{
  return(access(filename, F_OK));
}

int
directory_p(char *filename)
{
  if (stat(filename,&finfo) == -1) return(0);
  return(S_ISDIR(finfo.st_mode));
}

int
read_permission_p(char *filename)
{
  if (access(filename, R_OK) == 0) return(0);
  return(errno);
}  

int
write_permission_p(char *filename)
{
  if (access(filename, W_OK) == 0) return(0);
  return(errno);
}

long int
file_write_time(char *filename)
{
  if (!(stat(filename,&finfo))) {
    return((long) finfo.st_mtime);
  }
  return((long) 0);
}

int
getfileinfo(char *filename, int *i)
{
  if (!(stat(filename,&finfo))) {
    i[0] = (int) finfo.st_dev;
    i[1] = (int) finfo.st_ino;
    return 0;
  }
  return 1;
}
