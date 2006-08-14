
int
fileutils___file_exists_p(char *filename)
{
  return(file_exists_p(filename));
}

int
fileutils___directory_p(char *filename)
{
  return(directory_p(filename));
}

int
fileutils___read_permission_p(char *filename)
{
  return(read_permission_p(filename));
}  

int
fileutils___write_permission_p(char *filename)
{
  return(write_permission_p(filename));
}

long int
fileutils___file_write_time(char *filename)
{
  return(file_write_time(filename));
}

int
fileutils___getfileinfo(char *filename, int *i)
{
  return(getfileinfo(filename, i));
}
