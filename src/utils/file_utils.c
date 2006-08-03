
/* --------------------------------------------------------------------
   PVS
   Copyright (C) 2006, SRI International.  All Rights Reserved.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
   -------------------------------------------------------------------- */

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
