//
// printline.cpp  
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include <string.h>
#include <iostream.h>

#include "printline.h"

Pos dummyPos;

extern Deque<FileSource *> source;
extern Deque<char *> fileNames;

void 
Pos::printsource()
{
  char *str;
  int t, c;
  char temp[77];

  if (line == -1)
    return;

  for (t = 0; t < (int) fileNames.size(); t++)
    if (fileName == fileNames.get(t))
      break;
  cout << " '" << fileName << "' line " << line << "\n"; 
  str = source.get(t)->get(line-1);

  c = col;
  if (strlen(str) >= 73) {
    if (col > 73) {
      for (t = 0; t < 77; t++) {
	if (str[col - 40 + t] != '\0')
	  temp[t] = str[col - 40 + t];
	else
	  break;
      }
      temp[t] = '\0';
      c = 40;
    }
    else {
      strncpy(temp, str, 77);
      temp[76] = '\0';
    }
  }
  else
    strcpy(temp, str);
  cout << "  " << temp << "\n  ";
  for (t = 0; t < c; t++)
    if (temp[t] == '\t')
      cout << "\t";
    else
      cout << " ";
  cout << "^";
}
