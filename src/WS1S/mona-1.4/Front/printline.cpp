/*
 * MONA
 * Copyright (C) 1997-2000 BRICS.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the  Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 * USA.
 */

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
  cout << " '" << fileName << "' line " << line << " column " << col << "\n"; 
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
  for (t = 1; t < c; t++)
    if (temp[t] == '\t')
      cout << "\t";
    else
      cout << " ";
  cout << "^";
}
