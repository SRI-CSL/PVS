//
// deque.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __DEQUE_H
#define __DEQUE_H

#include <stdlib.h>
#include <assert.h>

template<class T>
class Deque {
  unsigned allocated, start, noelems;
  T *buffer;

public:

  typedef T *iterator;

  Deque()
  {
    allocated = 0;
    start = 0;
    noelems = 0;
    buffer = 0;
  }

  Deque(const T &elem)
  {
    buffer = new T[1];
    buffer[0] = elem;
    allocated = 1;
    start = 0;
    noelems = 1;
  }

  ~Deque()
  {
    delete[] buffer;
  }

  void push_back(const T &elem)
  {
    if (start + noelems >= allocated) {
      allocated = allocated * 3 / 2 + 2;
      T *t = new T[allocated];
      memcpy(&t[start], &buffer[start], noelems * sizeof(T));
      delete[] buffer;
      buffer = t;
    }

    buffer[start + noelems] = elem;
    noelems++;
  }

  T &pop_back()
  {
    assert(noelems > 0);

    noelems--;
    return buffer[start + noelems];
  }

  void push_front(const T &elem)
  {
    if (start == 0) {
      unsigned n =  allocated * 3 / 2 + 2;
      T *t = new T[n];
      memcpy(&t[start + n - allocated], &buffer[start], noelems * sizeof(T));
      delete[] buffer;
      buffer = t;
      start += n - allocated;
      allocated = n;
    }

    buffer[--start] = elem;
    noelems++;
  }

  T &pop_front()
  {
    assert(noelems > 0);
    
    noelems--;
    return buffer[start++];
  }

  void set(unsigned pos, const T &elem)
  {
    assert(pos < noelems);
    
    buffer[start + pos] = elem;
  }

  void set(iterator i, const T &elem)
  {
    assert(i >= &buffer[start] && i < &buffer[start+noelems]);
    
    *i = elem;
  }

  T &get(unsigned pos) const
  {
    assert(pos < noelems);

    return buffer[start + pos];
  }

  T &top() const
  {
    return get(noelems-1);
  }


  T *begin() const
  {
    return &buffer[start];
  }

  T *end() const
  {
    return &buffer[start + noelems];
  }

  unsigned size() const
  {
    return noelems;
  }

  bool empty() const
  {
    return noelems == 0;
  }

  void reset()
  {
    delete[] buffer;
    allocated = 0;
    start = 0;
    noelems = 0;
    buffer = 0;
  }

  void append(Deque<T> *d)
  {
    iterator i;
    if (d)
      for (i = d->begin(); i != d->end(); i++) 
	push_back(*i);
  }

  Deque<T> *copy() const
  {
    Deque<T> *d = new Deque<T>;
    iterator i;

    for (i = begin(); i != end(); i++)
      d->push_back(*i);

    return d;
  }

  void sort(int (*compar) (const void *, const void *))
  {
    qsort(begin(), noelems, sizeof(T), compar);
  }

  const T *search(const T &elem, int (*compar) (const void *, const void *))
  {
    unsigned a = 0, b = size(), c;
    while (a < b) {
      c = (b-a)/2 + a;
      int r = compar(&elem, &buffer[start + c]);
      if (r == 0)
	return &buffer[start + c];
      if (r > 0)
	a = c+1;
      else
	b = c;
    }
    return 0;
  }
};

template<class T>
class DequeGC: public Deque<T> { // garbage-collected deque
public:
  ~DequeGC()
  {
    iterator i;
    for (i = begin(); i != end(); i++)
      delete *i;
  }
};

template<class T>
class DequeGCA: public Deque<T> { // garbage-collected deque for arrays
public:
  ~DequeGCA()
  {
    iterator i;
    for (i = begin(); i != end(); i++)
      delete[] *i;
  }
};

#endif
