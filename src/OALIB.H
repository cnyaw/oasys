/*
  C++ library header file by Russell Wallace
  changes by Jeff Jenness 12-21-92
*/

#ifndef __cplusplus
#error Must compile this source using C++.
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))

#ifdef DEBUG
#define pdebug(v,f)     printf (#v " = %" #f " in " __FILE__ " %d\n",v,__LINE__)
#else
#define pdebug(v,f)
#endif

unsigned char hexchar2byte (int);
unsigned char hexchars2byte (int, int);

enum {
  FALSE,
  TRUE
};

struct slist {
  slist *next;

  void operator += (slist * n) {
    n->next = next;
    next = n;
  };
  int len (void);
  void free (void);
};

struct slisthead {
  slist *next, *last;

  slisthead (void) {
    next = 0;
    last = (slist *) this;
  };
  void operator += (slist *);
  void free (void) {
    ((slist *) this)->free ();
  };

  int len (void) {
    return next->len ();
  };
};

struct dlist {
  dlist *next, *prev;

  dlist (void) {
    next = prev = this;
  };
  void operator += (dlist *);
  void concat (dlist *);
  void remove (void);
  void free (void);
  void init (void) {
    next = prev = this;
  };

  int len (void);
};

struct treenode {
  treenode *left, *right;
  void *data;

  treenode (void *);
  ~treenode (void);
  int size (void);
};

struct tree {
  int (_cdecl * fcmp) (const char *, const char *);
  treenode *t;

  void *operator += (void *);
  void *operator[](void *);
  void free (void) {
    if (t) {
      delete t;
      t = 0;
    }
  };

  int size (void) {
    return t->size ();
  };
};

struct ftreenode {
  ftreenode *left, *right;
  void *data;

  ftreenode (void *);
  ~ftreenode (void);
  int size (void) {
    return ((treenode *) this)->size ();
  };
};

struct ftree {
  int (_cdecl * fcmp) (const char *, const char *);
  ftreenode *t;

  void *operator += (void *);
  void *operator[](void *data) {
    return (*((tree *) this))[data];
  };
  ~ftree (void) {
    if (t) {
      delete t;
      t = 0;
    }
  };
  int size (void) {
    return t->size ();
  };
};

char *defext (char *, char *, int = FALSE);
void perr (char *...);

int Create (char *);
FILE *Fopen (char *, char *);
int Open (char *);
void Read (int, void *, unsigned);
void Rename (char *, char *);
void Write (int, void *, unsigned);

char *strdup (char *);
int strpcmp (const void *, const void *);
inline char *strcap (char *s)
{
  *s = toupper (*s);
  return s;
}
inline char *strchop (char *s, int n)
{
  s[strlen (s) - n] = 0;
  return s;
}
