/*
  C++ routines for OASYS by Russell Wallace
  changes by Jeff Jenness 12-21-92
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <io.h>
#include <fcntl.h>
#include <string.h>
#include "oalib.h"

void perr (char *format...)
{
  va_list argptr;

  va_start (argptr, format);
  vfprintf (stderr, format, argptr);
  va_end (argptr);
  fputc ('\n', stderr);
  exit (1);
}

FILE *Fopen (char *filename, char *mode)
{
  FILE *result = fopen (filename, mode);

  if (result == 0)
    perr ("ERROR - Can't open file %s in mode %s", filename, mode);
  return result;
}

int Create (char *filename)
{
  int file;

  file = open (filename, O_CREAT | O_TRUNC | O_RDWR | O_BINARY, 0777);
  if (file < 0)
    perr ("ERROR - Can't create file %s", filename);
  return file;
}

void Write (int file, void *data, unsigned bytes)
{
  if (write (file, data, bytes) != bytes)
    perr ("ERROR - Can't write %u bytes to file %d", bytes, file);
}

int Open (char *filename)
{
  int file;

  file = open (filename, O_RDWR | O_BINARY);
  if (file < 0)
    perr ("ERROR - Can't open file %s", filename);
  return file;
}

void Read (int file, void *data, unsigned bytes)
{
  if (read (file, data, bytes) != bytes)
    perr ("ERROR - Can't read %u bytes from file %d", bytes, file);
}

char *defext (char *filename, char *ext, int force)
{
  int i = strlen (filename);

  while (i && filename[i] != '.' && filename[i] != '/' && filename[i] != '\\')
    i--;
  if (filename[i] == '.') {
    if (!force)
      return filename;
  }
  else
    i = strlen (filename);
  static char result[64];

  memcpy (result, filename, i);
  result[i] = 0;
  strcat (result, ext);
  return result;
}

unsigned char hexchar2byte (int hdigit)
{
  if (hdigit >= 'a' && hdigit <= 'z')
    return hdigit - 'a' + 10;
  if (hdigit >= 'A' && hdigit <= 'Z')
    return hdigit - 'A' + 10;
  return hdigit - '0';
}

unsigned char hexchars2byte (int hiNybble, int loNybble)
{
  return (hexchar2byte (hiNybble) << 4) | hexchar2byte (loNybble);
}

char *strdup (char *s)
{
  int i;

  i = strlen (s) + 1;
  return (char *) memcpy (new char[i], s, i);
}

int strpcmp (const void *s1, const void *s2)
{
  return strcmp (*((char **) s1), *((char **) s2));
}

void *tree::operator += (void *data)
{
  if (t == 0) {
    t = new treenode (data);
    return data;
  }
  treenode *n = t;
  int cmp;

  for (;;) {
    cmp = fcmp ((const char *) (n->data), (const char *) data);
    if (cmp == 0) {
      delete data;

      return n->data;
    }
    if (cmp < 0) {
      if (n->right == 0) {
        n->right = new treenode (data);
        return data;
      }
      n = n->right;
    } else {
      if (n->left == 0) {
        n->left = new treenode (data);
        return data;
      }
      n = n->left;
    }
  }
}

void *tree::operator [] (void *data)
{
  treenode *n = t;
  int cmp;

  while (n) {
    cmp = fcmp ((const char *) (n->data), (const char *) data);
    if (cmp == 0)
      return n->data;
    if (cmp < 0)
      n = n->right;
    else
      n = n->left;
  }
  return 0;
}

void slisthead::operator += (slist * n)
{
  last->next = n;
  last = n;
  n->next = 0;
}

treenode::treenode (void *_data)
{
  left = right = 0;
  data = _data;
}

treenode::~treenode ()
{
  if (left)
    delete left;

  if (right)
    delete right;
  delete data;
}

ftreenode::ftreenode (void *_data)
{
  left = right = 0;
  data = _data;
}

ftreenode::~ftreenode (void)
{
  if (left)
    delete left;

  if (right)
    delete right;
}

void *ftree::operator += (void *data)
{
  if (t == 0) {
    t = new ftreenode (data);
    return data;
  }
  ftreenode *n = t;
  int cmp;

  for (;;) {
    cmp = fcmp ((const char *) (n->data), (const char *) data);
    if (cmp == 0)
      return n->data;
    if (cmp < 0) {
      if (n->right == 0) {
        n->right = new ftreenode (data);
        return data;
      }
      n = n->right;
    }
    else {
      if (n->left == 0) {
        n->left = new ftreenode (data);
        return data;
      }
      n = n->left;
    }
  }
}

void slist::free (void)
{
  slist *p, *nextp;

  p = next;
  while (p) {
    nextp = p->next;
    delete p;

    p = nextp;
  }
}

int slist::len (void)
{
  int result = 0;
  slist *ptr = this;

  while (ptr) {
    result++;
    ptr = ptr->next;
  }
  return result;
}

void dlist::free (void)
{
  dlist *p, *nextp;

  p = next;
  while (p != this) {
    nextp = p->next;
    delete p;

    p = nextp;
  }
  next = prev = this;
}

void dlist::remove (void)
{
  prev->next = next;
  next->prev = prev;
  delete this;
}

int dlist::len (void)
{
  int result = 0;
  dlist *p = next;

  while (p != this) {
    result++;
    p = p->next;
  }
  return result;
}

void dlist::operator += (dlist * n)
{
  n->next = this;
  n->prev = prev;
  prev->next = n;
  prev = n;
}

void dlist::concat (dlist * h)
{
  dlist *p, *n;

  p = h->next;
  while (p != h) {
    n = p->next;
    *this += p;
    p = n;
  }
}
