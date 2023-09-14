#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <time.h>
#include <string.h>
#include "oalib.h"
#include "ins.h"

const int MAXWORDS = 64;
const int MAXSTACK = 1024;
const int MAXBUF = 512;

union var {
  int i;
  dlist *o;
  var *v;
};

struct phrase {
  int nwords;
  int *words;
};

struct Class {
  int nphrases;
  phrase *phrases;
};

struct instruction {
  int op;
  int i;
};

struct method {
  int type;
  int nargs;
  int *argtypes;
  int *selectors;
  int nvars;
  int *vartypes;
  int nverbs;
  phrase *verbs;
  int noselect;
  int ninstructions;
  instruction *instructions;
};

char **strings;
var *vars;
int nvars;
int nproperties;
int nvocab;
char **vocab;
int comma;
int nwords;
int words[MAXWORDS];
var nouns[MAXWORDS];
char buf[MAXBUF];
char filename[MAXBUF] = "oasys.game";
Class *classes;
int nclasses;
int nmethods;
int initmethod;
method *methods;
int nnouns;
int selectaddresseemethod;
dlist objects;
var stack[MAXSTACK];
int sp;
int restart;
int *vartypes;
int *propertytypes;
var nullv;

char* LoadFileStream(char *filename)
{
  FILE *hfile;
  char *file = 0;
  hfile = fopen(filename, "rb");
  if (hfile) {
    fseek(hfile, 0, SEEK_END);
    long sz = ftell(hfile);
    fseek(hfile, 0, SEEK_SET);
    file = new char[sz];
    if (file) {
      fread(file, sz, 1, hfile);
    }
    fclose(hfile);
  }
  return file;
}

inline void chkobject (dlist * o)
{
  if (!o)
    perr ("ERROR - Nonexistent object");
}

inline var *propertyptr (dlist * o, int i)
{
  chkobject (o);
  return ((var *) (((char *) o) + sizeof (dlist) + sizeof (int))) + i;
}

inline var *property (dlist * o, int i)
{
  if (o == 0)
    return &nullv;
  return ((var *) (((char *) o) + sizeof (dlist) + sizeof (int))) + i;
}

inline int *classnoptr (dlist * o)
{
  chkobject (o);
  return (int *) (o + 1);
}

inline int classno (dlist * o)
{
  if (o == 0)
    return -1;
  return *((int *) (o + 1));
}

int findvocab (char *s)
{
  char **result = (char **) bsearch (&s, vocab, nvocab, sizeof (char *),
                                     strpcmp);

  if (!result)
    return -1;
  return (int) (result - vocab);
}

int printx;
int printy;

void printnewline (void)
{
  putchar ('\n');
  printx = 0;
#ifndef __EMSCRIPTEN__
  printy++;
  if (printy == 24) {
    printf ("MORE...");
    getchar ();
    printf ("\r        \r");
    printy = 0;
  }
#endif
}

void printchar (int c)
{
  static char buf[84];
  static int i;

  switch (c) {
  case '\n':
  case ' ':
    if (printx + i > 78 && i <= 79)
      printnewline ();
    if (printx) {
      putchar (' ');
      printx++;
    }
    buf[i] = 0;
    printf ("%s", buf);
    printx += i;
    i = 0;
    if (c == '\n')
      printnewline ();
    break;
  default:
    buf[i++] = c;
    if (i == sizeof (buf))
      perr ("ERROR - Word too long");
  }
}

void print (char *s)
{
  while (*s)
    printchar (*s++);
}

void print (int i)
{
  char s[12];

  sprintf (s, "%d", i);
  print (s);
}

void input (char *s)
{
  printx = printy = 0;
#ifdef __EMSCRIPTEN__
  // NOP, wait for cSendCmd called.
#else
  gets (s);
#endif
}

int getinput_i(void)
{
  int i, j, k, c;

  for (i = 0; buf[i]; i++)
    if (buf[i] == '\t' || buf[i] == '\'' || buf[i] == '"')
      buf[i] = ' ';
  nwords = 0;
  comma = -1;
  for (i = 0;;) {
    while (buf[i] == ' ')
      i++;
    if (!buf[i])
      return TRUE;
    if (buf[i] == ',') {
      if (comma >= 0) {
        print ("Commands should not have more than one comma.\n");
        return FALSE;
      }
      comma = nwords;
      i++;
      continue;
    }
    j = i;
    do {
      buf[j] = tolower (buf[j]);
      j++;
    }
    while (buf[j] && buf[j] != ' ' && buf[j] != ',');
    c = buf[j];
    buf[j] = 0;
    if (strcmp (buf + i, "the")) {
      if (buf[i] == '-') {
        print ("Negative numbers not implemented.\n");
        return FALSE;
      }
      if (isdigit (buf[i]))
        words[nwords++] = ~atoi (buf + i);
      else {
        k = findvocab (buf + i);
        if (k < 0) {
          print ("I don't understand the word \"");
          print (buf + i);
          print ("\".\n");
          return FALSE;
        }
        if (nwords == MAXWORDS) {
          print ("There were too many words in that command.\n");
          return FALSE;
        }
        words[nwords++] = k;
      }
    }
    buf[j] = c;
    i = j;
  }
}

int getinput (void)
{
  print ("> ");
  input (buf);
#ifndef __EMSCRIPTEN__
  return getinput_i();
#else
  return FALSE;
#endif
}

int findclass (int *words, int nwords)
{
  int i, j;

  for (i = 0; i < nclasses; i++)
    for (j = 0; j < classes[i].nphrases; j++)
      if (nwords == classes[i].phrases[j].nwords &&
        !memcmp (words, classes[i].phrases[j].words, nwords * sizeof (int)))
      return i;

  return -1;
}

void writevars (FILE *file, int n, var * vars, int *vartypes)
{
  dlist *o;
  int i;

  do {
    var v = *vars;

    if (*vartypes == T_OBJECT) {
      for (o = objects.next, i = 0; o != &objects; o = o->next, i++)
        if (v.o == o) {
          v.i = i;
          break;
        }
      if (o == &objects)
        v.i = -1;
    }
    Write (file, &v, sizeof (v));
    vars++;
    vartypes++;
  }
  while (--n > 0);
}

void savegame (void)
{
  FILE *file;
  dlist *o;

  print ("File name? [");
  print (filename);
  print ("] ");
  input (buf);
  if (buf[0] == 0)
    strcpy (buf, filename);
  file = fopen(buf, "wb");
  if (!file) {
    print ("Can't create file.\n");
    return;
  }
  strcpy (filename, buf);
  Write (file, (void*)"oas\1", 4);
  writevars (file, nvars, vars, vartypes);
  writeint (file, objects.len ());
  for (o = objects.next; o != &objects; o = o->next) {
    writeint (file, *classnoptr (o));
    writevars (file, nproperties, propertyptr (o, 0), propertytypes);
  }
  fclose (file);
}

void chkvars (int n, var * vars, int *vartypes, dlist * o)
{
  do {
    if (*vartypes == T_OBJECT && vars->o == o)
      vars->o = 0;
    vars++;
    vartypes++;
  }
  while (--n > 0);
}

void restorevars (int n, var * vars, int *vartypes)
{
  dlist *o;
  int i;

  do {
    if (*vartypes == T_OBJECT) {
      if (vars->i < 0)
        vars->o = 0;
      else {
        for (o = objects.next, i = 0; i != vars->i; o = o->next, i++)
          assert (o != &objects);
        vars->o = o;
      }
    }
    vars++;
    vartypes++;
  }
  while (--n > 0);
}

int loadgame (void)
{
  char *file, *filehead;
  int i;
  char gamecode[4];
  dlist *o;

  print ("File name? [");
  print (filename);
  print ("] ");
  input (buf);
  if (buf[0] == 0)
    strcpy (buf, filename);
  filehead = file = LoadFileStream(buf);
  if (!file) {
    perr("Load file %s fail", buf);
  }
  Read (&file, gamecode, 4);
  if (memcmp (gamecode, "oas\1", 4)) {
    print ("Not a saved position for this game.\n");
    delete [] filehead;
    return FALSE;
  }
  strcpy (filename, buf);
  objects.free ();
  Read (&file, vars, nvars * sizeof (var));
  i = readint (&file);
  do {
    o = (dlist *) new char[sizeof (dlist) + sizeof (int) +
                           nproperties * sizeof (var)];
    memset (((char *) o) + sizeof (dlist) + sizeof (int), 0,
            nproperties * sizeof (var));

    *classnoptr (o) = readint (&file);
    Read (&file, propertyptr (o, 0), nproperties * sizeof (var));
    objects += o;
  }
  while (--i > 0);
  delete [] filehead;
  restorevars (nvars, vars, vartypes);
  for (o = objects.next; o != &objects; o = o->next)
    restorevars (nproperties, propertyptr (o, 0), propertytypes);
  return TRUE;
}

int getyn (char *s)
{
  for (;;) {
    print (s);
    input (buf);
    if (buf[0] == 'y' || buf[0] == 'Y')
      return TRUE;
    if (buf[0] == 'n' || buf[0] == 'N')
      return FALSE;
  }
}

var applymethod (dlist * o, int methodno, var * argv)
{
  var v = {0};
  int pc;
  dlist *o2, *o3;
  instruction I;
  int i;

  if (o == 0 && methodno != initmethod)
    return nullv;
  method *m = &methods[methodno];
  int bp = sp;

  memset (stack + bp, 0, m->nvars + sizeof (var));
  sp += m->nvars;
  for (pc = 0; pc != m->ninstructions; pc++) {
    assert (sp >= 0);
    if (sp >= MAXSTACK)
      perr ("ERROR - Stack overflow");
    I = m->instructions[pc];
    switch (I.op) {
    case I_JMP:
      pc += I.i;
      break;
    case I_JF:
      if (!stack[--sp].i)
        pc += I.i;
      break;
    case I_JT:
      if (stack[--sp].i)
        pc += I.i;
      break;
    case I_INT:
      stack[sp++].i = I.i;
      break;
    case I_OR:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i || stack[sp].i;
      break;
    case I_AND:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i && stack[sp].i;
      break;
    case I_NOT:
      stack[sp - 1].i = !stack[sp - 1].i;
      break;
    case I_REFLOCALVAR:
      stack[sp++].v = stack + bp + I.i;
      break;
    case I_REFARG:
      stack[sp++].v = argv + I.i;
      break;
    case I_REFGLOBALVAR:
      stack[sp++].v = vars + I.i;
      break;
    case I_REFPROPERTY:
      stack[sp - 1].v = property (stack[sp - 1].o, I.i);
      break;
    case I_DEREF:
      stack[sp - 1] = *stack[sp - 1].v;
      break;
    case I_CALLPROC:
      applymethod (stack[sp - methods[I.i].nargs - 1].o, I.i,
                   stack + sp - methods[I.i].nargs);
      if (restart)
        return nullv;
      sp -= methods[I.i].nargs + 1;
      break;
    case I_CALLFUNC:
      v = applymethod (stack[sp - methods[I.i].nargs - 1].o, I.i,
                       stack + sp - methods[I.i].nargs);
      if (restart)
        return nullv;
      sp -= methods[I.i].nargs + 1;
      stack[sp++] = v;
      break;
    case I_ASSIGN:
      sp -= 2;
      *stack[sp].v = stack[sp + 1];
      break;
    case I_DESTROY:
      o2 = stack[--sp].o;
      chkobject (o2);
      o2->remove ();
      for (o3 = objects.next; o3 != &objects; o3 = o3->next)
        chkvars (nproperties, propertyptr (o3, 0), propertytypes, o2);
      chkvars (nvars, vars, vartypes, o2);
      break;
    case I_CREATE:
      o2 = (dlist *) new char[sizeof (dlist) + sizeof (int) +
                              nproperties * sizeof (var)];
      memset (((char *) o2) + sizeof (dlist) + sizeof (int), 0,
              nproperties * sizeof (var));

      *classnoptr (o2) = I.i;
      objects += o2;
      stack[sp++].o = o2;
      break;
#ifndef __EMSCRIPTEN__
    case I_QUIT:
      if (!getyn ("Are you sure? (Y/N) "))
        break;
#endif
    case I_EXIT:
      restart = TRUE;
      return nullv;
    case I_THIS:
      stack[sp++].o = o;
      break;
    case I_RETFUNC:
      v = stack[--sp];
    case I_RETPROC:
      assert (sp == bp + m->nvars);
      sp = bp;
      return v;
    case I_EXISTS:
      stack[sp - 1].i = (stack[sp - 1].o != 0);
      break;
    case I_OBJECT:
      o2 = 0;
      i = stack[sp - 1].i;
      if (i > 0) {
        for (o2 = objects.next; i > 1 && o2 != &objects; o2 = o2->next, i--);
        if (i > 1)
          o2 = 0;
      }
      stack[sp - 1].o = o2;
      break;
    case I_PRINTINT:
      print (stack[--sp].i);
      break;
    case I_PRINTSTR:
      print (strings[stack[--sp].i]);
      break;
#ifndef __EMSCRIPTEN__
    case I_SAVE:
      savegame ();
      break;
    case I_LOAD:
      stack[sp++].i = loadgame ();
      break;
#endif
    case I_MINUS:
      stack[sp - 1].i = -stack[sp - 1].i;
      break;
    case I_ADD:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i + stack[sp].i;
      break;
    case I_SUB:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i - stack[sp].i;
      break;
    case I_MUL:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i * stack[sp].i;
      break;
    case I_DIV:
      sp--;
      if (!stack[sp].i)
        perr ("ERROR - Division by zero");
      stack[sp - 1].i = stack[sp - 1].i / stack[sp].i;
      break;
    case I_MOD:
      sp--;
      if (!stack[sp].i)
        perr ("ERROR - Division by zero");
      stack[sp - 1].i = stack[sp - 1].i % stack[sp].i;
      break;
    case I_EQ:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i == stack[sp].i;
      break;
    case I_NE:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i != stack[sp].i;
      break;
    case I_GT:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i > stack[sp].i;
      break;
    case I_LT:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i < stack[sp].i;
      break;
    case I_GE:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i >= stack[sp].i;
      break;
    case I_LE:
      sp--;
      stack[sp - 1].i = stack[sp - 1].i <= stack[sp].i;
      break;
    case I_OEQ:
      sp--;
      stack[sp - 1].i = stack[sp - 1].o == stack[sp].o;
      break;
    case I_ONE:
      sp--;
      stack[sp - 1].i = stack[sp - 1].o != stack[sp].o;
      break;
    case I_RANDOM:
      if (!stack[sp - 1].i)
        perr ("ERROR - Division by zero");
      stack[sp - 1].i = rand ()% stack[sp - 1].i;
      break;
    case I_IS:
      stack[sp - 1].i = (classno (stack[sp - 1].o) == I.i);
      break;
    case I_NEXT:
      if (stack[sp - 1].o)
        if ((stack[sp - 1].o = stack[sp - 1].o->next) == &objects)
          stack[sp - 1].o = 0;
      break;
    default:
      assert (FALSE);
    }
  }
  assert (sp == bp + m->nvars);
  sp = bp;
  return nullv;
}

dlist *nountoobject (int n, int methodno)
{
  dlist *o;

  for (o = objects.next; o != &objects; o = o->next)
    if (*classnoptr (o) == n) {
      if (methodno < 0)
        return o;
      var v = applymethod (o, methodno, 0);

      if (v.i)
        return o;
    }
  return 0;
}

void docommand (int addressee, int methodno)
{
  int i;
  dlist *ad = vars[0].o;

  if (addressee >= 0)
    if (selectaddresseemethod >= 0) {
      ad = nountoobject (addressee, selectaddresseemethod);
      if (ad == 0) {
        if (methods[selectaddresseemethod].noselect >= 0)
          print (strings[methods[selectaddresseemethod].noselect]);
        else
          print ("You can't talk to that.\n");
        return;
      }
    } else {
      print ("You can't talk to that.\n");
      return;
    }
  for (i = 0; i < nnouns; i++)
    if (methods[methodno].argtypes[i] == T_OBJECT) {
      nouns[i].o = nountoobject (nouns[i].i, methods[methodno].selectors[i]);
      if (nouns[i].o == 0) {
        if (methods[methods[methodno].selectors[i]].noselect >= 0)
          print (strings[methods[methods[methodno].selectors[i]].noselect]);
        return;
      }
    }
  applymethod (ad, methodno, nouns);
}

int command (void)
{
  int firstword, addressee, thisword, wordsleft, vwordsleft, vword, nounwords;
  int noun = -1, i, j, k, argno;

  firstword = 0;
  addressee = -1;
  if (comma >= 0) {
    if (comma == 0) {
      print ("I don't know who you're trying to talk to.\n");
      return FALSE;
    }
    addressee = findclass (words, comma);
    if (addressee < 0) {
      print ("I don't know who you're trying to talk to.\n");
      return FALSE;
    }
    nwords -= firstword = comma;
  }
  for (i = 0; i < nmethods; i++)
    if (methods[i].nverbs)
      for (j = 0; j < methods[i].nverbs; j++) {
        thisword = firstword;
        wordsleft = nwords;
        vwordsleft = methods[i].verbs[j].nwords;
        nnouns = 0;
        argno = 0;
        for (k = 0;; k++) {
          if (wordsleft == 0) {
            if (k == methods[i].verbs[j].nwords) {
              docommand (addressee, i);
              return TRUE;
            }
            break;
          }
          if (k == methods[i].verbs[j].nwords)
            break;
          vwordsleft--;
          vword = methods[i].verbs[j].words[k];
          if (vword >= 0) {
            if (vword != words[thisword])
              break;
            thisword++;
            wordsleft--;
          }
          else {
            if (methods[i].argtypes[argno++] == T_INT) {
              if (words[thisword] >= 0)
                break;
              nouns[~vword].i = ~words[thisword];
              thisword++;
              wordsleft--;
            }
            else {
              nounwords = 0;
              do {
                nounwords++;
                if (nounwords + vwordsleft > wordsleft)
                  break;
                noun = findclass (words + thisword, nounwords);
              }
              while (noun < 0);
              if (noun < 0)
                break;
              nouns[~vword].i = noun;
              thisword += nounwords;
              wordsleft -= nounwords;
            }
            nnouns = max (nnouns, -vword);
          }
        }
      }
  print ("I don't understand you.\n");
  return FALSE;
}

void LoadGameFromStream(char *file)
{
  int n, i, j, k;

  Read (&file, buf, 4);
  if (memcmp (buf, "oas", 4))
    perr ("Not an OASYS file");

  n = readint (&file);
  strings = new char *[n];

  for (i = 0; i < n; i++) {
    j = readint (&file);
  strings[i] = new char[j + 1];

    Read (&file, strings[i], j);
    strings[i][j] = 0;
  }

  nvars = readint (&file);
  vars = new var[nvars];
  vartypes = new int[nvars];
  Read (&file, vartypes, nvars * sizeof (int));

  nproperties = readint (&file);
  propertytypes = new int[nproperties];
  Read (&file, propertytypes, nproperties * sizeof (int));

  nvocab = readint (&file);
  vocab = new char *[nvocab];

  for (i = 0; i < nvocab; i++) {
    j = readint (&file);
    vocab[i] = new char[j + 1];

    Read (&file, vocab[i], j);
    vocab[i][j] = 0;
  }

  nclasses = readint (&file);
  classes = new Class[nclasses];
  for (i = 0; i < nclasses; i++) {
  int nphrases = readint (&file);

    classes[i].nphrases = nphrases;
    if (nphrases) {
      classes[i].phrases = new phrase[nphrases];
      for (j = 0; j < nphrases; j++) {
        k = readint (&file);
        classes[i].phrases[j].nwords = k;
        classes[i].phrases[j].words = new int[k];
        Read (&file, classes[i].phrases[j].words, k * sizeof (int));
      }
    }
  }

  nmethods = readint (&file);
  methods = new method[nmethods];
  initmethod = readint (&file);
  selectaddresseemethod = readint (&file);
  for (i = 0; i < nmethods; i++) {
    method *m = &methods[i];

    m->type = readint (&file);
    j = readint (&file);
    m->nargs = j;
    if (j) {
      m->argtypes = new int[j];
      m->selectors = new int[j];

      for (k = 0; k < j; k++) {
        m->argtypes[k] = readint (&file);
        m->selectors[k] = readint (&file);
      }
    }
    m->nvars = readint (&file);
    if (m->nvars) {
      m->vartypes = new int[m->nvars];
      Read (&file, m->vartypes, m->nvars * sizeof (int));
    }
    int nverbs = readint (&file);

    m->nverbs = nverbs;
    if (nverbs) {
      m->verbs = new phrase[nverbs];
      for (j = 0; j < nverbs; j++) {
        k = readint (&file);
        m->verbs[j].nwords = k;
        m->verbs[j].words = new int[k];
        Read (&file, m->verbs[j].words, k * sizeof (int));
      }
    }
    m->noselect = readint (&file);
    k = readint (&file);
    m->ninstructions = k;
    if (k) {
      m->instructions = new instruction[k];
      Read (&file, m->instructions, k * sizeof (instruction));
    }
  }
}

void LoadGame(char *filename)
{
  char *file;

  file = LoadFileStream(filename);
  if (!file) {
    perr("Load file %s fail", filename);
  }

  LoadGameFromStream(file);

  delete [] file;
}

void NewGame()
{
  objects.free ();
  sp = 0;
  memset (vars, 0, nvars * sizeof (var));
  restart = FALSE;
  applymethod (0, initmethod, 0);
}

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#include "ESCAPE.h"
extern "C" {
int EMSCRIPTEN_KEEPALIVE cSendCmd(char *pBuff)
{
  strcpy(buf, pBuff);
  if (getinput_i()) {
    printf("%s\n", pBuff);
    command();
  }
  getinput();
  return 0;
}
} // extern "C"
#endif

int main (int argc, char **argv)
{
  srand((unsigned int)time(0));

#ifdef __EMSCRIPTEN__
  LoadGameFromStream((char*)BIN);
  NewGame();
  getinput();
  emscripten_exit_with_live_runtime();
#else
  if (argc != 2 || !strcmp (argv[1], "?"))
    perr ("Object-Oriented Adventure Interpreter" VERSION "\n"
          "Usage: oai filename");
  LoadGame(argv[1]);
  NewGame();
  for (;;) {
    assert (sp == 0);
    if (!getinput ()) {
      continue;
    }
    command ();
    if (restart) {
      if (!getyn ("Would you like to play again? (Y/N) "))
        return 0;
      NewGame();
    }
  }
#endif
}
