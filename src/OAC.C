#include <stdio.h>
#include <limits.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include "OALIB.H"
#include "INS.H"

enum {
  TK_ID,
  TK_INT,
  TK_STR,
};

struct token {
  int type;
  union {
    int i;
    char *s;
  };
};

struct word:slist
{
  char *name;
};

struct phrase:slist
{
  slisthead words;
};

struct variable:slist
{
  char *name;
  int type;
};

struct arg:variable
{
  int selector;
};

struct instruction:slist
{
  int op;
  int i;
};

struct method:slist
{
  char *name;
  int type;
  slisthead args;
  slisthead verbs;
  int noselect;
  slisthead variables;
  slisthead code;
};

struct Class:slist
{
  char *name;
  slisthead nouns;
};

int parseexp (method *);

char *keyword[]=
{
  "int",
  "string",
  "object",
  "class",
  "method",
  "verbs",
  "if",
  "else",
  "print",
  "and",
  "or",
  "do",
  "while",
  "property",
  "destroy",
  "create",
  "exit",
  "quit",
  "this",
  "return",
  "not",
  "break",
  "continue",
  "exists",
  "save",
  "load",
  "player",
  "init",
  "select_addressee",
  "random",
  "is",
  "next",
};

enum {
  KW_INT,
  KW_STRING,
  KW_OBJECT,
  KW_CLASS,
  KW_METHOD,
  KW_VERBS,
  KW_IF,
  KW_ELSE,
  KW_PRINT,
  KW_AND,
  KW_OR,
  KW_DO,
  KW_WHILE,
  KW_PROPERTY,
  KW_DESTROY,
  KW_CREATE,
  KW_EXIT,
  KW_QUIT,
  KW_THIS,
  KW_RETURN,
  KW_NOT,
  KW_BREAK,
  KW_CONTINUE,
  KW_EXISTS,
  KW_SAVE,
  KW_LOAD,
  KW_PLAYER,
  KW_INIT,
  KW_SELECT_ADDRESSEE,
  KW_RANDOM,
  KW_IS,
  KW_NEXT,
  MAXKEYWORDS,
};

#define ID_INT               keyword[KW_INT]
#define ID_STRING            keyword[KW_STRING]
#define ID_OBJECT            keyword[KW_OBJECT]
#define ID_CLASS             keyword[KW_CLASS]
#define ID_METHOD            keyword[KW_METHOD]
#define ID_VERBS             keyword[KW_VERBS]
#define ID_IF                keyword[KW_IF]
#define ID_ELSE              keyword[KW_ELSE]
#define ID_PRINT             keyword[KW_PRINT]
#define ID_AND               keyword[KW_AND]
#define ID_OR                keyword[KW_OR]
#define ID_DO                keyword[KW_DO]
#define ID_WHILE             keyword[KW_WHILE]
#define ID_PROPERTY          keyword[KW_PROPERTY]
#define ID_DESTROY           keyword[KW_DESTROY]
#define ID_CREATE            keyword[KW_CREATE]
#define ID_EXIT              keyword[KW_EXIT]
#define ID_QUIT              keyword[KW_QUIT]
#define ID_THIS              keyword[KW_THIS]
#define ID_RETURN            keyword[KW_RETURN]
#define ID_NOT               keyword[KW_NOT]
#define ID_BREAK             keyword[KW_BREAK]
#define ID_CONTINUE          keyword[KW_CONTINUE]
#define ID_EXISTS            keyword[KW_EXISTS]
#define ID_SAVE              keyword[KW_SAVE]
#define ID_LOAD              keyword[KW_LOAD]
#define ID_PLAYER            keyword[KW_PLAYER]
#define ID_INIT              keyword[KW_INIT]
#define ID_SELECT_ADDRESSEE  keyword[KW_SELECT_ADDRESSEE]
#define ID_RANDOM            keyword[KW_RANDOM]
#define ID_IS                keyword[KW_IS]
#define ID_NEXT              keyword[KW_NEXT]

FILE *inf;
int line = 1;
int nxch;
token nxtk;
tree symhead = {strcmp};
int bad;
slisthead strings;
int nstrings = 1;
slisthead variables;
slisthead properties;
slisthead classes;
slisthead methods;
int stack[256];
int sp;
ftree vocabtree = {strcmp};
char **vocablist;
int nvocab;

inline int myisalpha (int c)
{
  return isalpha (c) || c == '_';
}

inline int myisalnum (int c)
{
  return isalnum (c) || c == '_';
}

void error (char *format...)
{
  va_list argptr;

  va_start (argptr, format);
  printf ("Fatal Error line %d: ", line);
  vprintf (format, argptr);
  va_end (argptr);
  putchar ('\n');
  exit (1);
}

void warning (char *format...)
{
  va_list argptr;

  va_start (argptr, format);
  printf ("Error line %d: ", line);
  vprintf (format, argptr);
  va_end (argptr);
  putchar ('\n');
  bad = TRUE;
}

inline void readch (void)
{
  if ((nxch = fgetc (inf)) == '\n')
    line++;
}

void lexinit (char *filename)
{
  inf = Fopen (filename, "r");
  readch ();
}

void lex (void)
{
  static char buf[10240];
  int i;

LOOP:
  while (isspace (nxch))
    readch ();
  if (myisalpha (nxch)) {
    i = 0;
    do {
      buf[i++] = tolower (nxch);
      readch ();
    }
    while (myisalnum (nxch));
    buf[i] = 0;
    nxtk.type = TK_ID;
    nxtk.s = strdup (buf);
    nxtk.s = (char *) (symhead += nxtk.s);
    return;
  }
  if (isdigit (nxch)) {
    i = 0;
    do {
      buf[i++] = nxch;
      readch ();
    }
    while (isdigit (nxch));
    buf[i] = 0;
    nxtk.type = TK_INT;
    nxtk.i = atoi (buf);
    return;
  }
  switch (nxch) {
  case '"':
    i = 0;
    do {
      readch ();
      while (nxch != '"') {
        if (nxch == EOF) {
          nxtk.type = EOF;
          return;
        }
        if (nxch == '\n')
          nxch = ' ';
        if (nxch == '\\') {
          int tempch;

          readch ();
          switch (nxch) {
          case 'n':
          case 'N':
            buf[i++] = '\n';
            break;
          case '"':
            buf[i++] = '"';
            break;
          default:
            tempch = nxch;
            readch ();
            buf[i++] = hexchars2byte (tempch, nxch);
          }
        }
        else
          buf[i++] = nxch;
        if (i >= sizeof (buf))
          error ("String too long");
        readch ();
      }
      do
        readch ();
      while (isspace (nxch));
    }
    while (nxch == '"')
      /* empty */;
    buf[i] = 0;
    nxtk.type = TK_STR;
    nxtk.s = strdup (buf);
    return;
  case '+':
  case '-':
  case '*':
  case '%':
  case '{':
  case '}':
  case '(':
  case ')':
  case EOF:
    nxtk.type = nxch;
    readch ();
    return;
  case '/':
    readch ();
    switch (nxch) {
    case '/':
      do
        readch ();
      while (nxch != '\n' && nxch != EOF);
      goto LOOP;
    case '*':
      readch ();
      do {
        while (nxch != '*') {
          if (nxch == EOF) {
            nxtk.type = EOF;
            return;
          }
          readch ();
        }
        readch ();
        if (nxch == EOF) {
          nxtk.type = EOF;
          return;
        }
      } while (nxch != '/');
      readch ();
      goto LOOP;
    default:
      nxtk.type = '/';
      return;
    }
  case '=':
    nxtk.type = '=';
    readch ();
    if (nxch == '=') {
      readch ();
      nxtk.type = '==';
    }
    return;
  case '>':
    nxtk.type = '>';
    readch ();
    if (nxch == '=') {
      readch ();
      nxtk.type = '>=';
    }
    return;
  case '<':
    nxtk.type = '<';
    readch ();
    if (nxch == '=') {
      readch ();
      nxtk.type = '<=';
    }
    return;
  case '!':
    readch ();
    if (nxch == '=') {
      readch ();
      nxtk.type = '!=';
      return;
    }
    error ("Bad character %c", '!');
  default:
    error ("Bad character %c", nxch);
  }
}

int typedecl (void)
{
  return nxtk.type == TK_ID &&
    (nxtk.s == ID_INT || nxtk.s == ID_STRING || nxtk.s == ID_OBJECT);
}

char *parseid (void)
{
  if (nxtk.type != TK_ID)
    error ("Identifier expected");
  char *s = nxtk.s;

  lex ();
  return s;
}

int isid (char *s)
{
  return nxtk.type == TK_ID && nxtk.s == s;
}

void parseid (char *s)
{
  if (!isid (s))
    error ("\"%s\" expected", s);
  lex ();
}

variable *parsevariable (void)
{
  variable *v = new variable;

  if (nxtk.s == ID_INT)
    v->type = T_INT;
  if (nxtk.s == ID_STRING)
    v->type = T_STR;
  if (nxtk.s == ID_OBJECT)
    v->type = T_OBJECT;
  lex ();
  v->name = parseid ();
  return v;
}

arg *parsearg (void)
{
  arg *a = new arg;

  if (nxtk.s == ID_INT)
    a->type = T_INT;
  if (nxtk.s == ID_STRING)
    a->type = T_STR;
  if (nxtk.s == ID_OBJECT)
    a->type = T_OBJECT;
  lex ();
  a->name = parseid ();
  if (a->type == T_OBJECT && nxtk.type == TK_ID && nxtk.s != ID_VERBS) {
    char *s = nxtk.s;
    method *m;
    int i;

    for (m = (method *) methods.next, i = 0; m; m = (method *) m->next, i++)
      if (m->name == s) {
        if (m->type != T_INT)
          warning ("Selector method \"%s\" should be of int type", s);
        if (m->args.next)
          warning ("Selector method \"%s\" should not take arguments", s);
        a->selector = i;
        lex ();
        return a;
      }
  }
  a->selector = -1;
  return a;
}

void parsepunct (int c)
{
  if (nxtk.type != c)
    error ("%c expected", c);
  lex ();
}

word *parseword (void)
{
  word *w = new word;

  w->name = parseid ();
  vocabtree += w->name;
  return w;
}

void parsewordlist (slisthead * h)
{
  parsepunct ('{');
  while (nxtk.type == TK_ID)
    *h += parseword ();
  parsepunct ('}');
}

void parsephraselist (slisthead * h)
{
  parsepunct ('{');
  while (nxtk.type == '{') {
    phrase *p = new phrase;

    parsewordlist (&p->words);
    *h += p;
  }
  parsepunct ('}');
}

instruction *addi (method * m, int i)
{
  instruction *I = new instruction;

  m->code += I;
  I->op = i;
  I->i = 0;
  return I;
}

int parseclassref (void)
{
  Class *c;
  int i;

  if (nxtk.type != TK_ID)
    error ("Class name expected");
  for (c = (Class *) classes.next, i = 0; c; c = (Class *) c->next, i++)
    if (c->name == nxtk.s) {
      lex ();
      return i;
    }
  error ("Class name expected");
  return 0;
}

int findvar (slisthead * h, char *s, variable ** result)
{
  variable *v;
  int i;

  for (v = (variable *) h->next, i = 0; v; v = (variable *) v->next, i++)
    if (v->name == s) {
      *result = v;
      return i;
    }
  return -1;
}

void push (int i)
{
  if (sp == sizeof (stack) / sizeof (int))
      error ("Expression too complex");

  stack[sp++] = i;
}

int pop (void)
{
  if (sp == 0)
    error ("Void type used in expression");
  return stack[--sp];
}

void popobject (void)
{
  if (pop ()!= T_OBJECT)
    warning ("Type must be object");
}

void popstr (void)
{
  if (pop ()!= T_STR)
    warning ("Type must be string");
}

void popint (void)
{
  if (pop ()!= T_INT)
    warning ("Type must be int");
}

int parsefactor (method * m)
{
  int i;
  method *m2;
  int n;
  variable *v;
  word *w;

  switch (nxtk.type) {
  case '-':
    lex ();
    n = parsefactor (m) + 1;
    popint ();
    addi (m, I_MINUS);
    push (T_INT);
    return n;
  case TK_INT:
    addi (m, I_INT)->i = nxtk.i;
    push (T_INT);
    lex ();
    return 1;
  case TK_STR:
    w = new word;
    w->name = nxtk.s;
    strings += w;
    addi (m, I_INT)->i = nstrings++;
    push (T_STR);
    lex ();
    return 1;
  case '(':
    lex ();
    n = parseexp (m);
    parsepunct (')');
    break;
  case TK_ID:
    if (nxtk.s == ID_LOAD) {
      lex ();
      addi (m, I_LOAD);
      push (T_INT);
      return 1;
    }
    if (nxtk.s == ID_RANDOM) {
      lex ();
      n = parseexp (m) + 1;
      popint ();
      addi (m, I_RANDOM);
      push (T_INT);
      return n;
    }
    if (nxtk.s == ID_NOT) {
      lex ();
      n = parsefactor (m) + 1;
      popint ();
      addi (m, I_NOT);
      push (T_INT);
      return n;
    }
    if (nxtk.s == ID_CREATE) {
      lex ();
      addi (m, I_CREATE)->i = parseclassref ();
      push (T_OBJECT);
      n = 1;
      break;
    }
    if (nxtk.s == ID_THIS) {
      lex ();
      addi (m, I_THIS);
      push (T_OBJECT);
      n = 1;
      break;
    }
    if (nxtk.s == ID_OBJECT) {
      lex ();
      n = parseexp (m) + 1;
      popint ();
      addi (m, I_OBJECT);
      push (T_OBJECT);
      break;
    }
    i = findvar (&m->variables, nxtk.s, &v);
    if (i >= 0) {
      addi (m, I_REFLOCALVAR)->i = i;
      lex ();
      n = 1;
      if (nxtk.type != '=') {
        addi (m, I_DEREF);
        push (v->type);
        n = 2;
      } else
        push (v->type | T_REF);
      break;
    }
    i = findvar (&m->args, nxtk.s, &v);
    if (i >= 0) {
      addi (m, I_REFARG)->i = i;
      lex ();
      n = 1;
      if (nxtk.type != '=') {
        addi (m, I_DEREF);
        push (v->type);
        n = 2;
      } else
        push (v->type | T_REF);
      break;
    }
    i = findvar (&variables, nxtk.s, &v);
    if (i >= 0) {
      addi (m, I_REFGLOBALVAR)->i = i;
      lex ();
      n = 1;
      if (nxtk.type != '=') {
        addi (m, I_DEREF);
        push (v->type);
        n = 2;
      } else
        push (v->type | T_REF);
      break;
    }
    error ("\"%s\" is not a valid expression", nxtk.s);
  default:
    error ("Expression expected");
  }
  for (;;) {
    if (nxtk.type != TK_ID)
      return n;
    if (nxtk.s == ID_EXISTS) {
      lex ();
      popobject ();
      addi (m, I_EXISTS);
      push (T_INT);
      return n + 1;
    }
    if (nxtk.s == ID_IS) {
      lex ();
      popobject ();
      addi (m, I_IS)->i = parseclassref ();
      push (T_INT);
      return n + 1;
    }
    if (nxtk.s == ID_NEXT) {
      lex ();
      popobject ();
      addi (m, I_NEXT);
      push (T_OBJECT);
      n++;
      continue;
    }
    i = findvar (&properties, nxtk.s, &v);
    if (i >= 0) {
      lex ();
      addi (m, I_REFPROPERTY)->i = i;
      popobject ();
      n++;
      if (nxtk.type != '=') {
        addi (m, I_DEREF);
        push (v->type);
        n++;
      } else
        push (v->type | T_REF);
      continue;
    }
    for (m2 = (method *) methods.next, i = 0; m2; m2 = (method *) m2->next, i++)
      if (m2->name == nxtk.s)
        break;
      if (m2) {
        lex ();
        popobject ();
        for (v = (variable *) m2->args.next; v; v = (variable *) v->next) {
          n += parseexp (m);
          if (pop ()!= v->type)
            warning ("Argument of wrong type for \"%s\"", m2->name);
        }
        n++;
        if (m2->type == T_VOID)
          addi (m, I_CALLPROC)->i = i;
        else {
          addi (m, I_CALLFUNC)->i = i;
          push (m2->type);
        }
        continue;
      }
    return n;
  }
}

int parseterm (method * m)
{
  int n = parsefactor (m);

  while (nxtk.type == '*' || nxtk.type == '/' || nxtk.type == '%') {
    int i = nxtk.type;

    lex ();
    popint ();
    n += parsefactor (m) + 1;
    popint ();
    switch (i) {
    case '*':
      addi (m, I_MUL);
      break;
    case '/':
      addi (m, I_DIV);
      break;
    case '%':
      addi (m, I_MOD);
      break;
    }
    push (T_INT);
  }
  return n;
}

int parsemathexp (method * m)
{
  int n = parseterm (m);

  while (nxtk.type == '+' || nxtk.type == '-') {
    int i = nxtk.type;

    lex ();
    popint ();
    n += parseterm (m) + 1;
    popint ();
    switch (i) {
    case '+':
      addi (m, I_ADD);
      break;
    case '-':
      addi (m, I_SUB);
      break;
    }
    push (T_INT);
  }
  return n;
}

int parserelexp (method * m)
{
  int n = parsemathexp (m);

  while (nxtk.type == '>' || nxtk.type == '<' || nxtk.type == '>=' ||
         nxtk.type == '<=') {
    int i = nxtk.type;

    lex ();
    popint ();
    n += parsemathexp (m) + 1;
    popint ();
    switch (i) {
    case '>':
      addi (m, I_GT);
      break;
    case '<':
      addi (m, I_LT);
      break;
    case '>=':
      addi (m, I_GE);
      break;
    case '<=':
      addi (m, I_LE);
      break;
    }
    push (T_INT);
  }
  return n;
}

int parseeqexp (method * m)
{
  int n = parserelexp (m);

  while (nxtk.type == '==' || nxtk.type == '!=') {
    int i = nxtk.type;

    lex ();
    n += parserelexp (m) + 1;
    switch (pop ()){
    case T_INT:
      popint ();
      switch (i) {
      case '==':
        addi (m, I_EQ);
        break;
      case '!=':
        addi (m, I_NE);
        break;
      }
      break;
    case T_STR:
      popstr ();
      switch (i) {
      case '==':
        addi (m, I_EQ);
        break;
      case '!=':
        addi (m, I_NE);
        break;
      }
      break;
    case T_OBJECT:
      popobject ();
      switch (i) {
      case '==':
        addi (m, I_OEQ);
        break;
      case '!=':
        addi (m, I_ONE);
        break;
      }
      break;
    default:
      assert (FALSE);
    }
    push (T_INT);
  }
  return n;
}

int parseandexp (method * m)
{
  int n = parseeqexp (m);

  while (isid (ID_AND)) {
    lex ();
    popint ();
    n += parseeqexp (m) + 1;
    popint ();
    addi (m, I_AND);
    push (T_INT);
  }
  return n;
}

int parseexp (method * m)
{
  int n = parseandexp (m);

  while (isid (ID_OR)) {
    lex ();
    popint ();
    n += parseandexp (m) + 1;
    popint ();
    addi (m, I_OR);
    push (T_INT);
  }
  return n;
}

int parsestatement (method * m)
{
  int n = 0;
  int i, expi;
  instruction *I, *oldI;

  switch (nxtk.type) {
  case '{':
    lex ();
    while (nxtk.type != '}')
      n += parsestatement (m);
    lex ();
    break;
  case TK_ID:
    if (nxtk.s == ID_PRINT) {
      lex ();
      n = parseexp (m) + 1;
      switch (pop ()){
      case T_INT:
        addi (m, I_PRINTINT);
        break;
      case T_STR:
        addi (m, I_PRINTSTR);
        break;
      default:
        warning ("Can only print int or string expression");
      }
      break;
    }
    if (nxtk.s == ID_BREAK) {
      lex ();
      addi (m, I_BREAK)->i = -1;
      n = 1;
      break;
    }
    if (nxtk.s == ID_CONTINUE) {
      lex ();
      addi (m, I_CONTINUE)->i = -1;
      n = 1;
      break;
    }
    if (nxtk.s == ID_RETURN) {
      lex ();
      if (m->type == T_VOID) {
        addi (m, I_RETPROC);
        n = 1;
      }
      else {
        n = parseexp (m) + 1;
        if (pop ()!= m->type)
          warning ("Return value of wrong type");
        addi (m, I_RETFUNC);
      }
      break;
    }
    if (nxtk.s == ID_DESTROY) {
      lex ();
      n = parseexp (m) + 1;
      addi (m, I_DESTROY);
      popobject ();
      break;
    }
    if (nxtk.s == ID_EXIT) {
      lex ();
      addi (m, I_EXIT);
      n = 1;
      break;
    }
    if (nxtk.s == ID_QUIT) {
      lex ();
      addi (m, I_QUIT);
      n = 1;
      break;
    }
    if (nxtk.s == ID_SAVE) {
      lex ();
      addi (m, I_SAVE);
      n = 1;
      break;
    }
    if (nxtk.s == ID_IF) {
      lex ();
      n = parseexp (m) + 1;
      I = addi (m, I_JF);
      popint ();
      n += (i = parsestatement (m));
      if (isid (ID_ELSE)) {
        lex ();
        I->i = i + 1;
        I = addi (m, I_JMP);
        n += (i = parsestatement (m)) + 1;
      }
      I->i = i;
      break;
    }
    if (nxtk.s == ID_DO) {
      lex ();
      oldI = (instruction *) m->code.last;
      n = expi = parsestatement (m);
      parseid (ID_WHILE);
      n += parseexp (m) + 1;
      I = addi (m, I_JT);
      popint ();
      I->i = -n;
      expi = n - expi;
      for (I = (instruction *) oldI->next, i = n - 1; I; I = (instruction *) I->next, i--)
        if (I->i < 0) {
          if (I->op == I_BREAK) {
            I->op = I_JMP;
            I->i = i;
          }
          if (I->op == I_CONTINUE) {
            I->op = I_JMP;
            I->i = i - expi;
          }
        }
      break;
    }
    if (nxtk.s == ID_WHILE) {
      lex ();
      n = parseexp (m) + 1;
      I = addi (m, I_JF);
      popint ();
      oldI = (instruction *) m->code.last;
      n += (I->i = (i = parsestatement (m)) + 1);
      I = addi (m, I_JMP);
      I->i = -n;
      for (I = (instruction *) oldI->next; I; I = (instruction *) I->next, i--)
        if (I->i < 0) {
          if (I->op == I_BREAK) {
            I->op = I_JMP;
            I->i = i;
          }
          if (I->op == I_CONTINUE) {
            I->op = I_JMP;
            I->i = i - 1;
          }
        }
      break;
    }
    n = parseexp (m);
    if (nxtk.type == '=') {
      i = pop ();
      if (!(i & T_REF))
        warning ("Can only assign to variable or property");
      lex ();
      n += parseexp (m) + 1;
      if ((i & 0x00ff) != pop ())
        warning ("Different types in assignment");
      addi (m, I_ASSIGN);
    }
    break;
  default:
    error ("Statement expected");
  }
  if (sp)
    warning ("Expression outside statement");
  return n;
}

void parsemethod (void)
{
  method *m = new method;

  lex ();
  if (nxtk.type != TK_ID)
    error ("Identifier expected");
  m->type = T_VOID;
  if (nxtk.s == ID_INT) {
    m->type = T_INT;
    lex ();
  } else if (nxtk.s == ID_STRING) {
    m->type = T_STR;
    lex ();
  } else if (nxtk.s == ID_OBJECT) {
    m->type = T_OBJECT;
    lex ();
  }
  m->name = parseid ();
  for (method * m2 = (method *) methods.next; m2; m2 = (method *) m2->next)
    if (m2->name == m->name)
      warning ("Method with duplicate name \"%s\"", m->name);
  while (typedecl ())
    m->args += parsearg ();
  if (isid (ID_VERBS)) {
    lex ();
    parsephraselist (&m->verbs);
    for (arg * a = (arg *) m->args.next; a; a = (arg *) a->next)
      if (a->type == T_STR)
        warning ("Method arguments may not be of string type");
  }
  m->noselect = -1;
  if (nxtk.type == TK_STR) {
    word *w = new word;

    w->name = nxtk.s;
    strings += w;
    m->noselect = nstrings++;
    lex ();
  }
  parsepunct ('{');
  while (typedecl ())
    m->variables += parsevariable ();
  methods += m;
  long n = 0;

  while (nxtk.type != '}')
    n += parsestatement (m);
  if (n * sizeof (int) * 2 > UINT_MAX)
    warning ("Method too large");

  lex ();
}

Class *parseclass (void)
{
  Class *c = new Class;

  lex ();
  c->name = parseid ();
  parsephraselist (&c->nouns);
  return c;
}

void parse (char *filename)
{
  for (int i = 0; i < MAXKEYWORDS; i++)
    symhead += keyword[i];
  lexinit (filename);
  lex ();
  variable *v = new variable;

  v->type = T_OBJECT;
  v->name = ID_PLAYER;
  variables += v;
  word *w = new word;

  w->name = "*NULL STRING*";
  strings += w;
  for (;;)
    switch (nxtk.type) {
    case EOF:
      return;
    case TK_ID:
      if (typedecl ()){
        variables += parsevariable ();
        continue;
      }
      if (nxtk.s == ID_PROPERTY) {
        lex ();
        properties += parsevariable ();
        continue;
      }
      if (nxtk.s == ID_METHOD) {
        parsemethod ();
        continue;
      }
      if (nxtk.s == ID_CLASS) {
        classes += parseclass ();
        continue;
      }
    default:
      error ("Variable, method or class definition expected");
    }
}

int findvocab (char *s)
{
  char **result = (char **) bsearch (&s, vocablist, nvocab, sizeof (char *),
                                     strpcmp);

  assert (result);
  return (int) (result - vocablist);
}

int copyvocab (ftreenode * t, int i)
{
  if (!t)
    return 0;
  int n = copyvocab (t->left, i);

  vocablist[i + n] = (char *) t->data;
  return n + 1 + copyvocab (t->right, i + n + 1);
}

void writephraselist (FILE *file, slisthead * h)
{
  writeint (file, h->len ());
  for (phrase * p = (phrase *) h->next; p; p = (phrase *) p->next) {
    writeint (file, p->words.len ());
    for (word * w = (word *) p->words.next; w; w = (word *) w->next)
      writeint (file, findvocab (w->name));
  }
}

int treenode::size (void)
{
  if (!this)
    return 0;
  return left->size ()+ 1 + right->size ();
}

void output (char *filename)
{
  int i, j;
  method *m;
  variable *v;
  word *w;
  phrase *p;
  Class *c;
  instruction *I;
  FILE *file;

  file = Fopen(filename, "wb");

  Write (file, (void *)"oas", 4);

  writeint (file, nstrings);
  for (w = (word *) strings.next; w; w = (word *) w->next) {
    i = (int)strlen (w->name);
    writeint (file, i);
    Write (file, w->name, i);
  }
  if (!nstrings)
    warning ("No strings defined");

  writeint (file, variables.len ());
  for (v = (variable *) variables.next; v; v = (variable *) v->next)
    writeint (file, v->type);

  i = properties.len ();
  if (i == 0)
    warning ("No properties defined");
  writeint (file, i);
  for (v = (variable *) properties.next; v; v = (variable *) v->next)
    writeint (file, v->type);

  nvocab = vocabtree.size ();
  writeint (file, nvocab);
  if (nvocab) {
    vocablist = new char *[nvocab];

    i = copyvocab (vocabtree.t, 0);
    assert (i == nvocab);
    for (i = 0; i < nvocab; i++) {
      j = (int)strlen (vocablist[i]);
      writeint (file, j);
      Write (file, vocablist[i], j);
    }
  } else
    warning ("No vocabulary defined");

  writeint (file, classes.len ());
  for (c = (Class *) classes.next; c; c = (Class *) c->next)
    writephraselist (file, &c->nouns);

  writeint (file, methods.len ());
  for (m = (method *) methods.next, i = 0; m; m = (method *) m->next, i++)
    if (m->name == ID_INIT) {
      if (m->type != T_VOID)
        warning ("Init method should not return a value");
      if (m->args.next)
        warning ("Init method should not take arguments");
      writeint (file, i);
      break;
    }
  if (!m)
    warning ("Init method not defined");
  for (m = (method *) methods.next, i = 0; m; m = (method *) m->next, i++)
    if (m->name == ID_SELECT_ADDRESSEE) {
      if (m->type != T_INT)
        warning ("Select addressee method should return an int value");
      if (m->args.next)
        warning ("Select addressee method should not take arguments");
      writeint (file, i);
      break;
    }
  if (!m)
    writeint (file, -1);
  for (m = (method *) methods.next; m; m = (method *) m->next) {
    writeint (file, m->type);
    writeint (file, m->args.len ());
    for (arg * a = (arg *) m->args.next; a; a = (arg *) a->next) {
      writeint (file, a->type);
      if (m->verbs.next) {
        if (a->selector < 0 && a->type == T_OBJECT)
          warning ("Command method \"%s\" has no selector on object argument", m->name);
      } else if (a->selector >= 0)
        warning ("Non-command method \"%s\" has selector on argument",
                 m->name);
      writeint (file, a->selector);
    }
    writeint (file, m->variables.len ());
    for (v = (variable *) m->variables.next; v; v = (variable *) v->next)
      writeint (file, v->type);
    writeint (file, m->verbs.len ());
    for (p = (phrase *) m->verbs.next; p; p = (phrase *) p->next) {
      writeint (file, p->words.len ());
      for (w = (word *) p->words.next; w; w = (word *) w->next) {
        for (v = (variable *) m->args.next, i = 0; v; v = (variable *) v->next, i++)
          if (v->name == w->name)
            break;
        if (v)
          writeint (file, ~i);
        else
          writeint (file, findvocab (w->name));
      }
    }
    writeint (file, m->noselect);
    if (m->noselect >= 0 && (m->args.next || m->type != T_INT))
      warning ("Non-selector method \"%s\" should not have message", m->name);
    writeint (file, m->code.len ());
    for (I = (instruction *) m->code.next; I; I = (instruction *) I->next) {
      if (I->op == I_BREAK)
        warning ("BREAK outside loop");
      if (I->op == I_CONTINUE)
        warning ("CONTINUE outside loop");
      Write (file, &I->op, 2 * sizeof (int));
    }
  }

  fclose (file);
}

int main (int argc, char **argv)
{
  printf ("Object-Oriented Adventure Compiler" VERSION "\n");
  if (argc != 2 || !strcmp (argv[1], "?"))
    perr ("Usage: oac filename");
  parse (defext (argv[1], ".s"));
  int i = (int)strlen (argv[1]);

  while (--i >= 0)
    if (argv[1][i] == '.') {
      argv[1][i] = 0;
      break;
    }
  output (argv[1]);
  if (bad)
    unlink (argv[1]);
  return bad;
}
