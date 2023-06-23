/*
  General header file for OASYS.
  You'll need oalib.h and oalib.c for the compile & link.
  This code is in the public domain.
*/

#define VERSION " v1.01 (" __DATE__ ")\n"\
                "\tby Russell Wallace changes by Jeff Jenness"

enum {
  T_VOID,
  T_INT,
  T_STR,
  T_OBJECT,
  T_REF = 0x1000
};

enum {
  I_JMP,
  I_JF,
  I_JT,
  I_INT,
  I_OR,
  I_AND,
  I_NOT,
  I_REFLOCALVAR,
  I_REFARG,
  I_REFGLOBALVAR,
  I_REFPROPERTY,
  I_DEREF,
  I_CALLPROC,
  I_CALLFUNC,
  I_ASSIGN,
  I_DESTROY,
  I_CREATE,
  I_EXIT,
  I_QUIT,
  I_THIS,
  I_RETPROC,
  I_RETFUNC,
  I_EXISTS,
  I_OBJECT,
  I_PRINTINT,
  I_PRINTSTR,
  I_SAVE,
  I_LOAD,
  I_MINUS,
  I_ADD,
  I_SUB,
  I_MUL,
  I_DIV,
  I_MOD,
  I_EQ,
  I_NE,
  I_GT,
  I_LT,
  I_GE,
  I_LE,
  I_OEQ,
  I_ONE,
  I_RANDOM,
  I_IS,
  I_NEXT,

  I_BREAK,
  I_CONTINUE,
};
