// This file is provided in support of the CS 146 W2020 final assessment.
// All other uses are prohibited unless permission is explicitly granted.
// Republication in any setting by unauthorized parties is prohibited.
//
// Author:  Brad Lushman
// Date:  April 9, 2020
#ifndef _PARSEFR_H_
#define _PARSEFR_H_

#include <stdio.h>
#include "parsesexp.h"

enum FRASTType {FUN = 0, APP, BIN, VAR, NUMBER, BADFR};

struct FRAST;
struct Fun;
struct App;
struct Bin;
struct Num;
struct Var;

struct FRAST {
  enum FRASTType type;
  union {
    struct Fun *f;
    struct App *a;
    struct Bin *b;
    struct Var *v;
    struct Num *n;
  };
};

struct Fun {
  char var[MAXLENGTH];
  struct FRAST body;
};

struct App {
  struct FRAST fn;
  struct FRAST arg;
};

struct Bin {
  char op;
  struct FRAST arg1, arg2;
};

struct Num {
  int val;
};

struct Var {
  char name[MAXLENGTH];
};

// Produces a Faux Racket AST structure
struct FRAST parseFR(FILE *in);

// Prints a Faux Racket AST structure
void printFR(struct FRAST expr);

// Frees a Faux Racket AST structure
void freeFR(struct FRAST expr);
#endif
