// This file is provided in support of the CS 146 W2020 final assessment.
// All other uses are prohibited unless permission is explicitly granted.
// Republication in any setting by unauthorized parties is prohibited.
//
// Author:  Brad Lushman
// Date:  April 9, 2020
#ifndef _PARSESEXP_H_
#define _PARSESEXP_H_

#include <stdio.h>
#include "scan.h"

enum SExpType {ATOM = 0, LIST, BADSEXP};

struct SExp {
  enum SExpType type;
  union {
    struct Token atom;
    struct SExpListNode *list;
  };
};

struct SExpListNode {
  struct SExp first;
  struct SExpListNode *rest;
};

struct SExp parseSExp(FILE *in);

void printSExp(struct SExp s);
void freeSExp (struct SExp s);

#endif
