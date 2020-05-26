// This file is provided in support of the CS 146 W2020 final assessment.
// All other uses are prohibited unless permission is explicitly granted.
// Republication in any setting by unauthorized parties is prohibited.
//
// Author:  Brad Lushman
// Date:  April 9, 2020
#ifndef _PARSESIMP_H_
#define _PARSESIMP_H_

#include <stdio.h>
#include "parsesexp.h"

enum AExpType {AEBIN = 0, AENUM, AEID, BADAE};

struct AEBin;
struct AENum;
struct AEId;

struct AEAST {
  enum AExpType type;
  union {
    struct AEBin *b;
    struct AENum *n;
    struct AEId *i;
  };
};

struct AEBin {
  char op;
  struct AEAST arg1, arg2;
};

struct AENum {
  int val;
};

struct AEId {
  char name[MAXLENGTH];
};

struct AEAST parseAE(struct SExp sexp);
void printAE(struct AEAST ae);
void freeAE(struct AEAST ae);

enum BExpType {BECOMPARE = 0, BENOT, BEAND, BEOR, BELIT, BADBE};

struct BECompare;
struct BENot;
struct BEAnd;
struct BEOr;
struct BELit;

struct BEAST {
  enum BExpType type;
  union {
    struct BECompare *c;
    struct BENot *n;
    struct BEAnd *a;
    struct BEOr *o;
    struct BELit *l;
  };
};

struct BECompare {
  char op[3];
  struct AEAST arg1, arg2;
};

struct BENot {
  struct BEAST arg;
};

struct BEAnd {
  struct BEAST arg1, arg2;
};

struct BEOr {
  struct BEAST arg1, arg2;
};

struct BELit {
  int val;
};

struct BEAST parseBE(struct SExp sexp);
void printBE(struct BEAST bexp);
void freeBE(struct BEAST bexp);

enum StmtType {SKIP = 0, SET, SEQ, PRINT, IIF, WHILE, BADSTMT};

struct Skip;
struct Set;
struct Seq;
struct Print;
struct Iif;
struct While;

struct StmtAST {
  enum StmtType type;
  union {
    struct Skip *skip;
    struct Set *set;
    struct Seq *seq;
    struct Print *print;
    struct Iif *iif;
    struct While *loop;
  };
};

struct Skip {};

struct Set {
  char var[MAXLENGTH];
  struct AEAST expr;
};

struct Seq {
  struct StmtAST stmt1;
  struct StmtAST stmt2;
};

struct Print {
  struct AEAST expr;
};

struct Iif {
  struct BEAST test;
  struct StmtAST tstmt;
  struct StmtAST fstmt;
};

struct While {
  struct BEAST test;
  struct StmtAST body;
};

struct StmtAST parseStmt(struct SExp sexp);
void printStmt(struct StmtAST stmt);
void freeStmt(struct StmtAST stmt);

struct Decl {
  char var[MAXLENGTH];
  int val;
};

struct DeclListNode {
  struct Decl decl;
  struct DeclListNode *next;
};

struct Program {
  int valid;
  struct DeclListNode *decls;
  struct StmtAST stmt;
};

// Produces a SIMP Program structure
struct Program parseProgram(FILE *in);

// Prints a SIMP Program structure
void printProgram(struct Program p);

// Frees a SIMP Program structure
void freeProgram (struct Program *p);
#endif
