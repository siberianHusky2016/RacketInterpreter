// This file is provided in support of the CS 146 W2020 final assessment.
// All other uses are prohibited unless permission is explicitly granted.
// Republication in any setting by unauthorized parties is prohibited.
//
// Author:  Brad Lushman
// Date:  April 9, 2020
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parsesimp.h"

static int length(struct SExpListNode *lst) {
  if (!lst) return 0;
  return 1 + length(lst->rest);
}

int isOperator(const char *str) {
  return !strcmp(str, "+") || !strcmp(str, "-") || !strcmp(str, "*")
    || !strcmp(str, "div") || !strcmp(str, "mod");
}

struct AENum *makeAENum(int val) {
  struct AENum *result = malloc(sizeof(struct AENum));
  result->val = val;
  return result;
}

struct AEId *makeAEId(char *name) {
  struct AEId *result = malloc(sizeof(struct AEId));
  strncpy(result->name, name, MAXLENGTH-1);
  result->name[MAXLENGTH-1] = 0;
  return result;
}

struct AEBin *makeAEBin(char op, struct AEAST arg1, struct AEAST arg2) {
  struct AEBin *result = malloc(sizeof(struct AEBin));
  result->op = op;
  result->arg1 = arg1;
  result->arg2 = arg2;
  return result;
}

struct AEAST parseAE(struct SExp sexp) {
  struct AEAST result;
  if (sexp.type == BADSEXP) {
    result.type = BADAE;
    return result;
  }
  if (sexp.type == ATOM) {
    struct Token t = sexp.atom;
    if (t.type == ID) {
      result.type = AEID;
      result.i = makeAEId(t.lexeme);
      return result;
    }
    else if (t.type == NUM) {
      int val;
      sscanf(t.lexeme, "%d", &val);
      result.type = AENUM;
      result.n = makeAENum(val);
      return result;
    }
    else {
      result.type = BADAE;
      return result;
    }
  }
  else if (sexp.type == LIST) {
    struct SExpListNode *lst = sexp.list;
    if (length(lst) != 3) {
      fprintf(stderr, "Ill-formed binary arithmetic expression\n");
      result.type = BADAE;
      return result;
    }
    char op;
    struct SExp first = sexp.list->first;
    struct SExp second = sexp.list->rest->first;
    struct SExp third = sexp.list->rest->rest->first;

    if (first.type != ATOM) {
      fprintf(stderr, "Arithmetic expression must begin with an operator\n");
      result.type = BADAE;
      return result;
    }
    struct Token operator = first.atom;

    if (!isOperator(operator.lexeme)) {
      fprintf(stderr, "Operator must be +, -, *, div, or mod\n");
      result.type = BADAE;
      return result;
    }

    op = operator.lexeme[0];
    if (!strcmp(operator.lexeme, "div")) op = '/';
    if (!strcmp(operator.lexeme, "mod")) op = '%';

    struct AEAST arg1 = parseAE(second);
    if (arg1.type == BADAE) {
      result.type = BADAE;
      return result;
    }
    
    struct AEAST arg2 = parseAE(third);
    if (arg2.type == BADAE) {
      freeAE(arg1);
      result.type = BADAE;
      return result;
    }

    result.type = AEBIN;
    result.b = makeAEBin(op, arg1, arg2);
    return result;
  }
}

void freeAE(struct AEAST ae) {
  switch (ae.type) {
   case AEBIN:
    freeAE(ae.b->arg1);
    freeAE(ae.b->arg2);
    free(ae.b);
    return;
   case AENUM:
    free(ae.n);
    return;
   case AEID:
    free(ae.i);
  }
}

void printAE(struct AEAST ae) {
  switch (ae.type) {
   case AEBIN:
    printf("ABin(%c,", ae.b->op);
    printAE(ae.b->arg1);
    printf(",");
    printAE(ae.b->arg2);
    printf(")");
    return;
   case AENUM:
    printf("ANum(%d)", ae.n->val);
    return;
   case AEID:
    printf("AId(%s)", ae.i->name);
    return;
   case BADAE:
    printf("Ill-formed arithmetic expression");
  }
}

struct BENot *makeBENot(struct BEAST arg) {
  struct BENot *result = malloc(sizeof(struct BENot));
  result->arg = arg;
  return result;
}

struct BECompare *makeBECompare(char *op, struct AEAST arg1, struct AEAST arg2) {
  struct BECompare *result = malloc(sizeof(struct BECompare));
  strcpy(result->op, op);
  result->arg1 = arg1;
  result->arg2 = arg2;
  return result;
}

struct BEAnd *makeBEAnd(struct BEAST arg1, struct BEAST arg2) {
  struct BEAnd *result = malloc(sizeof(struct BEAnd));
  result->arg1 = arg1;
  result->arg2 = arg2;
  return result;
}

struct BEOr *makeBEOr(struct BEAST arg1, struct BEAST arg2) {
  struct BEOr *result = malloc(sizeof(struct BEOr));
  result->arg1 = arg1;
  result->arg2 = arg2;
  return result;
}

struct BELit *makeBELit(int val) {
  struct BELit *result = malloc(sizeof(struct BELit));
  result->val = val;
  return result;
}

struct BEAST parseAnd(struct SExpListNode *lst) {
  struct BEAST result;
  int n = length(lst);

  if (n == 0) {
    result.type = BELIT;
    result.l = makeBELit(1);
    return result;
  }
  else if (n == 1) {
    return parseBE(lst->first);
  }
  else {
    struct BEAST rest = parseAnd(lst->rest);
    if (rest.type == BADBE) {
      result.type = BADBE;
      return result;
    }
    struct BEAST first = parseBE(lst->first);
    if (first.type == BADBE) {
      freeBE(rest);
      result.type = BADBE;
      return result;
    }
    result.type = BEAND;
    result.a = makeBEAnd(first, rest);
    return result;
  }
  return result;
}

struct BEAST parseOr(struct SExpListNode *lst) {
  struct BEAST result;
  int n = length(lst);

  if (n == 0) {
    result.type = BELIT;
    result.l = makeBELit(0);
    return result;
  }
  else if (n == 1) {
    return parseBE(lst->first);
  }
  else {
    struct BEAST rest = parseOr(lst->rest);
    if (rest.type == BADBE) {
      result.type = BADBE;
      return result;
    }
    struct BEAST first = parseBE(lst->first);
    if (first.type == BADBE) {
      freeBE(rest);
      result.type = BADBE;
      return result;
    }
    result.type = BEOR;
    result.o = makeBEOr(first, rest);
    return result;
  }
  return result;
}

struct BEAST parseBE(struct SExp sexp) {
  struct BEAST result;
  if (sexp.type == BADSEXP) {
    result.type = BADBE;
    return result;
  }
  if (sexp.type == ATOM) {
    struct Token t = sexp.atom;
    if (strcmp(t.lexeme, "true") && strcmp(t.lexeme, "false")) {
      fprintf(stderr, "Invalid boolean literal\n");
      result.type = BADBE;
      return result;
    }
    result.type = BELIT;
    result.l = makeBELit(strcmp(t.lexeme, "true") ? 0 : 1);
    return result;
  }
  else if (sexp.type == LIST) {
    struct SExpListNode *lst = sexp.list;
    int n = length(lst);

    if (n < 2) {
      fprintf(stderr, "Ill-formed boolean expression\n");
      result.type = BADBE;
      return result;
    }

    struct SExp first = lst->first;
    if (first.type != ATOM) {
      fprintf(stderr, "Boolean expression must begin with an operator\n");
      result.type = BADBE;
      return result;
    }

    struct Token op = first.atom;

    if (!strcmp(op.lexeme, "not")) {
      if (n != 2) {
        fprintf(stderr, "Negation requires exactly one argument\n");
        result.type = BADBE;
        return result;
      }
      struct SExp operand = lst->rest->first;
      struct BEAST arg = parseBE(operand);
      if (arg.type == BADBE) {
        result.type = BADBE;
        return result;
      }
      result.type = BENOT;
      result.n = makeBENot(arg);
      return result;
    }
    else if (!strcmp(op.lexeme, "and")) {
      return parseAnd(lst->rest);
    }
    else if (!strcmp(op.lexeme, "or")) {
      return parseOr(lst->rest);
    }
    else if (!strcmp(op.lexeme, ">") || !strcmp(op.lexeme, "<")
        || !strcmp(op.lexeme, ">=") || !strcmp(op.lexeme, "<=")
        || !strcmp(op.lexeme, "=")) {
      if (n != 3) {
        fprintf(stderr, "Comparisons require exactly two arguments\n");
        result.type = BADBE;
        return result;
      }
      struct AEAST arg1 = parseAE(lst->rest->first);
      if (arg1.type == BADAE) {
        result.type = BADBE;
        return result;
      }
      struct AEAST arg2 = parseAE(lst->rest->rest->first);
      if (arg2.type == BADAE) {
        freeAE(arg1);
        result.type = BADBE;
        return result;
      }

      result.type = BECOMPARE;
      result.c = makeBECompare(op.lexeme, arg1, arg2);
      return result;
    }
    else {
      fprintf(stderr, "Invalid boolean operator\n");
      result.type = BADBE;
      return result;
    }
  }
}

void printBE(struct BEAST be) {
  switch (be.type) {
   case BECOMPARE:
    printf("BBin(%s,", be.c->op);
    printAE(be.c->arg1);
    printf(",");
    printAE(be.c->arg2);
    printf(")");
    return;
   case BENOT:
    printf("Not(");
    printBE(be.n->arg);
    printf(")");
    return;
   case BEAND:
    printf("And(");
    printBE(be.a->arg1);
    printf(",");
    printBE(be.a->arg2);
    printf(")");
    return;
   case BEOR:
    printf("Or(");
    printBE(be.o->arg1);
    printf(",");
    printBE(be.o->arg2);
    printf(")");
    return;
   case BELIT:
    printf("BLit(%s)", be.l->val?"true":"false");
    return;
   case BADBE:
    printf("Invalid boolean expression");
    return;
  }
}

void freeBE(struct BEAST be) {
  switch (be.type) {
   case BECOMPARE:
    freeAE(be.c->arg1);
    freeAE(be.c->arg2);
    free(be.c);
    return;
   case BENOT:
    freeBE(be.n->arg);
    free(be.n);
    return;
   case BEAND:
    freeBE(be.a->arg1);
    freeBE(be.a->arg2);
    free(be.a);
    return;
   case BEOR:
    freeBE(be.o->arg1);
    freeBE(be.o->arg2);
    free(be.o);
    return;
   case BELIT:
    free(be.l);
    return;
  }
}

struct Seq *makeSeq(struct StmtAST stmt1, struct StmtAST stmt2) {
  struct Seq *result = malloc(sizeof(struct Seq));
  result->stmt1 = stmt1;
  result->stmt2 = stmt2;
  return result;
}

struct Set *makeSet(const char *var, struct AEAST expr) {
  struct Set *result = malloc(sizeof(struct Set));
  strcpy(result->var, var);
  result->expr = expr;
  return result;
}

struct Print *makePrint(struct AEAST expr) {
  struct Print *result = malloc(sizeof(struct Print));
  result->expr = expr;
  return result;
}

struct Iif *makeIif(struct BEAST test, struct StmtAST tstmt, struct StmtAST fstmt) {
  struct Iif *result = malloc(sizeof(struct Iif));
  result->test = test;
  result->tstmt = tstmt;
  result->fstmt = fstmt;
  return result;
}

struct While *makeWhile(struct BEAST test, struct StmtAST body) {
  struct While *result = malloc(sizeof(struct While));
  result->test = test;
  result->body = body;
  return result;
}

static int isKeyword(const char *id) {
  return !strcmp(id, "vars") || !strcmp(id, "skip") || !strcmp(id, "seq")
    || !strcmp(id, "set") || !strcmp(id, "print") || !strcmp(id, "iif")
    || !strcmp(id, "while");
}

struct StmtAST parseSequence (struct SExpListNode *lst) {
  struct StmtAST result = {BADSTMT};

  if (!lst) {
    fprintf(stderr, "List cannot be empty\n");
    return result;
  }

  int n = length(lst);
  if (n == 1) {
    return parseStmt(lst->first);
  }

  struct StmtAST stmt1 = parseStmt(lst->first);
  if (stmt1.type == BADSTMT) {
    return result;
  }

  struct StmtAST stmt2 = parseSequence(lst->rest);
  if (stmt2.type == BADSTMT) {
    freeStmt(stmt1);
    return result;
  }

  result.type = SEQ;
  result.seq = makeSeq(stmt1, stmt2);
  return result;
}

struct StmtAST parseStmt(struct SExp sexp) {
  struct StmtAST result;
  if (sexp.type == BADSEXP || sexp.type == ATOM) {
    result.type = BADSTMT;
    return result;
  }
  struct SExpListNode *lst = sexp.list;
  int n = length(lst);
  if (n == 0) {
    fprintf(stderr, "Empty statement\n");
    result.type = BADSTMT;
    return result;
  }

  struct SExp first = lst->first;

  if (first.type != ATOM) {
    fprintf(stderr, "Statement type cannot be a list\n");
    result.type = BADSTMT;
    return result;
  }

  struct Token t = first.atom;

  if (t.type != ID) {
    fprintf(stderr, "Statement must begin with a keyword\n");
    result.type = BADSTMT;
    return result;
  }

  if (!strcmp(t.lexeme, "skip")) {
    if (n != 1) {
      fprintf(stderr, "Skip statement takes no arguments\n");
      result.type = BADSTMT;
      return result;
    }
    result.type = SKIP;
    result.skip = malloc(sizeof(struct Skip));
    return result;
  }
  else if (!strcmp(t.lexeme, "seq")) {
    if (n != 3) {
      fprintf(stderr, "Seq statement takes two arguments\n");
      result.type = BADSTMT;
      return result;
    }
    struct StmtAST stmt1 = parseStmt(lst->rest->first);
    if (stmt1.type == BADSTMT) {
      result.type = BADSTMT;
      return result;
    }
    struct StmtAST stmt2 = parseStmt(lst->rest->rest->first);
    if (stmt2.type == BADSTMT) {
      freeStmt(stmt1);
      result.type = BADSTMT;
      return result;
    }
    result.type = SEQ;
    result.seq = makeSeq(stmt1, stmt2);
    return result;
  }
  else if (!strcmp(t.lexeme, "set")) {
    if (n != 3) {
      fprintf(stderr, "Set statement takes two arguments\n");
      result.type = BADSTMT;
      return result;
    }

    struct SExp second = lst->rest->first;
    if (second.type != ATOM || second.atom.type != ID) {
      fprintf(stderr, "First arg of set statement must be an ID\n");
      result.type = BADSTMT;
      return result;
    }
    if (isKeyword(second.atom.lexeme)) {
      fprintf(stderr, "Variable of set statement cannot be a keyword\n");
      result.type = BADSTMT;
      return result;
    }

    struct AEAST expr = parseAE(lst->rest->rest->first);
    if (expr.type == BADAE) {
      result.type = BADSTMT;
      return result;
    }
    result.type = SET;
    result.set = makeSet(second.atom.lexeme, expr);
    return result;
  }
  else if (!strcmp(t.lexeme, "print")) {
    if (n != 2) {
      fprintf(stderr, "Print statement takes one argument\n");
      result.type = BADSTMT;
      return result;
    }
    struct AEAST expr = parseAE(lst->rest->first);
    if (expr.type == BADAE) {
      result.type = BADSTMT;
      return result;
    }
    result.type = PRINT;
    result.print = makePrint(expr);
    return result;
  }
  else if (!strcmp(t.lexeme, "iif")) {
    if (n != 4) {
      fprintf(stderr, "Iif statement takes three arguments\n");
      result.type = BADSTMT;
      return result;
    }
    struct BEAST test = parseBE(lst->rest->first);
    if (test.type == BADBE) {
      result.type = BADSTMT;
      return result;
    }
    struct StmtAST tstmt = parseStmt(lst->rest->rest->first);
    if (tstmt.type == BADSTMT) {
      freeBE(test);
      result.type = BADSTMT;
      return result;
    }
    struct StmtAST fstmt = parseStmt(lst->rest->rest->rest->first);
    if (fstmt.type == BADSTMT) {
      freeBE(test);
      freeStmt(tstmt);
      result.type = BADSTMT;
      return result;
    }
    result.type = IIF;
    result.iif = makeIif(test, tstmt, fstmt);
    return result;
  }
  else if (!strcmp(t.lexeme, "while")) {
    if (n < 3) {
      fprintf(stderr, "While statement takes test and body statements\n");
      result.type = BADSTMT;
      return result;
    }
    struct BEAST test = parseBE(lst->rest->first);
    if (test.type == BADBE) {
      result.type = BADSTMT;
      return result;
    }
    struct StmtAST body = parseSequence(lst->rest->rest);
    if (body.type == BADSTMT) {
      freeBE(test);
      result.type = BADSTMT;
      return result;
    }
    result.type = WHILE;
    result.loop = makeWhile(test, body);
    return result;
  }
  else {
    fprintf(stderr, "Statement must begin with a keyword\n");
    result.type = BADSTMT;
    return result;
  }
}

void printStmt(struct StmtAST stmt) {
  switch (stmt.type) {
   case SKIP:
    printf("Skip");
    return;
   case SEQ:
    printf("Seq(");
    printStmt(stmt.seq->stmt1);
    printf(",");
    printStmt(stmt.seq->stmt2);
    printf(")");
    return;
   case SET:
    printf("Set(%s,", stmt.set->var);
    printAE(stmt.set->expr);
    printf(")");
    return;
   case PRINT:
    printf("Print(");
    printAE(stmt.print->expr);
    printf(")");
    return;
   case IIF:
    printf("Iif(");
    printBE(stmt.iif->test);
    printf(",");
    printStmt(stmt.iif->tstmt);
    printf(",");
    printStmt(stmt.iif->fstmt);
    printf(")");
    return;
   case WHILE:
    printf("While(");
    printBE(stmt.loop->test);
    printf(",");
    printStmt(stmt.loop->body);
    printf(")");
    return;
   case BADSTMT:
    printf("Invalid statement");
    return;
  }
}

void freeStmt(struct StmtAST stmt) {
  switch (stmt.type) {
   case SKIP:
    free(stmt.skip);
    return;
   case SEQ:
    freeStmt(stmt.seq->stmt1);
    freeStmt(stmt.seq->stmt2);
    free(stmt.seq);
    return;
   case SET:
    freeAE(stmt.set->expr);
    free(stmt.set);
    return;
   case PRINT:
    freeAE(stmt.print->expr);
    free(stmt.print);
    return;
   case IIF:
    freeBE(stmt.iif->test);
    freeStmt(stmt.iif->tstmt);
    freeStmt(stmt.iif->fstmt);
    free(stmt.iif);
    return;
   case WHILE:
    freeBE(stmt.loop->test);
    freeStmt(stmt.loop->body);
    free(stmt.loop);
    return;
  }
}

struct DeclListNode badDeclList;

struct DeclListNode *parseDeclSequence(struct SExpListNode *lst) {
  if (!lst) return NULL;
  struct SExp decl = lst->first;
  if (decl.type != LIST || length(decl.list) != 2) {
    fprintf(stderr, "Decl must have the form (var value)\n");
    return &badDeclList;
  }
  struct SExp var = decl.list->first;
  if (var.type != ATOM || var.atom.type != ID) {
    fprintf(stderr, "Var part of a decl must be an id\n");
    return &badDeclList;
  }
  if (isKeyword(var.atom.lexeme)) {
    fprintf(stderr, "Var part of a decl cannot be a keyword\n");
    return &badDeclList;
  }
  char *varName = var.atom.lexeme;

  struct SExp val = decl.list->rest->first;
  if (val.type != ATOM || val.atom.type != NUM) {
    fprintf(stderr, "Value part of a decl must be a number\n");
    return &badDeclList;
  }
  struct Decl declResult;
  strcpy(declResult.var, var.atom.lexeme);
  sscanf(val.atom.lexeme, "%d", &declResult.val);

  struct DeclListNode *rest = parseDeclSequence(lst->rest);
  if (rest == &badDeclList) return rest;

  struct DeclListNode *result = malloc(sizeof(struct DeclListNode));
  result->decl = declResult;
  result->next = rest;
  return result;
}

static struct Program doParse(struct SExp sexp) {
  struct Program result = {0};
  if (sexp.type != LIST) return result;
  struct SExpListNode *lst = sexp.list;
  int n = length(lst);
  if (n < 3) {
    fprintf(stderr, "Expected:  (vars [decls] stmts)\n");
    return result;
  }
  if (lst->first.type != ATOM) {
    fprintf(stderr, "Expected:  (vars [decls] stmts)\n");
    return result;
  }
  if (strcmp(lst->first.atom.lexeme, "vars")) {
    fprintf(stderr, "Expected:  (vars [decls] stmts)\n");
    return result;
  }

  struct SExp decls = lst->rest->first;
  if (decls.type != LIST) {
    fprintf(stderr, "Expected:  decls must be a list\n");
    return result;
  }

  struct DeclListNode *declsAST = parseDeclSequence(decls.list);
  if (declsAST == &badDeclList) {
    return result;
  }

  struct StmtAST stmt = parseSequence(lst->rest->rest);
  if (stmt.type == BADSTMT) {
    struct DeclListNode *cur = declsAST;
    while (cur) {
      struct DeclListNode *tmp = cur->next;
      free(cur);
      cur = tmp;
    }
    return result;
  }

  result.valid = 1;
  result.decls = declsAST;
  result.stmt = stmt;
  return result;
}

struct Program parseProgram(FILE *in) {
  struct SExp sexp = parseSExp(in);
  struct Program p = doParse(sexp);
  freeSExp(sexp);
  return p;
}

void printDecls(struct DeclListNode *decls) {
  if (!decls) return;
  printf("(%s,%d)", decls->decl.var, decls->decl.val);
  if (decls->next) {
    printf(",");
    printDecls(decls->next);
  }
}

void printProgram(struct Program p) {
  if (!p.valid) {
    printf("Invalid SIMP program");
    return;
  }
  printf("Vars([");
  printDecls(p.decls);
  printf("],");
  printStmt(p.stmt);
  printf(")");
}

void freeProgram(struct Program *p) {
  if (!p->valid) return;
  struct DeclListNode *cur = p->decls;
  while (cur) {
    struct DeclListNode *tmp = cur->next;
    free(cur);
    cur = tmp;
  }
  freeStmt(p->stmt);
  p->valid = 0;
}
