// This file is provided in support of the CS 146 W2020 final assessment.
// All other uses are prohibited unless permission is explicitly granted.
// Republication in any setting by unauthorized parties is prohibited.
//
// Author:  Brad Lushman
// Date:  April 9, 2020
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "parsefr.h"

struct Fun *makeFun(char *name, struct FRAST body) {
  struct Fun *result = malloc(sizeof(struct Fun));
  strncpy(result->var, name, MAXLENGTH-1);
  result->var[MAXLENGTH-1] = 0;
  result->body = body;
  return result;
}

struct Bin *makeBin(char op, struct FRAST arg1, struct FRAST arg2) {
  struct Bin *result = malloc(sizeof(struct Bin));
  result->op = op;
  result->arg1 = arg1;
  result->arg2 = arg2;
  return result;
}

struct App *makeApp(struct FRAST fn, struct FRAST arg) {
  struct App *result = malloc(sizeof(struct Bin));
  result->fn = fn;
  result->arg = arg;
  return result;
}

struct Var *makeVar(char *name) {
  struct Var *result = malloc(sizeof(struct Var));
  strncpy(result->name, name, MAXLENGTH-1);
  result->name[MAXLENGTH-1] = 0;
  return result;
}

struct Num *makeNum(int num) {
  struct Num *result = malloc(sizeof(struct Num));
  result->val = num;
  return result;
}

int isKeyword (char *word) {
  return !strcmp(word, "fun") || !strcmp(word, "with");
}

static int length (struct SExpListNode *n) {
  if (!n) return 0;
  return 1 + length(n->rest);
}

struct FRAST doParse(struct SExp sexp) {
  struct FRAST result;
  if (sexp.type == BADSEXP) {
    result.type = BADFR;
    return result;
  }
  if (sexp.type == ATOM) {
    struct Token t = sexp.atom;
    if (t.type == ID) {
      if (isKeyword(t.lexeme)) {
        result.type = BADFR;
        return result;
      }
      result.type = VAR;
      result.v = makeVar(t.lexeme);
      return result;
    }
    else if (t.type == NUM) {
      result.type = NUMBER;
      int val;
      sscanf(t.lexeme, "%d", &val);
      result.n = makeNum(val);
      return result;
    }
    else {
      result.type = BADFR;
      return result;
    }
  }
  if (sexp.type == LIST) {
    int n = length(sexp.list);
    if (n == 0) {
      result.type = BADFR;
      return result;
    }
    struct SExp first = sexp.list->first;
    if (first.type == ATOM) {
      struct Token t = first.atom;
      if (t.type == ID) {
        if (!strcmp(t.lexeme, "fun")) {
          if (n != 3) {
            fprintf(stderr, "Ill-formed 'fun' expression\n");
            result.type = BADFR;
            return result;
          }
          struct SExp second = sexp.list->rest->first;
          if (second.type == ATOM) {
            fprintf(stderr, "Function must have param in parens\n");
            result.type = BADFR;
            return result;
          }
          if (length(second.list) != 1) {
            fprintf(stderr, "Function must have exactly one param\n");
            result.type = BADFR;
            return result;
          }
          struct SExp param = second.list->first;
          if (param.type != ATOM) {
            fprintf(stderr, "Use of list as function param\n");
            result.type = BADFR;
            return result;
          }
          if (isKeyword(param.atom.lexeme)) {
            fprintf(stderr,
              "Use of keyword %s as function param\n", param.atom.lexeme);
            result.type = BADFR;
            return result;
          }
          struct FRAST body = doParse(sexp.list->rest->rest->first);
          if (body.type == BADFR) {
            result.type = BADFR;
            return result;
          }
          result.type = FUN;
          result.f = makeFun(param.atom.lexeme, body);
          return result;
        }
        else if (!strcmp(t.lexeme, "with")) {
          if (n != 3) {
            fprintf(stderr, "Ill-formed 'with' expression\n");
            result.type = BADFR;
            return result;
          }
          struct SExp second = sexp.list->rest->first;
          if (second.type == ATOM) {
            fprintf(stderr, "'with' requires (name value) binding\n");
            result.type = BADFR;
            return result;
          }
          struct SExpListNode *binding = second.list;
          if (length(binding) == 1) { // Allow for double parens
            struct SExp inner = binding->first;
            if (inner.type != LIST) {
              fprintf(stderr, "'with' requires (name value) binding\n");
              result.type = BADFR;
              return result;
            }
            binding = inner.list;
          }
          if (length(binding) != 2) {
            fprintf(stderr, "'with' requires (name value) binding\n");
            result.type = BADFR;
            return result;
          }
          struct SExp var = binding->first;
          if (var.type != ATOM) {
            fprintf(stderr, "'with' must bind an id\n");
            result.type = BADFR;
            return result;
          }
          struct Token t = var.atom;
          if (isKeyword(t.lexeme)) {
            fprintf(stderr, "'with' cannot bind keyword %s\n", t.lexeme);
            result.type = BADFR;
            return result;
          }
          struct FRAST val = doParse(binding->rest->first);
          if (val.type == BADFR) {
            fprintf(stderr, "Error in bound value of 'with' expression\n");
            result.type = BADFR;
            return result;
          }

          struct FRAST body = doParse(sexp.list->rest->rest->first);
          if (body.type == BADFR) {
            freeFR(val);
            fprintf(stderr, "Error in body of 'with' expression\n");
            result.type = BADFR;
            return result;
          }

          struct FRAST fun;
          fun.type = FUN;
          fun.f = makeFun(t.lexeme, body);

          result.type = APP;
          result.a = makeApp(fun, val);
          return result;
        }
        else {
          if (n != 2) {
            fprintf(stderr, "Function application takes exactly one arg\n");
            result.type = BADFR;
            return result;
          }
          struct FRAST fn = doParse(sexp.list->first);
          if (fn.type == BADFR) {
            fprintf(stderr, "Error in function of function application\n");
            result.type = BADFR;
            return result;
          }
          struct FRAST arg = doParse(sexp.list->rest->first);
          if (arg.type == BADFR) {
            freeFR(fn);
            fprintf(stderr, "Error in argument of function application\n");
            result.type = BADFR;
            return result;
          }
          result.type = APP;
          result.a = makeApp(fn, arg);
          return result;
        }
      }
      else if (t.type == NUM) {
        fprintf(stderr, "Cannot hava a number in function position\n");
        result.type = BADFR;
        return result;
      }
      else if (t.type == OP) {
        if (n != 3) {
          fprintf(stderr, "Binary operation takes exactly two args\n");
          result.type = BADFR;
          return result;
        }
        char op = t.lexeme[0];
        struct FRAST arg1 = doParse(sexp.list->rest->first);
        if (arg1.type == BADFR) {
          fprintf(stderr, "Error in argument 1 of binary operation\n");
          result.type = BADFR;
          return result;
        }
        struct FRAST arg2 = doParse(sexp.list->rest->rest->first);
        if (arg2.type == BADFR) {
          freeFR(arg1);
          fprintf(stderr, "Error in argument 2 of binary operation\n");
          result.type = BADFR;
          return result;
        }
        result.type = BIN;
        result.b = makeBin(op, arg1, arg2);
        return result;
      }
    }
    else { // first.type == LIST
      if (n != 2) {
        fprintf(stderr, "Function application takes exactly one arg\n");
        result.type = BADFR;
        return result;
      }
      struct FRAST fn = doParse(sexp.list->first);
      if (fn.type == BADFR) {
        fprintf(stderr, "Error in function of function application\n");
        result.type = BADFR;
        return result;
      }
      struct FRAST arg = doParse(sexp.list->rest->first);
      if (arg.type == BADFR) {
        freeFR(fn);
        fprintf(stderr, "Error in argument of function application\n");
        result.type = BADFR;
        return result;
      }
      result.type = APP;
      result.a = makeApp(fn, arg);
      return result;
    }
  }
}

struct FRAST parseFR(FILE *in) {
  struct SExp sexp = parseSExp(in);
  struct FRAST result =  doParse(sexp);
  freeSExp(sexp);
  return result;
}

void printFR(struct FRAST expr) {
  switch(expr.type) {
   case VAR:
    printf("Var(%s)", expr.v->name);
    return;
   case FUN:
    printf("Fun(%s,", expr.f->var);
    printFR(expr.f->body);
    printf(")");
    return;
   case APP:
    printf("App(");
    printFR(expr.a->fn);
    printf(",");
    printFR(expr.a->arg);
    printf(")");
    return;
   case BIN:
    printf("Bin(%c,", expr.b->op);
    printFR(expr.b->arg1);
    printf(",");
    printFR(expr.b->arg2);
    printf(")");
    return;
   case NUMBER:
    printf("Num(%d)", expr.n->val);
    return;
   case BADFR:
    printf("Invalid Faux Racket Expression");
  }
}

void freeFR(struct FRAST expr) {
  switch(expr.type) {
   case VAR:
    free(expr.v);
    return;
   case FUN:
    freeFR(expr.f->body);
    free(expr.f);
    return;
   case APP:
    freeFR(expr.a->fn);
    freeFR(expr.a->arg);
    free(expr.a);
    return;
   case BIN:
    freeFR(expr.b->arg1);
    freeFR(expr.b->arg2);
    free(expr.b);
    return;
   case NUMBER:
    free(expr.n);
  }
}
