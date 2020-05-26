// This file is provided in support of the CS 146 W2020 final assessment.
// All other uses are prohibited unless permission is explicitly granted.
// Republication in any setting by unauthorized parties is prohibited.
//
// Author:  Brad Lushman
// Date:  April 9, 2020
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include "parsesexp.h"

static struct SExpListNode BadList;

struct SExpListNode *cons(struct SExp first, struct SExpListNode *rest) {
  struct SExpListNode *result = malloc(sizeof(struct SExpListNode));
  result->first = first;
  result->rest = rest;
  return result;
}

void printSExpList(struct SExpListNode *list);

void printSExp(struct SExp s) {
  if (s.type == BADSEXP) printf("Bad S-expression");
  else if (s.type == ATOM) printToken(s.atom);
  else {
    printf("(");
    printSExpList(s.list);
    printf(")");
  }
}

void printSExpList(struct SExpListNode *list) {
  if (list) {
    printSExp(list->first);
    if (list->rest) printf(" ");
    printSExpList(list->rest);
  }
}

void freeSExpList(struct SExpListNode *list);

void freeSExp (struct SExp s) {
  if (s.type == LIST) {
    freeSExpList(s.list);
  }
}

void freeSExpList(struct SExpListNode *list) {
  if (list) {
    freeSExp(list->first);
    freeSExpList(list->rest);
    free(list);
  }
}

enum TokenType match (enum TokenType left) {
  if (left == LPAREN) return RPAREN;
  if (left == LBRACK) return RBRACK;
  else return ERROR;
}

struct SExpListNode *parseSExpList(FILE *in, enum TokenType open) {
  struct Token t = scan(in);
  if (t.type == DONE || t.type == ERROR) {
    return &BadList;
  }
  if (t.type == RPAREN || t.type == RBRACK) {
    return (t.type == match(open)) ? NULL : &BadList;
  }
  struct SExp item;
  if (t.type == LPAREN || t.type == LBRACK) {
    struct SExpListNode *first = parseSExpList(in, t.type);
    if (first == &BadList) {
      return &BadList;
    }
    item.type = LIST;
    item.list = first;
  }
  if (t.type == ID || t.type == NUM || t.type == OP) {
    item.type = ATOM;
    item.atom = t;
  }
  struct SExpListNode *rest = parseSExpList(in, open);
  return (rest == &BadList) ?  &BadList : cons(item, rest);
}

struct SExp parseSExp(FILE *in) {
  struct Token t = scan(in);
  struct SExp result;
  if (t.type == RPAREN || t.type == RBRACK || t.type == DONE
      || t.type == ERROR) {
    result.type = BADSEXP;
    return result;
  }
  if (t.type == ID || t.type == NUM || t.type == OP) {
    result.type = ATOM;
    result.atom = t;
    return result;
  }
  if (t.type == LPAREN || t.type == LBRACK) {
    result.type = LIST;
    result.list = parseSExpList(in, t.type);
    if (result.list == &BadList) {
      result.type = BADSEXP;
    }
    return result;
  }
}
