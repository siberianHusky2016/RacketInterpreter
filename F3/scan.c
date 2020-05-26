// This file is provided in support of the CS 146 W2020 final assessment.
// All other uses are prohibited unless permission is explicitly granted.
// Republication in any setting by unauthorized parties is prohibited.
//
// Author:  Brad Lushman
// Date:  April 9, 2020
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include "scan.h"

struct Token scan (FILE *in) {
  struct Token t;
  char c = getc(in);
  while (isspace(c)) { c = getc(in); }

  if (c == EOF) {
    t.type = DONE;
    return t;
  }
  if (c == '(') {
    t.type = LPAREN;
    sprintf(t.lexeme, "(");
    return t;
  }
  if (c == ')') {
    t.type = RPAREN;
    sprintf(t.lexeme, ")");
    return t;
  }
  if (c == '[') {
    t.type = LBRACK;
    sprintf(t.lexeme, "[");
    return t;
  }
  if (c == ']') {
    t.type = RBRACK;
    sprintf(t.lexeme, "]");
    return t;
  }
  if (c == '+' || c == '*' || c == '/' || c == '>' || c == '<' || c == '=') {
    t.type = OP;
    sprintf(t.lexeme, "%c", c);
    if (c == '>' || c == '<') {
      char next = getc(in);
      if (next == '=') {
        sprintf(t.lexeme + 1, "%c", next);
      }
      else {
        ungetc(next, in);
      }
    }
    return t;
  }
  if (isdigit(c) || c == '-') {
    t.type = NUM;
    int tokenLength = 0;
    t.lexeme[tokenLength++] = c;
    t.lexeme[tokenLength] = 0;

    if (c == '-') {
      c = getc(in);
      ungetc(c, in);
      if (!isdigit(c)) {
        t.type = OP;
        return t;
      }
    }
    c = getc(in);
    while (isdigit(c)) {
      t.lexeme[tokenLength++] = c;
      c = getc(in);
      if (tokenLength == MAXLENGTH - 1) break;
    }
    t.lexeme[tokenLength] = 0;
    ungetc(c, in);
    return t;
  }
  if (isalpha(c)) {
    t.type = ID;
    int tokenLength = 0;
    while (isalpha(c) || isdigit(c)) {
      t.lexeme[tokenLength++] = c;
      c = getc(in);
      if (tokenLength == MAXLENGTH - 1) break;
    }
    t.lexeme[tokenLength] = 0;
    ungetc(c, in);
    return t;
  }
  t.type = ERROR;
  return t;
}

void printToken(struct Token t) {
  switch (t.type) {
   case LPAREN:
    printf("LPAREN(");
    break;
   case RPAREN:
    printf("RPAREN(");
    break;
   case LBRACK:
    printf("LBRACK(");
    break;
   case RBRACK:
    printf("RBRACK(");
    break;
   case NUM:
    printf("NUM(");
    break;
   case ID:
    printf("ID(");
    break;
   case OP:
    printf("OP(");
    break;
   case DONE:
    printf("DONE");
    return;
   case ERROR:
    printf("ERROR");
    return;
  }
  printf("%s)", t.lexeme);
}

