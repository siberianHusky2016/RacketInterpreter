// This file is provided in support of the CS 146 W2020 final assessment.
// All other uses are prohibited unless permission is explicitly granted.
// Republication in any setting by unauthorized parties is prohibited.
//
// Author:  Brad Lushman
// Date:  April 9, 2020
#ifndef _SCAN_H_
#define _SCAN_H_

#include <stdio.h>

enum TokenType {LPAREN=0, RPAREN, LBRACK, RBRACK, ID, NUM, OP, DONE, ERROR};

enum {MAXLENGTH = 101};

struct Token {
  enum TokenType type;
  char lexeme[MAXLENGTH];
};

struct Token scan(FILE *in);

void printToken(struct Token t);

#endif
