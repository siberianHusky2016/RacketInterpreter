// This file is provided in support of the CS 146 W2020 final assessment.
// All other uses are prohibited unless permission is explicitly granted.
// Republication in any setting by unauthorized parties is prohibited.
//
// Author:  Brad Lushman
// Date:  April 9, 2020

// Place your solution to problem 3 in this file.

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "parsefr.h"

enum CONTType {EMPTY = 0, APPL, APPR, BINL, BINR};

struct AppL;
struct AppR;
struct BinL;
struct BinR;

// data Cont = MT | AppL Ast Cont | AppR Val Cont
//           | BinL Op Ast Cont | BinR Op Val Cont
struct CONT {
  enum CONTType type;
  union {
    struct AppL *al;
    struct AppR *ar;
    struct BinL *bl;
    struct BinR *br;
  };
};

struct AppL {
  struct FRAST Ast;
  struct CONT Cont;
};
struct AppR {
  struct FRAST Val;
  struct CONT Cont;
};
struct BinL {
  char op;
  struct FRAST Ast;
  struct CONT Cont;};
struct BinR {
  char op;
  struct FRAST Val;
  struct CONT Cont;
};

struct Num *makeNum(int num);

void *interp();
void *applyCount();
struct FRAST copyFR(struct FRAST expr);
struct FRAST subst(struct FRAST v, char fp[], struct FRAST expr);
void trampoline(void *(*f)());


struct FRAST globalExpr;
struct CONT globalCont;
int k = 0;
int g = 0;

int main() {

  globalCont.type = EMPTY;
  globalExpr = parseFR(stdin);


  // printf("===============================\n");
  // printFR(globalExpr);
  // printf("\n===============================\n");

  trampoline(interp);
  printFR(globalExpr);
  printf("\n");
  // printf("===============================\n");
  freeFR(globalExpr);


}



// interp x c = applyCont c x
void *interp() {
  // printf("\nInterp\n");
  struct FRAST resultExpr;
  struct CONT resultCont;
  if (globalExpr.type == BIN){
    // interp (Bin op x y) c = interp x (BinL op y c)
    // created updated AST and Cont
    resultExpr = globalExpr.b->arg1;                          // Configures resultExpr
    resultCont.type = BINL;                                   // Sets resultCont type
    struct BinL *newBinL = malloc(sizeof(struct BinL));
    struct FRAST newArg2 = globalExpr.b->arg2;
    char newOp = globalExpr.b->op;
    newBinL->op = newOp;                                      // set resultCont op
    newBinL->Ast = newArg2;                                   // set resultCont ast
    newBinL->Cont = globalCont;                               // set resultCont cont
    resultCont.bl = newBinL;                                  // attaches resultCont
    
    free(globalExpr.b);                                       // might be sourse of problem

    globalExpr = resultExpr;
    globalCont = resultCont;                                  // sets global variables
    return interp;
  } else if (globalExpr.type == APP) {
    // interp (App f x) c = interp f (AppL x c)
    resultExpr = globalExpr.a->fn;                            // new Expr is f, all arguments of old Expr copied
    struct AppL *newAppL = malloc(sizeof(struct AppL));       // new Cont is of type AppL
    newAppL->Ast = globalExpr.a->arg;                                     
    newAppL->Cont = globalCont;                               // set values of resultCont
    resultCont.type = APPL;
    resultCont.al = newAppL;                                
    free(globalExpr.a);
    globalExpr = resultExpr;
    globalCont = resultCont;                                  // sets global variables
    return interp;
  } else {
    return applyCount;
  }
  
}


// applyCont :: Cont -> Val -> Val
// applyCont (BinL op y k) x = interp y (BinR op x k)
// applyCont (BinR Plus (Numb x) k) (Numb y) = applyCont k (Numb (x+y))
// applyCont (BinR Times (Numb x) k) (Numb y) = applyCont k (Numb (x*y))
// applyCont (AppL a k) f = interp a (AppR f k)
// applyCont (AppR (Fun fp fb) k) x = interp (subst fp x fb) k
// applyCont MT x = x

void *applyCount(){
  // printf("\napplyCont\n");
  struct FRAST resultExpr;
  struct CONT resultCont;
  switch(globalCont.type){
    case BINL:;
      //            globalCont  expr      expr  globalCont
      // applyCont (BinL op y k) x = interp y (BinR op x k)
      struct BinR *newBinR = malloc(sizeof(struct BinR));
      newBinR->op = globalCont.bl->op;
      newBinR->Val = globalExpr;
      newBinR->Cont = globalCont.bl->Cont;
      resultCont.type = BINR;
      resultCont.br = newBinR;
      resultExpr = globalCont.bl->Ast;
      free(globalCont.bl);
      globalExpr = resultExpr;
      globalCont = resultCont;
      return interp;
    case BINR:;
      //                 globalCont         expr            globalCont expr 
      // applyCont (BinR Plus (Numb x) k) (Numb y) = applyCont k (Numb (x+y))
      // applyCont (BinR Times (Numb x) k) (Numb y) = applyCont k (Numb (x*y))

      // defines (Numb (x+y)) / (Numb (x*y))
      int returnInt;
      if (globalCont.br->op == '+') returnInt = globalCont.br->Val.n->val + globalExpr.n->val;
      else if (globalCont.br->op == '*') returnInt = globalCont.br->Val.n->val * globalExpr.n->val;

      struct Num *newNum = makeNum(returnInt);
      resultCont = globalCont.br->Cont;                   // Sets return Cont as k
      resultExpr.type = NUMBER;
      resultExpr.n = newNum;                              // Sets return Expr as (Numb (x+y)) / (Numb (x*y))

      freeFR(globalCont.br->Val);
      free(globalExpr.n);
      free(globalCont.br);

      globalExpr = resultExpr;
      globalCont = resultCont;
      return applyCount;
    case APPL:;
      //          globalCont expr      expr  globalCont
      // applyCont (AppL a k) f = interp a (AppR f k)
      struct AppR *newAppR = malloc(sizeof(struct AppR));
      newAppR->Val = globalExpr;
      newAppR->Cont = globalCont.al->Cont;
      resultCont.type = APPR;
      resultCont.ar = newAppR;
      resultExpr = globalCont.al->Ast;
      free(globalCont.al);
      globalExpr = resultExpr;
      globalCont = resultCont;
      return interp;
    case APPR:
      //                globalCont    expr              expr  globalCont
      // applyCont (AppR (Fun fp fb) k) x = interp (subst x fp fb) k
      resultExpr = subst(globalExpr , globalCont.ar->Val.f->var , globalCont.ar->Val.f->body);
      resultCont = globalCont.ar->Cont;
      freeFR(globalExpr);
      freeFR(globalCont.ar->Val);
      free(globalCont.ar);
      globalExpr = resultExpr;
      globalCont = resultCont;
      return interp;
    case EMPTY:
      return NULL;
  }
}

struct FRAST copyFR(struct FRAST expr) {
  struct FRAST result;
  switch(expr.type) {
   case VAR:
    result.type = VAR;
    struct Var *returnVar = malloc(sizeof(struct Var));
    strcpy(returnVar->name, expr.v->name);
    result.v = returnVar;
    return result;
   case FUN:
    result.type = FUN;
    struct Fun *returnFun = malloc(sizeof(struct Fun));
    strcpy(returnFun->var, expr.f->var);
    struct FRAST returnFunBody = copyFR(expr.f->body);
    returnFun->body = returnFunBody;
    result.f = returnFun;
    return result;
   case APP:
    result.type = APP;
    struct App *returnApp = malloc(sizeof(struct App));
    struct FRAST returnAppFn = copyFR(expr.a->fn);
    struct FRAST returnAppArg = copyFR(expr.a->arg);
    returnApp->fn = returnAppFn;
    returnApp->arg = returnAppArg;
    result.a = returnApp;
    return result;
   case BIN:
    result.type = BIN;
    struct Bin *returnBin = malloc(sizeof(struct Bin));
    returnBin->op = expr.b->op;
    struct FRAST returnBinA = copyFR(expr.b->arg1);
    struct FRAST returnBinB = copyFR(expr.b->arg2);
    returnBin->arg1 = returnBinA;
    returnBin->arg2 = returnBinB;
    result.b = returnBin;
    return result;
   case NUMBER:
    result.type = NUMBER;
    struct Num *returnNum = malloc(sizeof(struct Num));
    returnNum->val = expr.n->val;
    result.n = returnNum;
    return result;
  }
}

struct FRAST subst(struct FRAST v, char fp[], struct FRAST expr){ // Value, function parameter, function body

  // printf("\n===============================\n");     // Debug printing module
  // printf("\n Performing Substitution With: ");
  // printf("\n Val = ");
  // printFR(v);
  // printf("\n Fp = %s", fp);
  // printf("\n Fb = ");
  // printFR(expr);
  // printf("\n===============================\n");

  switch(expr.type) {
   case NUMBER:;
    struct FRAST resultNumber = copyFR(expr);
    return resultNumber;
   case VAR:;
    if (strcmp(fp, expr.v->name) == 0) {
      struct FRAST resultVar1 = copyFR(v);
      return resultVar1;
    } else {
      struct FRAST resultVar2 = copyFR(expr);
      return resultVar2;
    }
   case BIN:;
    struct FRAST result;
    result.type = BIN;
    struct Bin *returnBin = malloc(sizeof(struct Bin));
    struct FRAST fst = subst(v, fp, expr.b->arg1);
    struct FRAST snd = subst(v, fp, expr.b->arg2);
    returnBin->op = expr.b->op;
    returnBin->arg1 = fst;
    returnBin->arg2 = snd;
    result.b = returnBin;
    return result; 
   case APP:;
    struct FRAST resultApp;
    resultApp.type = APP;
    struct App *returnApp = malloc(sizeof(struct App));
    returnApp->fn = subst(v, fp, expr.a->fn);
    returnApp->arg = subst(v, fp, expr.a->arg);
    resultApp.a = returnApp;
    return resultApp;
   case FUN:;
    struct FRAST resultFun;
    resultFun.type = FUN;
    struct Fun *returnFun = malloc(sizeof(struct Fun));     // initialize return function
    strcpy(returnFun->var, expr.f->var);                    // places return function variable
    struct FRAST returnFunBody;
    if (strcmp(fp, expr.f->var) == 0) {
      returnFunBody = copyFR(expr.f->body);
      returnFun->body = returnFunBody;                      // places return function argument
    } else {
      returnFunBody = subst(v, fp, expr.f->body);           // places return function argument
      returnFun->body = returnFunBody;
    }
    resultFun.f = returnFun;                                // places return function into result FRAST
    return resultFun;
   case BADFR:
    printf("Invalid Faux Racket Expression, Failed At Substitution");
  }
}

void trampoline(void *(*f)()) {
  while (f) {
    void *g = f();
    f = (void *(*)())g;
  }
}