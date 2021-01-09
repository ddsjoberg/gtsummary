#ifndef INTER_H
#define INTER_H

// extern "C" {
#include <R.h>
#include <Rdefines.h>
// }

#include "tntsupp.h"
#include "geese.h"
#include "famstr.h"
#include "param.h"
#include "geesubs.h"

DMatrix asDMatrix(SEXP a);

DVector asDVector(SEXP a);

IVector asIVector(SEXP a);

Vector<DVector> asVDVector(SEXP a);

SEXP asSEXP(const DMatrix &a);

SEXP asSEXP(const DVector &a);

SEXP asSEXP(const IVector &a);

SEXP asSEXP(const Vector<DVector> &a);


Control asControl(SEXP con);

GeeParam asGeeParam(SEXP par);

GeeStr asGeeStr(SEXP geestr);

Corr asCorr(SEXP cor);

SEXP asSEXP(GeeParam &Par);

#endif  //INTER_H
