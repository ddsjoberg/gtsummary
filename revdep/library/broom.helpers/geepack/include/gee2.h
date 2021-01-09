#ifndef GEE2_H
#define GEE2_H

#include "tntsupp.h"
#include "geese.h"

// extern "C"{
#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
// }

#include "famstr.h"
#include "param.h"
#include "inter.h"
#include "utils.h"
#include "geesubs.h"

IVector comp_lev(GeeStr &geestr, Corr &cor);

void gee_var(DVector &Y, DMatrix &X, 
	     DVector &Offset, DVector &Doffset, DVector &W,
	     IVector &LinkWave, 
	     DMatrix &Zsca, DMatrix &Zcor, DVector &CorP, 
	     IVector &Clusz, IVector &ZcorSize, 
	     GeeStr &geestr, Corr &cor, GeeParam &par, Control &con);

double update_beta(DVector &Y, DMatrix &X, DVector &Offset, DVector &W,
		   IVector &LinkWave, DVector &CorP,
		   DMatrix &Zcor,  IVector &Clusz,
		   IVector &ZcorSize, IVector &Jack, 
		   GeeParam &par, GeeStr &geestr, Corr &cor);

double update_gamma(DVector &PR, DVector &W, IVector &LinkWave,
		    IVector &Clusz, IVector &Jack,
		    DVector &Doffset, DMatrix &Zsca, 
		    GeeParam &par, GeeStr &geestr);

double update_alpha(DVector &PR, DVector &Phi, DVector &CorP, DVector &W,
		    IVector &Clusz, IVector &ZcorSize, IVector &Jack,
		    DMatrix &Zcor, 
		    GeeParam &par, GeeStr &geestr, Corr &cor);

void gee_est(DVector &Y, DMatrix &X, 
	     DVector &Offset, DVector &Doffset, DVector &W, 
	     IVector &LinkWave,
	     DMatrix &Zsca, DMatrix &Zcor, DVector &CorP,
	     IVector &Clusz, IVector &ZcorSize,
	     GeeStr &geestr, Corr &cor, GeeParam &par,
	     IVector &Jack, Control &con);

void getJackVar(Vector<DVector> &beta_i, Vector<DVector> &alpha_i,
		Vector<DVector> &gamma_i, GeeParam &par, 
		int jack);

void gee_jack(DVector &Y, DMatrix &Xmat, DVector &Offset, DVector &Doffset,
	      DVector &W, IVector &LinkWave, DMatrix &Zsca, DMatrix &Zcor,
	      DVector &CorP, IVector &Clusz, IVector &ZcorSize,
	      GeeStr &geestr, Corr &cor,
	      GeeParam &par, Control &con);

void jack_ajs(DVector &Y, DMatrix &X, DVector &Offset, DVector &Doffset,
	      DVector &W, IVector &LinkWave, DMatrix &Zsca, DMatrix &Zcor,
	      DVector &CorP, IVector &Clusz, IVector &ZcorSize,
	      GeeStr &geestr, Corr &cor,
	      GeeParam &par, Control &con);

void gee_top(DVector &Y, DMatrix &Xmat, 
	     DVector &Offset, DVector &Doffset, DVector &W, 
	     IVector &LinkWave,
	     DMatrix &Zsca, DMatrix &Zcor, DVector &CorP,
	     IVector &Clusz, 
	     GeeStr &geestr, Corr &cor, GeeParam &par,
	     Control &con);

extern "C" {
  SEXP gee_rap(SEXP y, SEXP x, SEXP offset, SEXP doffset, SEXP w,
	       SEXP linkwave, SEXP zsca, SEXP zcor, SEXP corp,
	       SEXP clusz, SEXP geestr, SEXP cor, SEXP par, SEXP con);
}

#endif //GEE2_H
