#ifndef ORDGEE_H
#define ORDGEE_H

//#include "tnt/region1d.h"
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

double odds2p11(double psi, double mu1, double mu2);

DMatrix odds2p11(DVector &Psi, DVector &Mu1, DVector &Mu2);

double p11_odds(double psi, double mu1, double mu2);

DVector p11_mu(double psi, double mu1, double mu2);

DVector p11_odds(DVector &Psi, DVector &Mu1, DVector &Mu2);

DMatrix Vijj(DVector &Mu, bool rev);

DMatrix Vijk(DVector &Mu1, DVector &Mu2, DVector &Psi);

DMatrix getU3_Beta(DVector &Mu1, DVector &Mu2, DVector &Psi,
		   DMatrix &D1, DMatrix &D2, 
		   DVector &PR1, DVector &PR2);

DMatrix ord2V1(DVector &Mu, DVector &Psi, int clusz, bool rev);
DMatrix Mu2V1(DVector &Mu, int clusz, bool rev);

void ord_prep_beta(DVector &Y, DMatrix &X, DVector &Offset,
		   DMatrix &Z, DVector &Ooffset,
		   Index1D &I, Index1D &J,
		   int clusz, int c, bool rev,
		   IVector &LinkWave, 
		   GeeParam &par, GeeStr &geestr, Corr &cor,
		   //output
		   DMatrix &Di, DVector &PRi, DMatrix &Vi);

double update_beta(DVector &Y, DMatrix &X, DVector &Offset, DVector &Ooffset,
		   DVector &W, IVector &LinkWave, //DVector &CorP,
		   DMatrix &Z,  IVector &Clusz, int c, bool rev,
		   //IVector &ZcorSize, IVector &Jack, 
		   GeeParam &par, GeeStr &geestr, Corr &cor);

DVector kronecker(const DVector &v1, const DVector &v2);

DVector vec(const DMatrix &m);

DMatrix ESSTijk(DVector &Mu1, DVector &Mu2, DMatrix &P11,
		int c1, int c3, bool rev);

DMatrix ESST(DVector &Mu1, DVector &Mu2, DMatrix &P11, bool rev);

void ord_prep_alpha(DVector &PR1, DVector &PR2, //DMatrix &V,
		    DVector &Mu1, DVector &Mu2, 
		    //c^2 x 1       c x 1           c x 1
		    DMatrix &Z, DVector &Ooffset,
		    bool rev, GeeParam &par, GeeStr &geestr,
		    //output
		    DVector &U2, DMatrix &V2, DMatrix &D2);

double update_alpha(DVector &PR, DVector &Mu, DVector &W,
		    DMatrix &Z, DVector &Ooffset, 
		    IVector &Clusz, int c, bool rev,
		    GeeParam &par, GeeStr &geestr, Corr &cor);

void ordgee_est(DVector &Y, DMatrix &X, 
		DVector &Offset, DVector &Ooffset, DVector &W, 
		IVector &LinkWave,
		DMatrix &Z, IVector &Clusz, int c, bool rev, 
		GeeStr &geestr, Corr &cor, GeeParam &par,
		Control &con);

void HiandGi(DVector &Y, DMatrix &X, DVector &Offset, DVector &Ooffset,
	     IVector &LinkWave, 
	     DMatrix &Z,  int s1, int c, bool rev,
	     Index1D &I, Index1D &J,
	     GeeParam &par, GeeStr &geestr, Corr &cor,
	     //output
	     Hess &Hi, Grad &Gi);

void HnandGis(DVector &Y, DMatrix &X, DVector &Offset, DVector &Ooffset,
	      IVector &LinkWave, DMatrix &Z,  
	      IVector &Clusz, int c, bool rev,
	      GeeParam &par, GeeStr &geestr, Corr &cor, IVector &Scur,
	      Hess &Hn, Vector<Grad> &Gis);

void HnandGis(DVector &Y, DMatrix &X, DVector &Offset, DVector &Ooffset,
	      IVector &LinkWave, DMatrix &Z,  
	      IVector &Clusz, int c, bool rev,
	      GeeParam &par, GeeStr &geestr, Corr &cor,
	      Hess &Hn, Vector<Grad> &Gis);


extern "C" {
  SEXP ordgee_rap(SEXP y, SEXP x, SEXP offset, SEXP doffset, SEXP w,
		  SEXP linkwave, SEXP z, SEXP clusz, SEXP ncat, SEXP rev,
		  SEXP geestr, SEXP cor, SEXP par, SEXP con);
}


#endif //ORDGEE_H
