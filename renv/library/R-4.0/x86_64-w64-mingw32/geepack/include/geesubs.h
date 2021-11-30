#ifndef GEESUBS_H
#define GEESUBS_H

#include "tntsupp.h"
#include "geese.h"
#include "famstr.h"
#include "param.h"
//#include "lgtdl.h"

class Grad{
protected:
  DVector U1_, U2_, U3_;
public:
  Grad(int p, int r, int q) {
    DVector U1(p), U2(r), U3(q);
    U1_ = U1; U2_ = U2; U3_ = U3;
  }
  Grad(GeeParam &par) {
    int p = par.p(), q = par.q(), r = par.r();
    DVector U1(p), U2(r), U3(q);
    U1_ = U1; U2_ = U2; U3_ = U3;
  }
  Grad() {
    Grad(0, 0, 0);
  }
  ~Grad() {}
  Grad & operator=(const Grad &G);
  void set_U1(const DVector &u) {U1_ = u;}
  void set_U2(const DVector &u) {U2_ = u;}
  void set_U3(const DVector &u) {U3_ = u;}
  DVector U1() const {return U1_;}
  DVector U2() const {return U2_;}
  DVector U3() const {return U3_;}
  friend ostream& operator<<(ostream&, const Grad&);
};

class Hess{
protected:
  DMatrix A_, B_, C_, D_, E_, F_;
public:
  Hess(DMatrix &A, DMatrix &B, DMatrix &C, 
       DMatrix &D, DMatrix &E, DMatrix &F)
    : A_(A), B_(B), C_(C), D_(D), E_(E), F_(F) {}
  Hess(int p, int r, int q) {
    DMatrix A(p,p), B(r,p), C(r,r), D(q,p), E(q,r), F(q,q);
    A_ = A; B_ = B; C_ = C; D_ = D; E_ = E; F_ = F;
  }
  Hess(GeeParam &par) {
    int p = par.p(), q = par.q(), r = par.r();
    DMatrix A(p,p), B(r,p), C(r,r), D(q,p), E(q,r), F(q,q);
    A_ = A; B_ = B; C_ = C; D_ = D; E_ = E; F_ = F;
  }
  Hess(const Hess &H):  
    A_(H.A()), B_(H.B()), C_(H.C()),
    D_(H.D()), E_(H.E()), F_(H.F()) {
    //A_ = H.A(); B_ = H.B(); C_ = H.C();
    //D_ = H.D(); E_ = H.E(); F_ = H.F();
  }
  Hess() {
    Hess(0, 0, 0);
  }
  ~Hess() {}
  DMatrix A() const {return A_;}
  DMatrix B() const {return B_;}
  DMatrix C() const {return C_;}
  DMatrix D() const {return D_;}
  DMatrix E() const {return E_;}
  DMatrix F() const {return F_;}
  void set_A(const DMatrix &a) {A_ = a;}
  void set_B(const DMatrix &b) {B_ = b;}
  void set_C(const DMatrix &c) {C_ = c;}
  void set_D(const DMatrix &d) {D_ = d;}
  void set_E(const DMatrix &e) {E_ = e;}
  void set_F(const DMatrix &f) {F_ = f;}
  void inc_A(const DMatrix &a) {A_ = A_ + a;}
  void inc_B(const DMatrix &b) {B_ = B_ + b;}
  void inc_C(const DMatrix &c) {C_ = C_ + c;}
  void inc_D(const DMatrix &d) {D_ = D_ + d;}
  void inc_E(const DMatrix &e) {E_ = E_ + e;}
  void inc_F(const DMatrix &f) {F_ = F_ + f;}
  void inc(Hess &H) {
    inc_A(H.A()); inc_B(H.B()); inc_C(H.C());
    inc_D(H.D()); inc_E(H.E()); inc_F(H.F());
  }
  void dec_A(const DMatrix &a) {A_ = A_ - a;}
  void dec_B(const DMatrix &b) {B_ = B_ - b;}
  void dec_C(const DMatrix &c) {C_ = C_ - c;}
  void dec_D(const DMatrix &d) {D_ = D_ - d;}
  void dec_E(const DMatrix &e) {E_ = E_ - e;}
  void dec_F(const DMatrix &f) {F_ = F_ - f;}
  void dec(Hess &H) {
    dec_A(H.A()); dec_B(H.B()); dec_C(H.C());
    dec_D(H.D()); dec_E(H.E()); dec_F(H.F());
  }
  Hess& operator=(const Hess &H) {
    A_ = H.A(); B_ = H.B(); C_ = H.C();
    D_ = H.D(); E_ = H.E(); F_ = H.F();
    return *this;
  }
  friend ostream& operator<<(ostream&, const Hess&);
};

Hess operator-(Hess &H1, Hess &H2);

Hess inv(Hess &H, IVector &level);

Hess operator*(const double &x, const Hess &H);

ostream& operator<<(ostream& s, const Hess &H);

DVector genzi(const DVector &PR);

DVector utri(const DMatrix &R);

DMatrix getZ_Beta(DMatrix &D, DVector &PR, 
		  DVector &V, DVector &V_Mu, DVector &z);
      
DMatrix getZ_Gamma(DMatrix &D, DVector &PR, DVector &Phi, DVector &z);

DMatrix getS_Beta(DMatrix &D, DVector &PR, DVector &V, DVector &V_Mu);

void HiandGi(DVector &PRi, DVector &Phii, DMatrix &Di, DMatrix &R,
	     DVector &Vi, DVector &V_Mui, DMatrix &D2i, DMatrix &E,
	     DVector &Wi, IVector &level,
	     //output
	     Hess &H, Grad &G);

void PRandD(DVector &Y, DMatrix &X, DVector &Offset, 
	    Index1D &I, IVector &LinkWave, 
	    GeeParam &par, GeeStr &geestr,
	    DVector &PRi, DMatrix &Di);

void gee_prep(DVector &Y, DMatrix &X, DVector &Offset,
	      Index1D &I, IVector &LinkWave, 
	      GeeParam &par, GeeStr &geestr,
	      DVector &PRi, DMatrix &Di, DVector &Vi, DVector &V_Mui);

DMatrix getR(DMatrix &Zmat, Index1D &I, Index1D &J, DVector &CorP,
	     GeeParam &par, GeeStr &geestr, Corr &cor);

int RandE(DMatrix &Zmat, Index1D &I, Index1D &J, DVector &CorP,
	  GeeParam &par, GeeStr &geestr, Corr &cor,
	  DMatrix &R, DMatrix &E);

void gm_prep(DVector &PR, Index1D &I, IVector &LinkWave,
	     DVector &Doffset, DMatrix &Zsca, GeeParam &par, GeeStr &geestr,
	     DVector &Phii, DVector &Si, DMatrix &D2i);

void PhiandD2(Index1D &I, IVector &LinkWave,
	      DVector &Doffset, DMatrix &Zsca, GeeParam &par, GeeStr &geestr,
	      DVector &Phii, DMatrix &D2i);

DVector getPR(DVector &Y, DMatrix &X, DVector &Offset, IVector &LinkWave,
	      GeeParam &par, GeeStr &geestr);

DVector getPhi(DVector &Doffset, DMatrix &Zsca, IVector &LinkWave,
	       GeeParam &par, GeeStr &geestr);

void HnandGis(DVector &Ycur, DMatrix &X, 
	      DVector &Offset, DVector &Doffset, DVector &W, 
	      IVector &LinkWave, IVector &Clusz, IVector &ZcorSize,
	      DMatrix &Zsca, DMatrix &Zcor, DVector &CorP,
	      GeeParam &par, GeeStr &geestr, Corr &cor,
	      IVector &Scur, IVector &level,
	      //output
	      Hess &H, Vector<Grad> &Gis);
void HisandGis(DVector &Ycur, DMatrix &X, 
	      DVector &Offset, DVector &Doffset, DVector &W, 
	      IVector &LinkWave, IVector &Clusz, IVector &ZcorSize,
	      DMatrix &Zsca, DMatrix &Zcor, DVector &CorP,
	      GeeParam &par, GeeStr &geestr, Corr &cor,
	      IVector &Scur, IVector &level,
	      //output
	       Vector<Hess> &His, Vector<Grad> &Gis);
/*
DVector interpprev(double t, Vector<DVector> &VV, DVector &tis);
double interpprev(double t, DVector &v, DVector &tis);
DVector getY(double t, DVector &Yall);
IVector getS(double t, DVector &S);

void AandCis(Vector<Lgtdl> &Yall, DMatrix &X, 
	     Vector<DVector> &Offset, Vector<DVector> &Doffset,
	     Vector<DVector> &Weight,
	     IVector &LinkWave, 
	     IVector &Clusz, IVector &ZcorSize,
	     DMatrix &Zsca, DMatrix &Zcor, DVector &CorP,
	     Vector<DVector> &Beta, 
	     Vector<DVector> &Gamma, 
	     Vector<DVector> &Alpha,
	     DVector &W, DVector &S, DVector &Tis, DVector &Tlim,
	     int l, DVector &Ita,
	     GeeStr &geestr, Corr &cor, 
	     int ndivs, int fgconf,
	     //output:
	     DMatrix &A, Vector<DVector> &Cis);
*/


/* 
This fix was suggested by 
Jeffrey Horner <jeffrey.horner@vanderbilt.edu>
and Cole Beck <cole.beck@vanderbilt.edu>.
Cole Beck's email on Jan. 4, 2012 explains why:

I believe the function definition for the template function "Valid" (see geesubs.cc) should actually be in the header file.  Taken from http://www.cplusplus.com/doc/tutorial/templates/:

Because templates are compiled when required, this forces a restriction for multi-file projects: the implementation (definition) of a template class or function must be in the same file as its declaration. That means that we cannot separate the interface in a separate header file, and that we must include both interface and implementation in any file that uses the templates.
*/

//get the valid components in X by valid indicator VI
template<class T>
Vector<T> Valid(Vector<T> &X, IVector &VI) {
 int l = sum(VI), k = 1;
 Vector<T> ans(l);
 for (int i = 1; i <= VI.dim(); i++) {
   if (VI(i) == 1) ans(k++) = X(i);
 }
 return ans;
}

template<class T>
Fortran_Matrix<T> Valid(Fortran_Matrix<T> &X, IVector &VI) {
 int l = sum(VI), k = 1, nc = X.num_cols();
 Fortran_Matrix<T> ans(l, nc);
 for (int i = 1; i <= VI.dim(); i++) {
   if (VI(i) == 1) {
     for (int j = 1; j <= nc; j++) ans(k, j) = X(i, j);
     k++;
   }
 }
 return ans;
}

IVector genVI(IVector &Si, int c = 1);

IVector genCrossVI(IVector &Si, int c = 1);

void getDatI(DVector &Y, DVector &Offset, DVector &Doffset,    
	     DVector &W, DVector &CorP, 
	     DMatrix &X, DMatrix &Zsca, DMatrix &Zcor,
	     IVector &LinkWave, 
	     //extract indicator
	     Index1D &I, Index1D &J, IVector Scuri,
	     Corr &cor,
	     //output
	     DVector &VYi, DVector &VOffseti, DVector &VDoffseti, 
	     DVector &VWi, DVector &VCorPi,
	     DMatrix &VXi, DMatrix &VZscai, DMatrix &VZcori,
	     IVector &VLinkWavei);
 
#endif //GEESUBS_H
