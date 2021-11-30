#ifndef PARAM_H
#define PARAM_H

#include "tntsupp.h"
#include "geese.h"

class Control{
protected:
  int _trace;
  int _ajs;
  int _j1s;
  int _fij;
  int _maxiter;
  double _tol;
  
public:
  Control(int trace, int ajs, int j1s, int fij, int maxiter, double tol);
  Control(int *con, double tol);
  Control(const Control &C);
  int trace() const {return _trace;}
  int ajs() const {return _ajs;}
  int j1s() const {return _j1s;}
  int fij() const {return _fij;}
  int maxiter() const {return _maxiter;}
  double tol() const {return _tol;}
  void set_maxiter(int mit) {_maxiter = mit;}
};

class GeeParam{
protected:
  DVector _beta;
  DVector _alpha;
  DVector _gamma;
  DMatrix _vbeta;
  DMatrix _vbeta_naiv;
  DMatrix _vbeta_ajs;
  DMatrix _vbeta_j1s;
  DMatrix _vbeta_fij;
  DMatrix _valpha;
  DMatrix _valpha_stab;
  DMatrix _valpha_naiv;
  DMatrix _valpha_ajs;
  DMatrix _valpha_j1s;
  DMatrix _valpha_fij;
  DMatrix _vgamma;
  DMatrix _vgamma_ajs;
  DMatrix _vgamma_j1s;
  DMatrix _vgamma_fij;
  //int _ScaleFix;
  int _err;
public:
  GeeParam(DVector Beta, DVector Alpha, DVector Gamma);
  GeeParam(DVector Beta, DVector Alpha, DVector Gamma,
	DMatrix VBeta, DMatrix VBeta_naiv, 
	DMatrix VBeta_ajs, DMatrix VBeta_j1s, 
	DMatrix VBeta_fij,
	DMatrix VAlpha, DMatrix VAlpha_stab,
	DMatrix VAlpha_naiv, DMatrix VAlpha_ajs, 
	DMatrix VAlpha_j1s, DMatrix VAlpha_fij,
	DMatrix VGamma, DMatrix VGamma_ajs, 
	DMatrix VGamma_j1s, DMatrix VGamma_fij);
  ~GeeParam() {}
  void set_beta(const DVector &v) {_beta = v;}
  void set_alpha(const DVector &v) {_alpha = v;}
  void set_vbeta(const DMatrix &m) {_vbeta = m;}
  void set_vbeta_naiv(const DMatrix &m) {_vbeta_naiv = m;}
  void set_vbeta_ajs(const DMatrix &m) {_vbeta_ajs = m;}
  void set_vbeta_j1s(const DMatrix &m) {_vbeta_j1s = m;}
  void set_vbeta_fij(const DMatrix &m) {_vbeta_fij = m;}
  void set_valpha(const DMatrix &m) {_valpha = m;}
  void set_valpha_stab(const DMatrix &m) {_valpha_stab = m;}
  void set_valpha_naiv(const DMatrix &m) {_valpha_naiv = m;}
  void set_valpha_ajs(const DMatrix &m) {_valpha_ajs = m;}
  void set_valpha_j1s(const DMatrix &m) {_valpha_j1s = m;}
  void set_valpha_fij(const DMatrix &m) {_valpha_fij = m;}
  void set_gamma(const DVector &v) {_gamma = v;}
  void set_vgamma(const DMatrix &m) {_vgamma = m;}
  void set_vgamma_ajs(const DMatrix &m) {_vgamma_ajs = m;}
  void set_vgamma_j1s(const DMatrix &m) {_vgamma_j1s = m;}
  void set_vgamma_fij(const DMatrix &m) {_vgamma_fij = m;}
  void set_err(int e) {_err = e;}

  DVector beta() {return _beta;}
  DVector alpha() {return _alpha;}
  DVector gamma() {return _gamma;}
  DMatrix vbeta() {return _vbeta;}
  DMatrix valpha() {return _valpha;}
  DMatrix vbeta_naiv() {return _vbeta_naiv;}
  DMatrix vbeta_ajs() {return _vbeta_ajs;}
  DMatrix vbeta_j1s() {return _vbeta_j1s;}
  DMatrix vbeta_fij() {return _vbeta_fij;}
  DMatrix valpha_stab() {return _valpha_stab;}
  DMatrix valpha_naiv() {return _valpha_naiv;}
  DMatrix valpha_ajs() {return _valpha_ajs;}
  DMatrix valpha_j1s() {return _valpha_j1s;}
  DMatrix valpha_fij() {return _valpha_fij;}
  DMatrix vgamma() {return _vgamma;}
  DMatrix vgamma_ajs() {return _vgamma_ajs;}
  DMatrix vgamma_j1s() {return _vgamma_j1s;}
  DMatrix vgamma_fij() {return _vgamma_fij;}
  //int ScaleFix() {return _ScaleFix;}
  int p() {return _beta.dim();}
  int r() {return _gamma.dim();}
  int q() {return _alpha.dim();}
  int err() {return _err;}
};

#endif //PARAM_H
