#ifndef FAMSTR_H
#define FAMSTR_H

#include "tntsupp.h"
#include "geese.h"

typedef double fun1(double);
typedef bool fun2(double);

enum links {L_0, IDENT, LOGIT, PROBIT, CLOGLOG, LOG, INVERSE, FISHERZ,
	    LWYBC2, LWYLOG};
enum variances {V_0, GAUSSIAN, BINOMIAL, POISSON, GAMMA};
enum correlations {C_0, INDEPENDENCE, EXCHANGEABLE, AR1, UNSTRUCTURED, USERDEFINED,  FIXED};


DMatrix cor_exch(const DVector &rho, const DVector &wave);

DMatrix cor_rho_exch(const DVector &rho, const DVector &wave);

DMatrix cor_indep(const DVector &, const DVector &wave);

DMatrix cor_rho_indep(const DVector &, const DVector &);

DMatrix cor_ar1(const DVector &rho, const DVector &wave);

DMatrix cor_rho_ar1(const DVector &rho, const DVector &wave);

DMatrix cor_unstr(const DVector &rho, const DVector &wave);

DMatrix cor_rho_unstr(const DVector &rho, const DVector &wave);

DMatrix cor_fixed(const DVector &, const DVector &wave); //get cor matrix

DMatrix cor_rho_fixed(const DVector &, const DVector &); //derivative

class Corr{
public:
  typedef DMatrix matfun(const DVector &, const DVector&);
  typedef DMatrix cor_rho_fun(const DVector &, const DVector&);
protected:
  int _corst, _nparam, _maxwave;
  matfun *_mat;
  cor_rho_fun *_cor_rho;
  void init(matfun *mat, cor_rho_fun *cor_rho) {
    _mat = mat; _cor_rho = cor_rho;
  }
public:
  Corr(int corst, int maxwave);
  ~Corr() {}
  DMatrix mat(DVector &rho, DVector &wave) {
    return _mat(rho, wave);
  }
  DMatrix cor_rho(DVector &rho, DVector &wave) {
    return _cor_rho(rho, wave);
  }
  int nparam(){return _nparam;}
  int corst() {return _corst; }
};

class Link{
protected:
  fun1 *_linkfun, *_linkinv, *_mu_eta;
  void init(fun1* linkfun, fun1* linkinv, fun1* mu_eta) {
    _linkfun = linkfun; _linkinv = linkinv; _mu_eta = mu_eta;
  }
public:
  //Link(int link);
  //Link() {int link = IDENT; Link(link); }
  Link(int link = IDENT);  
  Link(fun1* linkfun, fun1* linkinv, fun1* mu_eta);
  ~Link() {}
  double linkfun(double mu) {return _linkfun(mu);}
  double linkinv(double eta) {return _linkinv(eta);}
  double mu_eta(double eta) {return _mu_eta(eta);}
};

class Variance{
protected:
  fun1 *_v, *_v_mu;
  fun2 *_validmu;
  void init(fun1* v, fun1* v_mu, fun2 validmu) {
    _v = v; _v_mu = v_mu; _validmu = validmu;
  }
public:
  Variance(fun1* v, fun1* v_mu, fun2* validmu) {
    init(v, v_mu, validmu);
  }
  //Variance(int var);
  //Variance() {int var = GAUSSIAN; Variance(var);} 
  Variance(int var = GAUSSIAN);
  ~Variance() {}
  double v(double mu) {return _v(mu);}
  double v_mu(double mu) {return _v_mu(mu);}
  bool validmu(double mu) {return _validmu(mu);}
};

/*
static Link Ident(1), Logit(2), Probit(3), Cloglog(4), Log(5), Inverse(6), Fisherz(7);
static Variance Gaussian(1), Binomial(2), Poisson(3), Gamma(4);

static Link LINK[] = {Ident, Logit, Probit, Cloglog, Log, Inverse, Fisherz};
static Variance VARIANCE[] = {Gaussian, Binomial, Poisson, Gamma};
*/


class GeeStr{
  Vector<Link> MeanLink;
  Vector<Variance> V;
  Vector<Link> ScaleLink;
  Link CorrLink;
  int ScaleFix_;
public:
  GeeStr(int n, Vector<int> meanlink, Vector<int> v,
	 Vector<int> scalelink, int corrlink, int scalefix);
  ~GeeStr() {}
  int ScaleFix() {return ScaleFix_;}
  double MeanLinkfun(double mu, int wave) {
    return MeanLink(wave).linkfun(mu);
  }
  double MeanLinkinv(double eta, int wave) {
    return MeanLink(wave).linkinv(eta);
  }
  double MeanMu_Eta(double eta, int wave) {
    return MeanLink(wave).mu_eta(eta);
  }
  DVector MeanLinkfun(const DVector &Mu, const IVector &Wave);
  DVector MeanLinkinv(const DVector &Eta, const IVector &Wave);
  DVector MeanMu_eta(const DVector &Eta, const IVector &Wave);
  DVector ScaleLinkfun(const DVector &Mu, const IVector &Wave);
  DVector ScaleLinkinv(const DVector &Eta, const IVector &Wave);
  DVector ScaleMu_eta(const DVector &Eta, const IVector &Wave);
  DVector CorrLinkfun(const DVector &Mu);
  DVector CorrLinkinv(const DVector &Eta);
  DVector CorrMu_eta(const DVector &Eta);
  DVector v(const DVector &Mu, const IVector &Wave);
  DVector v_mu(const DVector &Mu, const IVector &Wave);
  bool validMu(const DVector &Mu, const IVector &Wave);
};


#endif //FAMSTR_H
