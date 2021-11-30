// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/** \file 
   \brief Classes to construct multivariate **negative log** Gaussian density objects.
   \note These density classes return the **negative log likelihood**.
*/


#define TYPEDEFS(scalartype_)			\
public:						\
typedef scalartype_ scalartype;			\
typedef vector<scalartype> vectortype;		\
typedef matrix<scalartype> matrixtype;		\
typedef array<scalartype> arraytype

#define VARIANCE_NOT_YET_IMPLEMENTED            \
private:                                        \
vectortype variance(){return vectortype();}     \
public:

/**
  Conventions for simulation:

  \internal

  - The simulate method is pass by reference and modifies the
    input. So `Foo.simulate.(x);` fills `x` with a simulation. This is
    implemented for *all* classes.

  - For classes with unambiguous dimension (e.g. MVNORM, GMRF, ...)
    there is a simulate method with void input. It can be used as
    `x=Foo.simulate();`. Other classes (e.g. AR1, ARk) does not have
    this void argument version (attempt of use results in a compile
    time error). It follows that e.g. `SEPARABLE(AR1(phi),
    MVNORM(Sigma))` only has the void argument method whereas
    `SEPARABLE(MVNORM(Sigma), MVNORM(Sigma))` has both methods.

  - All classes must have a method `cov_sqrt_scale` that applied to a
    vector u returns "Sigma^(1/2) * u" for *some* square root. It is
    natural that this method is used by the simulate methods. This
    method is crucial for `SEPARABLE`.

  - All classes must have a `dim()` member. Attempt to call for
    un-supported classes gives compile time error.
*/
#define SIMULATE_NOT_YET_IMPLEMENTED            \
private:                                        \
vectortype sqrt_cov_scale(vectortype x){}       \
void simulate(vectortype &u){}                  \
vectortype simulate(){}                         \
public:

/* Add this macro to all classes with *dynamic* dimension (e.g. AR1) */
#define SIMULATE_IMPLEMENTED_UNKNOWN_SIZE       \
void simulate(vectortype &x) {                  \
  rnorm_fill(x);                                \
  x = sqrt_cov_scale(x);                        \
  x = zero_derivatives(x);                      \
}
/* Add this macro to all classes with *fixed* dimension (e.g. MVNORM) */
#define SIMULATE_IMPLEMENTED_KNOWN_SIZE(SIZE)   \
SIMULATE_IMPLEMENTED_UNKNOWN_SIZE               \
vectortype simulate() {                         \
  vectortype x(SIZE);                           \
  simulate(x);                                  \
  return x;                                     \
}

/* Utility function: The simulators should not track derivatives when
   running with AD types. A workaround is to zero out the derivatives
   after simulation (FIXME: there is a minor efficiency loss by
   tracking the derivatives in the first place...). */
template<class arraytype>
arraytype zero_derivatives(arraytype x) {
  for(int i=0; i<x.size(); i++)
    x(i) = asDouble(x(i));
  return x;
}

/* Utility function */
template<class arraytype>
void rnorm_fill(arraytype &x) {
  for(int i=0; i<x.size(); i++)
    x(i) = Rf_rnorm(0., 1.);
}

/** \brief Multivariate normal distribution with user supplied covariance matrix

    Class to evaluate the negative log density of a mean zero
    multivariate Gaussian variable with general covariance matrix Sigma.
    Intended for small dense covariance matrices.    
    \code
      matrix<Type> Sigma(3,3);
      Sigma.fill(0.1);             // Fill the whole matrix
      Sigma.diagonal() *= 10.0;    // Multiply diagonal by 10 to positive definite Sigma
      vector<Type> x0(3);          // Point of evaluation
      x0.fill(0.0);                // Initialize x0 to be zero
      MVNORM_t<Type> N_0_Sigma(Sigma);   // N_0_Sigma is now a Distribution  
      res = N_0_Sigma(x0);         // Evaluates (neg. log) density at x
    \endcode

*/
template <class scalartype_>
class MVNORM_t{
  TYPEDEFS(scalartype_);
  matrixtype Q;       /* Inverse covariance matrix */
  scalartype logdetQ; /* log-determinant of Q */
  matrixtype Sigma;   /* Keep for convenience - not used */
  matrixtype L_Sigma; /* Used by simulate() */
public:
  MVNORM_t(){}
  MVNORM_t(matrixtype Sigma_, bool use_atomic=true){
    setSigma(Sigma_, use_atomic);
  }

  /** \brief Covariance matrix extractor 

    Typical use:
    \code 
      matrix<Type> Sigma(3,3);
      MVNORM_t<Type> N_0_Sigma(Sigma);   // N_0_Sigma is now a Distribution  
      N_0_Sigma.cov();                   // Returns covariance matrix (Sigma in this case)
    \endcode
    Useful for classes such as \ref UNSTRUCTURED_CORR_t that inherits from MVNORM_t.
  */
  matrixtype cov(){return Sigma;}

  /* initializer via covariance matrix */
  void setSigma(matrixtype Sigma_, bool use_atomic=true){
    Sigma = Sigma_;
    scalartype logdetS;
    if(use_atomic){
      Q = atomic::matinvpd(Sigma,logdetS);
    } else {
      matrixtype I(Sigma.rows(),Sigma.cols());
      I.setIdentity();
      Eigen::LDLT<Eigen::Matrix<scalartype,Dynamic,Dynamic> > ldlt(Sigma);
      Q = ldlt.solve(I);
      vectortype D = ldlt.vectorD();
      logdetS = D.log().sum();
    }
    logdetQ = -logdetS;
  }
  scalartype Quadform(vectortype x){
    return (x*(vectortype(Q*x))).sum();
  }
  /** \brief Evaluate the negative log density */
  scalartype operator()(vectortype x){
    return -scalartype(.5)*logdetQ + scalartype(.5)*Quadform(x) + x.size()*scalartype(log(sqrt(2.0*M_PI)));
  }
  /** \brief Evaluate _projected_ negative log density
      \param keep Vector of 0/1 indicating marginal to evaluate.
   */
  scalartype operator()(vectortype x, vectortype keep){
    matrix<scalartype> S = Sigma;
    vector<scalartype> not_keep = scalartype(1.0) - keep;
    for(int i = 0; i < S.rows(); i++){
      for(int j = 0; j < S.cols(); j++){
	S(i,j) = S(i,j) * keep(i) * keep(j);
      }
      S(i,i) += not_keep(i) * scalartype(1.0 / (2.0 * M_PI));
    }
    return MVNORM_t<scalartype>(S)(x * keep);
  }
  arraytype jacobian(arraytype x){
    arraytype y(x.dim);
    matrixtype m(x.size()/x.cols(),x.cols());
    for(int i=0;i<x.size();i++)m(i)=x[i];
    matrixtype mQ=m*Q;
    for(int i=0;i<x.size();i++)y[i]=mQ(i);
    return y;
  }
  int ndim(){return 1;}
  VARIANCE_NOT_YET_IMPLEMENTED
  vectortype sqrt_cov_scale(vectortype u) {
    if(L_Sigma.rows() == 0) {
      Eigen::LLT<Eigen::Matrix<scalartype,Dynamic,Dynamic> > llt(Sigma);
      L_Sigma = llt.matrixL();
    }
    vectortype ans = L_Sigma * u;
    return ans;
  }
  SIMULATE_IMPLEMENTED_KNOWN_SIZE(Sigma.rows());
};

/** \brief Construct object to evaluate multivariate zero-mean normal density with user supplied covariance matrix

    \param Sigma Positive definite covariance matrix.
    \param use_atomic Determines if "atomic functions" are used for the linear algebra (default).
    
    Shortform version for working with the density of the \ref MVNORM_t 
    distribution (C++ class). Typical use:
    \code 
      MVNORM(Sigma)(x);
    \endcode
    where \c Sigma is a covariance matrix. The part \c MVNORM(Sigma) creates
    an object of MVNORM_t, which is then evaluated by the part \c (x).
*/
template <class scalartype>
MVNORM_t<scalartype> MVNORM(matrix<scalartype> Sigma, bool use_atomic = true){
  return MVNORM_t<scalartype>(Sigma, use_atomic);
}

/** \brief Multivariate normal distribution with unstructered correlation matrix

   Class to evaluate the negative log density of a multivariate Gaussian 
   variable with \b unstructured symmetric positive definite correlation matrix (*Sigma*). The
   typical application of this is that you want to estimate all the elements of *Sigma*,
   in such a way that the symmetry and positive definiteness constraint is respected. 
   We parameterize *S* via a lower triangular matrix *L*
   with unit diagonal i.e. we need (n*n-n)/2 parameters to describe 
   an n dimensional correlation matrix.

   For instance in the case n=4 the correlation matrix is given by
   \f[\Sigma = D^{-\frac{1}{2}}LL'D^{-\frac{1}{2}}\f]
   where
   \f[
   L=\begin{pmatrix}
   1 \\
   \theta_0 & 1 \\
   \theta_1 & \theta_2 & 1 \\
   \theta_3 & \theta_4 & \theta_5 & 1
   \end{pmatrix}
   \f]
   (lower triangle filled row-wise) and
   \f[
   D=diag(LL')
   \f]

   Example:
   \code
   // Construct density object of dimension 4
   vector<Type> theta(6);
   UNSTRUCTURED_CORR_t<Type> nll(theta);
   vector<Type> x(4);
   res = nll(x);               // Evaluate neg. log density
   \endcode

   \remark *Sigma* is available via \ref MVNORM_t.cov , e.g.
   \code
     nll.cov();
   \endcode
   
   \remarks *Sigma* has 1's on its diagonal. To scale the variances we can use \ref VECSCALE_t , e.g.
   \code
     vector<Type> sds(4);
     sds.fill(2.0);                            // Set all standard deviations to 2.0
     res = VECSCALE_t(nll,sds)(x);
   \endcode
*/   
template <class scalartype_>
class UNSTRUCTURED_CORR_t : public MVNORM_t<scalartype_>{
  TYPEDEFS(scalartype_);
  UNSTRUCTURED_CORR_t(){}
  UNSTRUCTURED_CORR_t(vectortype x){
    // (n*n-n)/2=nx  ==>  n*n-n-2*nx=0 ==> n=(1+sqrt(1+8*nx))/2
    int nx=x.size();
    int n=int((1.0+sqrt(1.0+8*nx))/2.0);
    if((n*n-n)/2!=nx)Rcout << "vector does not specify an UNSTRUCTERED_CORR\n";
    matrixtype L(n,n);
    L.setIdentity();
    int i,j,k=0;
    for(i=0;i<L.rows();i++){
      for(j=0;j<L.cols();j++){
	if(i>j){L(i,j)=x[k];k++;}
      }
    }
    matrixtype llt=L*L.transpose();
    matrixtype Sigma=llt;
    for(i=0;i<Sigma.rows();i++){
      for(j=0;j<Sigma.cols();j++){
	Sigma(i,j)/=sqrt(llt(i,i)*llt(j,j));
      }
    }    
    this->setSigma(Sigma); /* Call MVNORM_t initializer */
  }
};
/** \brief Construct object to evaluate the density with unstructured correlation matrix.
     See UNSTRUCTURED_CORR_t for details */
template <class scalartype>
UNSTRUCTURED_CORR_t<scalartype> UNSTRUCTURED_CORR(vector<scalartype> x){
  return UNSTRUCTURED_CORR_t<scalartype>(x);
}

/** \brief Standardized normal distribution

    Class to evaluate the negative log density of a (multivariate)
    standard normal distribution.
    \verbatim
    Examples: N01()
    \endverbatim
*/   
template<class scalartype_> 
class N01{
  TYPEDEFS(scalartype_);
public:
  /** \brief Evaluate the negative log density */
  scalartype operator()(scalartype x){
    return x*x*.5 + log(sqrt(2.0*M_PI));
  }
  scalartype operator()(vectortype x){
    return (x*x*scalartype(.5) + scalartype(log(sqrt(2.0*M_PI))) ).sum() ;
  }
  scalartype operator()(arraytype x){
    return (x*x*scalartype(.5) + scalartype(log(sqrt(2.0*M_PI))) ).sum() ;
  }
  arraytype jacobian(arraytype x){return x;}
  int ndim(){return 1;}
  VARIANCE_NOT_YET_IMPLEMENTED
  // FIXME: 1D will not suffice for e.g. SEPARABLE(N01, OTHER);
  vectortype simulate() {
    vectortype x(1);
    x[0] = rnorm(0.0, 1.0);
    return x;
  }
  // Inplace simulate
  void simulate(vectortype &x) {
    rnorm_fill(x);
  }
  vectortype sqrt_cov_scale(vectortype u) {
    return u;
  }

};

/** \brief Stationary AR1 process

    Class to evaluate the negative log density of a (multivariate) AR1
    process with parameter phi and given marginal distribution.
    @param phi       Scalar -1<phi<1
    @tparam MARGINAL The desired (multivariate) marginal distribution.

    Let \f$f(x)\f$ denote a multivariate Gaussian mean-zero negative log density
    represented by its covariance matrix \f$\Sigma\f$. Define recursively the vectors
    \f[x_0\sim N(0,\Sigma)\f]
    \f[x_1 = \phi x_0 + \sigma\varepsilon_1\:,\:\:\: \varepsilon_1 \sim N(0,\Sigma)\f]
    \f[x_i = \phi x_{i-1} + \sigma\varepsilon_i\:,\:\:\: \varepsilon_i \sim N(0,\Sigma)\f]
    where \f$\sigma=\sqrt{1-\phi^2}\f$. Then \f$E(x_i)=0\f$, \f$V(x_i)=\Sigma\f$ and the covariance
    is \f$E(x_ix_j')=\phi^{|i-j|}\Sigma\f$. We refer to this process as a stationary 1st order
    autoregressive process with multivariate increments with parameter phi and marginal distribution f.
    Compactly denoted AR1(phi,f).

    Note that the construction can be carried out recursively, as "AR1(phi,f)" is itself a distribution
    that can be used as input to AR1(). See example below:
    \code
    \\ Construct negative log density of standard AR1 process on a line:
    Type phi1=0.8;
    AR1_t<N01<Type> > f1(phi1);
    \\ Can be evaluated on a vector:
    vector<Type> x(10);
    Type ans=f1(x);
    \endcode

    Now use f1 as marginal in a new AR1 process with parameter phi2:

    \code
    \\ Construct negative log density of standard AR1 process on a line:
    Type phi2=0.5;
    AR1_t<AR1_t<N01<Type> > > f2(phi1,f1);
    \\ Can be evaluated on a 2-dimensional array:
    vector<Type> x(10,20);
    Type ans=f2(x);
    \endcode

*/   
template <class distribution>
class AR1_t{
  TYPEDEFS(typename distribution::scalartype);
private:
  scalartype phi;
  distribution MARGINAL;
public:
  AR1_t(){/*phi=phi_;MARGINAL=f_;*/}
  AR1_t(scalartype phi_, distribution f_) : phi(phi_), MARGINAL(f_) {}
  /** \brief Evaluate the negative log density */
  scalartype operator()(vectortype x){
    scalartype value;
    value=scalartype(0);
    int n=x.rows();
    int m=x.size()/n;
    scalartype sigma=sqrt(scalartype(1)-phi*phi); /* Steady-state standard deviation */
    value+=MARGINAL(x(0));                                       /* E.g. x0 ~ N(0,1)  */
    for(int i=1;i<n;i++)value+=MARGINAL((x(i)-x(i-1)*phi)/sigma);/* x(i)-phi*x(i-1) ~ N(0,sigma^2) */
    value+=scalartype((n-1)*m)*log(sigma);
    return value;
  }

  /* Copied vector version - apply over outermost dimension */
  scalartype operator()(arraytype x){
    scalartype value;
    value=scalartype(0);
    int n=x.cols();
    int m=x.size()/n;
    scalartype sigma=sqrt(scalartype(1)-phi*phi); /* Steady-state standard deviation */
    value+=MARGINAL(x.col(0));                                  /* E.g. x0 ~ N(0,1)  */
    for(int i=1;i<n;i++)value+=MARGINAL((x.col(i)-x.col(i-1)*phi)/sigma);/* x(i)-phi*x(i-1) ~ N(0,sigma^2) */
    value+=scalartype((n-1)*m)*log(sigma);
    return value;
  }


  arraytype jacobian(arraytype x){
    scalartype sigma=sqrt(scalartype(1)-phi*phi); /* Steady-state standard deviation */
    int n=x.cols();
    arraytype y(x.dim);
    y.setZero();
    y.col(0) = y.col(0) + MARGINAL.jacobian(x.col(0));
    for(int i=1;i<n;i++){
      //MARGINAL((x(i)-x(i-1)*phi)/sigma);
      y.col(i-1) = y.col(i-1) - (phi/sigma) * MARGINAL.jacobian((x.col(i)-x.col(i-1)*phi)/sigma);
      y.col(i) = y.col(i) +  MARGINAL.jacobian((x.col(i)-x.col(i-1)*phi)/sigma)/sigma;
    }
    return y;
  }
  int ndim(){return 1;}
  VARIANCE_NOT_YET_IMPLEMENTED

  /* Simulation */
  arraytype sqrt_cov_scale(arraytype u) {
    if(u.dim.size() == 1) {
      // FIXME: This entire class should be reconsidered with this
      // special case in mind !
      u.dim.resize(2); u.dim << 1, u.size();
    }
    int n = u.cols();
    arraytype x(u.dim);
    scalartype sigma = sqrt(1.0 - phi * phi); /* Steady-state standard deviation */
    x.col(0) = MARGINAL.sqrt_cov_scale(u.col(0));
    for(int i=1; i<n; i++)
      x.col(i) = phi * x.col(i-1) + sigma * MARGINAL.sqrt_cov_scale(u.col(i));
    return x;
  }
  vectortype sqrt_cov_scale(vectortype u) {
    vector<int> dim(2);
    dim << 1, u.size();
    arraytype u_array(u, dim);
    return sqrt_cov_scale(u_array);
  }
  /** \brief Draw a simulation from the process */
  void simulate(vectortype &x) {
    rnorm_fill(x);
    x = sqrt_cov_scale(x);
    x = zero_derivatives(x);
  }
  void simulate(arraytype &x) {
    rnorm_fill(x);
    x = sqrt_cov_scale(x);
    x = zero_derivatives(x);
  }

};
template <class scalartype, class distribution>
AR1_t<distribution> AR1(scalartype phi_, distribution f_){
  return AR1_t<distribution>(phi_, f_);
}
template <class scalartype>
AR1_t<N01<scalartype> > AR1(scalartype phi_){
  return AR1_t<N01<scalartype> >(phi_, N01<scalartype>());
}


/** \brief Stationary AR(k) process.

    @param phi_ Vector of length k with parameters.

    \verbatim
    Class to evaluate the negative log density of a stationary 
    AR(k)-process with parameter vector phi=[phi_1,...,phi_k]:
    
    x[t]=phi_1*x[t-1]+...+phi_k*x[t-k]+eps[t]

    where eps[t]~N(0,sigma^2). The parameter sigma^2 is chosen to 
    obtain V(x[t])=1 so that the class actually specifies a correlation
    model.

    Examples: ARk(phi)  <-- simple mean zero variance 1 AR(k) process.

    Steady state initial distribution is found by (e.g. k=3)

    [gamma(1)]    [gamma(0) gamma(1) gamma(2)]     [phi1]
    [ ....   ] =  [gamma(1) gamma(0) gamma(1)]  *  [phi2]
    [gamma(3)]    [gamma(2) gamma(1) gamma(0)]     [phi3]
    \endverbatim

*/   
template <class scalartype_>
class ARk_t{
  TYPEDEFS(scalartype_);
  //private:
  int k;
  vectortype phi;   /* [phi1,...,phik] */
  vectortype gamma; /* [gamma(1),...,gamma(k)] (note gamma(0) is 1) */
  /* Initial distribution matrices. */
  matrixtype V0;    /* kxk variance  */
  matrixtype Q0;    /* kxk precision */
  matrixtype L0;    /* kxk Cholesky Q0 = L0*L0' */
  /* gamma is found through (I-M)*gamma=phi ... */
  matrixtype M;     /* kxk   */
  matrixtype I;     /* kxk   */
  scalartype sigma;/* increment standard deviation */
  scalartype logdetQ0;
public:
  ARk_t(){/*phi=phi_;MARGINAL=f_;*/}
  ARk_t(vectortype phi_){
    phi=phi_;
    k=phi.size();
    V0.resize(k,k);Q0.resize(k,k);
    M.resize(k,k);I.resize(k,k);
    /* build M-matrix */
    M.setZero();
    int d;
    for(int i=0;i<k;i++){
      for(int j=0;j<k;j++){
	d=abs(i-j);
	if(d!=0){
	  M(i,d-1)+=phi[j];
	}
      }
    }
    I.setIdentity();
    gamma=((I-M).inverse())*matrixtype(phi);
    /* Increment sd */
    sigma=sqrt(scalartype(1)-(phi*gamma).sum());
    /* build V0 matrix */
    for(int i=0;i<k;i++){
      for(int j=0;j<k;j++){
	d=abs(i-j);
	if(d==0)
	  V0(i,j)=scalartype(1); 
	else 
	  V0(i,j)=gamma(d-1);
      }
    }
    /* build Q0 matrix */
    Q0=V0.inverse();
    /* log determinant */
    L0=Q0.llt().matrixL(); /* L0 L0' = Q0 */
    logdetQ0=scalartype(0);
    for(int i=0;i<k;i++)logdetQ0+=scalartype(2)*log(L0(i,i));
  }
  /** \brief Covariance extractor. 
      Run Youle-Walker recursions and return a vector of length n representing
      the auto-covariance function.
  */
  vectortype cov(int n){
    vectortype rho(n);
    for(int i=0;i<n;i++){
      if(i==0){rho(0)=scalartype(1);}
      else if(i<=k){rho(i)=gamma(i-1);}
      else { /* youle walker */
	scalartype tmp=0;
	for(int j=0;j<k;j++)tmp+=phi[j]*rho[i-1-j];
	rho(i)=tmp;
      }
    }
    return rho;
  }

  /** \brief Evaluate the negative log density */
  scalartype operator()(vectortype x){
    scalartype value=0;
    /* Initial distribution. For i = k,...,1 the recursions for
       solving L' y = u (1-based index notation) are:

       y(i) = L(i,i)^-1 * ( u(i) - L(i+1,i) * y(i+1) - ... - L(k,i) * y(k) )

       where u(i) ~ N(0,1).
    */
    scalartype mu, sd;
    int col;
    for(int i=0; (i<k) & (i<x.size()); i++){
      mu = scalartype(0);
      col = k-1-i; /* reversed index */
      for(int j=col+1; j<k; j++) mu -= L0(j,col) * x(k-1-j);
      mu /= L0(col, col);
      sd = scalartype(1) / L0(col, col);
      value -= dnorm(x[i], mu, sd, true);
    }
    scalartype tmp;
    for(int i=k;i<x.size();i++){
      tmp=scalartype(0);
      for(int j=0;j<k;j++){
	tmp+=phi[j]*x[i-1-j];
      }
      value-=dnorm(x[i],tmp,sigma,1);
    }
    return value;
  }
  arraytype jacobian(arraytype x){
    arraytype y(x.dim);
    y.setZero();
    for(int i=0;i<k;i++)
      for(int j=0;j<k;j++)
	y.col(i)=y.col(i)+Q0(i,j)*x.col(j);
    vectortype v(k+1);
    v(0)=scalartype(1);
    for(int i=1;i<=k;i++)v[i]=-phi[i-1];
    v=v/sigma;
    for(int i=k;i<x.cols();i++){
      for(int j1=0;j1<=k;j1++){
	for(int j2=0;j2<=k;j2++){
	  y.col(i-j1)=y.col(i-j1)+v[j1]*v[j2]*x.col(i-j2);
	}
      }
    }
    return y;
  }
  int ndim(){return 1;}
  VARIANCE_NOT_YET_IMPLEMENTED
  /* Simulate - copy paste from evaluation operator */
  vectortype sqrt_cov_scale(vectortype u){
    vectortype x(u.size());
    scalartype mu, sd;
    int col;
    for (int i = 0; (i < k) & (i < x.size()); i++) {
      mu = scalartype(0);
      col = k - 1 - i; /* reversed index */
      for (int j = col + 1; j < k; j++)
        mu -= L0(j, col) * x(k - 1 - j);
      mu /= L0(col, col);
      sd = scalartype(1) / L0(col, col);
      x(i) = sd * u(i) + mu;
    }
    scalartype tmp;
    for (int i = k; i < x.size(); i++) {
      tmp = scalartype(0);
      for (int j = 0; j < k; j++) {
        tmp += phi[j] * x[i - 1 - j];
      }
      x(i) = sigma * u(i) + tmp;
    }
    return x;
  }
  SIMULATE_IMPLEMENTED_UNKNOWN_SIZE
};

template <class scalartype>
ARk_t<scalartype> ARk(vector<scalartype> phi){
  return ARk_t<scalartype>(phi);
}


/** \brief Continuous AR(2) process

    \verbatim
    Process with covariance satisfying the 2nd order ode 
    rho''=c1*rho'-rho on an arbitrary irregular grid. 
    (shape=c1/2, -1<shape<1). Initial condition rho(0)=1, rho'(0)=0,
    rho''(0)=-1.
    \endverbatim
    
    Process is augmented with derivatives in order to obtain exact sparseness
    of the full precision. That is, if a model is desired on a grid of size n,
    then additional n extra nuisance parameters must be supplied.
    
    @param grid_ Possibly irregular grid of length n
    @param shape_ Parameter defining the shape of the correlation function.
    @param scale_ Parameter defining the correlation range.  
*/
template <class scalartype_>
class contAR2_t{
  TYPEDEFS(scalartype_);
private:
  typedef Matrix<scalartype,2,2> matrix2x2;
  typedef Matrix<scalartype,2,1> matrix2x1;
  typedef Matrix<scalartype,4,4> matrix4x4;
  typedef Matrix<scalartype,4,1> matrix4x1;
  scalartype shape,scale,c0,c1;
  vectortype grid;
  matrix2x2 A, V0, I;
  matrix4x4 B, iB; /* B=A %x% I + I %x% A  */
  matexp<scalartype,2> expA;
  matrix4x1 vecSigma,iBvecSigma;
  vector<MVNORM_t<scalartype> > neglogdmvnorm; /* Cache the 2-dim increments */
  vector<matrix2x2 > expAdt; /* Cache matrix exponential for grid increments */
public:
  contAR2_t(){};
  contAR2_t(vectortype grid_, scalartype shape_, scalartype scale_=1){
    shape=shape_;scale=scale_;grid=grid_;
    c0=scalartype(-1);c1=scalartype(2)*shape_;
    c0=c0/(scale*scale); c1=c1/scale;
    A << scalartype(0), scalartype(1), c0, c1;
    V0 << 1,0,0,-c0;
    I.setIdentity();
    B=kronecker(I,A)+kronecker(A,I);
    iB=B.inverse();
    expA=matexp<scalartype,2>(A);
    vecSigma << 0,0,0,scalartype(-2)*c1*V0(1,1);
    iBvecSigma=iB*vecSigma;
    /* cache increment distribution N(0,V(dt)) - one for each grid point */
    neglogdmvnorm.resize(grid.size());
    neglogdmvnorm[0]=MVNORM_t<scalartype>(V0);
    for(int i=1;i<grid.size();i++)neglogdmvnorm[i]=MVNORM_t<scalartype>(V(grid(i)-grid(i-1)));
    /* cache matrix exponential */
    expAdt.resize(grid.size());
    expAdt[0]=expA(scalartype(0));
    for(int i=1;i<grid.size();i++)expAdt[i]=expA(grid(i)-grid(i-1));
  }
  /* Simple formula for matrix exponential exp(B*t) */
  matrix4x4 expB(scalartype t){
    return kronecker(expA(t),expA(t));
  }
  /* Variance as fct. of time when started deterministic */
  matrix2x2 V(scalartype t){
    matrix4x1 tmp;
    tmp=expB(t)*iBvecSigma-iBvecSigma;
    matrix2x2 ans;
    for(int i=0;i<4;i++)ans(i)=tmp(i);
    return ans;
  }
  /** \brief Evaluate the negative log density of the process x with 
      nuisance parameters dx */
  scalartype operator()(vectortype x,vectortype dx){
    matrix2x1 y, y0;
    scalartype ans;
    y0 << x(0), dx(0);
    ans = neglogdmvnorm[0](y0);
    for(int i=1;i<grid.size();i++){
      y0 << x(i-1), dx(i-1);
      y << x(i), dx(i);
      ans += neglogdmvnorm[i](y-expAdt[i]*y0);
    }
    return ans;
  }
  /* Experiment: Implementing matrix-vector multiply - Q*x.
     To be used when creating separable extensions... 
     think best to assume that input array has dimension (2,ntime,...) 
     so that we can easily extract x(t) and multiply Q*x(t) with a 2x2 matrix */
  scalartype operator()(vectortype x){ /* x.dim=[2,n] */
    vector<int> dim(2);
    dim << 2 , x.size()/2 ;
    array<scalartype> y(x,dim);
    y=y.transpose();
    return this->operator()(y.col(0),y.col(1));
  }
  arraytype matmult(matrix2x2 Q,arraytype x){
    arraytype y(x.dim);
    y.col(0) = Q(0,0)*x.col(0)+Q(0,1)*x.col(1); /* TODO: can we subassign like this in array class? Hack: we use "y.row" for that */
    y.col(1) = Q(1,0)*x.col(0)+Q(1,1)*x.col(1);
    return y;
  }
  arraytype jacobian(arraytype x){
    arraytype y(x.dim);
    y.setZero();
    arraytype tmp(y(0).dim);
    y.col(0) = neglogdmvnorm[0].jacobian(x.col(0)); /* Time zero contrib */
    for(int i=1;i<grid.size();i++){
      /* When taking derivative of .5*(x(i)-G*x(i-1))'*Q*(x(i)-G*x(i-1)) [where G=expAdt]
	 we get contributions like: 
	 x(i-1): -G'*Q*(x(i)-G*x(i-1))
	 x(i):   Q*(x(i)-G*x(i-1))
      */
      tmp=neglogdmvnorm[i].jacobian( x.col(i) - matmult(expAdt[i],x.col(i-1)) );
      y.col(i)=y.col(i)+tmp;
      y.col(i-1)=y.col(i-1)-matmult(expAdt[i].transpose(), tmp );
    }
    return y;
  } 
  int ndim(){return 2;} /* Number of dimensions this structure occupies in total array */
  VARIANCE_NOT_YET_IMPLEMENTED
  SIMULATE_NOT_YET_IMPLEMENTED
};
template <class scalartype, class vectortype>
contAR2_t<scalartype> contAR2(vectortype grid_, scalartype shape_, scalartype scale_=1){
  return contAR2_t<scalartype>(grid_, shape_, scale_);
}
template <class scalartype>
contAR2_t<scalartype> contAR2(scalartype shape_, scalartype scale_=1){
  return contAR2_t<scalartype>(shape_, scale_);
}

/** \brief Gaussian Markov Random Field

    \verbatim
    Class to evaluate the negative log density of a mean zero multivariate 
    normal distribution with a sparse precision matrix. Let Q denote the 
    precision matrix. Then the density is proportional to
    |Q|^.5*exp(-.5*x'*Q*x)
    
    Three constructors are available:

    1. General case
    ===============
    The user supplies the precision matrix Q of class Eigen::SparseMatrix<Type> 
    
    2. Special case: GMRF on d-dimensional lattice.
    ===============================================
    The user supplies a d-dim lattice for which Q is automatically 
    constructed like this:
    First order Gaussian Markov Random Field on (subset of) d-dim grid.
    Grid is specified through the first array argument to constructor, 
    with individual nodes determined by the outdermost dimension 
    e.g. x= 1 1 2 2
            1 2 1 2
    corresponding to a 2x2 lattice with 4 nodes and d=2.

    Example of precision in 2D:

       -1
    -1 4+c -1
       -1

   The precision Q is convolved with it self "order" times. This way
   more smoothness can be obtained. The quadratic form contribution 
   is .5*x'*Q^order*x

   3. Vector of deltas
   ===================
   The parameter "delta" describes the (inverse) correlation. It is
   allowed to specify a vector of deltas so that different spatial 
   regions can have different spatial correlation.
   
   NOTE: The variance in the model depends on delta. In other words:
   The model may be thought of as an arbitrary scaled correlation 
   model and is thus not really meaningful without an additional scale
   parameter (see SCALE_t and VECSCALE_t classes).
   \endverbatim
*/
template <class scalartype_>
class GMRF_t {
  TYPEDEFS(scalartype_);
private:
  Eigen::SparseMatrix<scalartype> Q;
  scalartype logdetQ;
  int sqdist(vectortype x, vectortype x_) {
    int ans = 0;
    int tmp;
    for(int i=0; i<x.size(); i++){
      tmp = CppAD::Integer(x[i]) - CppAD::Integer(x_[i]);
      ans += tmp * tmp;
    }
    return ans;
  }
public:
  GMRF_t(){}
  GMRF_t(Eigen::SparseMatrix<scalartype> Q_, int order_=1, bool normalize=true){
    setQ(Q_, order_, normalize);
  }
  GMRF_t(arraytype x, vectortype delta, int order_=1, bool normalize=true){
    int n = x.cols();
    typedef Eigen::Triplet<scalartype> T;
    std::vector<T> tripletList;
    for(int i=0; i<n; i++){
      for(int j=0; j<n; j++){
	if (sqdist(x.col(i),x.col(j)) == 1){
          tripletList.push_back(T(i, j, scalartype(-1)));
          tripletList.push_back(T(i, i, scalartype( 1)));
	}
      }
    }
    for(int i=0; i<n; i++) {
      tripletList.push_back(T(i, i, delta[i]));
    }
    Eigen::SparseMatrix<scalartype> Q_(n, n);
    Q_.setFromTriplets(tripletList.begin(), tripletList.end());
    setQ(Q_, order_, normalize);
  }
  void setQ(Eigen::SparseMatrix<scalartype> Q_, int order=1, bool normalize=true){
    Q = Q_;
    if (normalize) {
      Eigen::SimplicialLDLT< Eigen::SparseMatrix<scalartype> > ldl(Q);
      vectortype D = ldl.vectorD();
      logdetQ = (log(D)).sum();
    } else {
      logdetQ = 0;
    }
    /* Q^order */
    for(int i=1; i<order; i++){
      Q = Q * Q_;
    }
    logdetQ = scalartype(order) * logdetQ;
  }
  /* Quadratic form: x'*Q^order*x */
  scalartype Quadform(vectortype x){
    return (x * (Q * x.matrix()).array()).sum();
  }
  scalartype operator()(vectortype x){
    return -scalartype(.5) * logdetQ + scalartype(.5) * Quadform(x) + x.size() * scalartype(log(sqrt(2.0 * M_PI)));
  }
  /* jacobian */
  arraytype jacobian(arraytype x){
    arraytype y(x.dim);
    matrixtype m(x.size()/x.cols(),x.cols());
    for(int i=0; i<x.size(); i++) m(i) = x[i];
    matrixtype mQ = m * Q;
    for(int i=0; i<x.size(); i++) y[i] = mQ(i);
    return y;
  }
  int ndim() { return 1; }
  vectortype variance() {
    int n = Q.rows();
    vectortype ans(n);
    matrixtype C = invertSparseMatrix(Q);
    for(int i=0; i<n; i++) ans[i] = C(i,i);
    return ans;
  }
  /* Simulation */
  Eigen::SparseMatrix<scalartype> L;
  Eigen::PermutationMatrix<Dynamic,Dynamic> Pinv;
  vectortype sqrt_cov_scale(vectortype u) {
    if(L.rows() == 0) {
      Eigen::SimplicialLLT<Eigen::SparseMatrix<scalartype> > solver(Q);
      L = solver.matrixL();
      Pinv = solver.permutationPinv();
    }
    // L*L^T = P*Q*Pinv => Q^-1 = A*A^T where A:=P^-1*L^T^-1
    matrixtype x = L.transpose().template triangularView<Eigen::Upper>().solve(u.matrix());
    x = Pinv * x;
    return x.vec();
  }
  SIMULATE_IMPLEMENTED_KNOWN_SIZE(Q.rows())
};
/** \brief Construct object to evaluate density of Gaussian Markov Random Field (GMRF) for sparse Q

  For detailed explanation of GMRFs see the class definition @ref GMRF_t
  \param Q precision matrix
  \param order Convolution order, i.e. the precision matrix is Q^order (matrix product)
  \param normalize Add normalizing constant ?

*/
template <class scalartype>
GMRF_t<scalartype> GMRF(Eigen::SparseMatrix<scalartype> Q, int order, bool normalize=true) {
  return GMRF_t<scalartype>(Q, order, normalize);
}
template <class scalartype, class arraytype >
GMRF_t<scalartype> GMRF(arraytype x, vector<scalartype> delta, int order=1, bool normalize=true) {
  return GMRF_t<scalartype>(x, delta, order, normalize);
}
template <class scalartype, class arraytype >
GMRF_t<scalartype> GMRF(arraytype x, scalartype delta, int order=1, bool normalize=true) {
  vector<scalartype> d(x.cols());
  for(int i=0; i<d.size(); i++) d[i] = delta;
  return GMRF_t<scalartype>(x, d, order, normalize);
}
template <class scalartype>
GMRF_t<scalartype> GMRF(Eigen::SparseMatrix<scalartype> Q, bool normalize = true) {
  return GMRF_t<scalartype>(Q, 1, normalize);
}

/** \brief Apply scale transformation on a density

    Assume x has density f. Construct the density of y=scale*x where scale is a scalar.

    @param f_ distribution
    @param scale_ scalar
*/ 
template <class distribution>
class SCALE_t{
  TYPEDEFS(typename distribution::scalartype);
private:
  distribution f;
  scalartype scale;
public:
  SCALE_t(){}
  SCALE_t(distribution f_, scalartype scale_){scale=scale_;f=f_;}
  /** \brief Evaluate the negative log density */
  scalartype operator()(arraytype x){
    scalartype ans=f(x/scale);
    ans+=x.size()*log(scale);
    return ans;
  }
  scalartype operator()(vectortype x){
    scalartype ans=f(x/scale);
    ans+=x.size()*log(scale);
    return ans;
  }
  arraytype jacobian(arraytype x){
    return f.jacobian(x/scale)/scale;    
  }
  int ndim(){return f.ndim();}
  vectortype variance(){
    return (scale*scale)*f.variance();
  }
  vectortype sqrt_cov_scale(vectortype u){
    return scale * f.sqrt_cov_scale(u);
  }
  SIMULATE_IMPLEMENTED_UNKNOWN_SIZE
};
template <class scalartype, class distribution>
SCALE_t<distribution> SCALE(distribution f_, scalartype scale_){
  return SCALE_t<distribution>(f_,scale_);
}

/** \brief Apply a vector scale transformation on a density

    Assume x has density f. Construct the density of y=scale*x where scale is a vector.

    @param f_ distribution
    @param scale_ vector
    
    \remark 
    To scale the standard deviations of a 
    unit-variance multivariate normal distribution of class UNSTRUCTURED_CORR_t:
    \code
      vector<Type> Lx(6);				       
      UNSTRUCTURED_CORR_t<Type> nll(Lx);
      vector<Type> sds(4);
      sds.fill(2.0);                            // Set all standard deviations to 2.0
      res = VECSCALE_t(nll,sds)(x);
    \endcode
    
    \remark
    Another application is to scale the variance of a unit-variance AR(1) process;
    see \ref AR1_t . 
*/ 
template <class distribution>
class VECSCALE_t{
  TYPEDEFS(typename distribution::scalartype);
private:
  distribution f;
  vectortype scale;
public:
  VECSCALE_t(){}
  VECSCALE_t(distribution f_, vectortype scale_){scale=scale_;f=f_;}
  /** \brief Evaluate the negative log density */
  scalartype operator()(arraytype x){
    // assert that x.size()==scale.size()
    scalartype ans=f(x/scale);
    ans+=(log(scale)).sum();
    return ans;
  }
  scalartype operator()(vectortype x){
    // assert that x.size()==scale.size()
    scalartype ans=f(x/scale);
    ans+=(log(scale)).sum();
    return ans;
  }
  arraytype jacobian(arraytype x){
    // assert that x.rows()==scale.size()
    arraytype y(x);
    for(int i=0;i<y.cols();i++)y.col(i)=y.col(i)/scale[i];
    y=f.jacobian(y);
    for(int i=0;i<y.cols();i++)y.col(i)=y.col(i)/scale[i];
    return y;
  }
  int ndim(){return f.ndim();}
  VARIANCE_NOT_YET_IMPLEMENTED
  vectortype sqrt_cov_scale(vectortype u){
    return scale * f.sqrt_cov_scale(u);
  }
  SIMULATE_IMPLEMENTED_UNKNOWN_SIZE
};
/** \brief Construct object to evaluate a scaled density. See VECSCALE_t for details */
template <class vectortype, class distribution>
VECSCALE_t<distribution> VECSCALE(distribution f_, vectortype scale_){
  return VECSCALE_t<distribution>(f_,scale_);
}


/** \brief Separable extension of two densitites

    Take two densities f and g, and construct the density of their separable
    extension, defined as the multivariate Gaussian distribution
    with covariance matrix equal to the kronecker product between
    the covariance matrices of the two distributions.
    Note that f acts on the outermost array dimension and g acts on the fastest
    running array dimension.

    \verbatim
    More precisely: evaluate density 
    h(x)=|S/(2*pi)|^.5*exp(-.5*x'*S*x) 
    where S=kronecker(Q,R)=Q%x%R assuming we have access to densities
    f(x)=|Q/(2*pi)|^.5*exp(-.5*x'*Q*x)
    g(x)=|R/(2*pi)|^.5*exp(-.5*x'*R*x)
    (Note: R corresponds to fastest running array dimension in Q%x%R ...)
    Let nq=nrow(Q) and nr=nrow(R),
    using rules of the kronecker product we have that
    * Quadratic form = .5*x'*S*x = .5*x'*(Q%x%I)*(I%x%R)*x 
    * Normalizing constant = 
    |S/(2*pi)|^.5 = 
    |(Q/sqrt(2*pi))%x%(R/sqrt(2*pi))|^.5 =
    |(Q/sqrt(2*pi))|^(nr*.5) |(R/sqrt(2*pi))|^(nq*.5) =
    ... something that can be expressed through the normalizing
    constants f(0) and g(0) ...
    f(0)^nr * g(0)^nq * sqrt(2*pi)^(nq*nr)
    \endverbatim

    Example:
    \code
    // Separable extension of two AR1 processes
    Type phi1=0.8;
    AR1_t<N01<Type> > f(phi1);
    Type phi2=0.8;
    AR1_t<N01<Type> > g(phi2);
    SEPARABLE_t<AR1_t<N01<Type> > , AR1_t<N01<Type> > > h(f,g);
    // Can be evaluated on an array:
    array<Type> x(10,20);
    Type ans=h(x);
    \endcode

*/ 
//template <class scalartype, class vectortype, class arraytype, class distribution1, class distribution2>
template <class distribution1, class distribution2>
class SEPARABLE_t{
  TYPEDEFS(typename distribution1::scalartype);
private:
  distribution1 f;
  distribution2 g;
public:
  SEPARABLE_t(){}
  SEPARABLE_t(distribution1 f_, distribution2 g_){f=f_;g=g_;}
  /*
    Example: x.dim=[n1,n2,n3].
    Apply f on outer dimension (n3) and rotate:
    [n3,n1,n2]
    Apply g on new outer dimension (n2) and rotate back:
    [n1,n2,n3]
   */
  arraytype jacobian(arraytype x){
    int n=f.ndim();
    x=f.jacobian(x);
    x=x.rotate(n);
    x=g.jacobian(x);
    x=x.rotate(-n);
    return x;
  }
  /* Create zero vector corresponding to the last n dimensions of dimension-vector d */
  arraytype zeroVector(vector<int> d, int n){
    int m=1;
    vector<int> revd=d.reverse();
    vector<int> revnewdim(n);
    for(int i=0;i<n;i++){m=m*revd[i];revnewdim[i]=revd[i];}
    vectortype x(m);
    x.setZero();
    return arraytype(x,revnewdim.reverse());
  }
  scalartype operator()(arraytype x){
    if(this->ndim() != x.dim.size())Rcout << "Wrong dimension in SEPARABLE_t\n";
    /* Calculate quadform */
    arraytype y(x.dim);
    y=jacobian(x);
    y=x*y; /* pointwise */
    scalartype q=scalartype(.5)*(y.sum()); 
    /* Add normalizing constant */
    int n=f.ndim();
    arraytype zf=zeroVector(x.dim,n);
    q+=f(zf)*(scalartype(x.size())/scalartype(zf.size()));
    x=x.rotate(n);
    int m=g.ndim();
    arraytype zg=zeroVector(x.dim,m);
    q+=g(zg)*(scalartype(x.size())/scalartype(zg.size()));
    q-=log(sqrt(2.0*M_PI))*(zf.size()*zg.size());
    /* done */
    return q;
  }
  int ndim(){return f.ndim()+g.ndim();}
  VARIANCE_NOT_YET_IMPLEMENTED

  /* For parallel accumulation:
     ==========================
     Copied operator() above and added extra argument "i" to divide the accumulation
     in chunks. The evaluation of
         operator()(x)
     is equivalent to summing up
         operator()(x,i)
     with i running through the _outer_dimension_ of x.
  */
  scalartype operator()(arraytype x, int i){
    if(this->ndim() != x.dim.size())Rcout << "Wrong dimension in SEPARABLE_t\n";
    /* Calculate quadform */
    arraytype y(x.dim);
    y=jacobian(x);
    y=x*y; /* pointwise */
    scalartype q=scalartype(.5)*(y.col(i).sum()); 
    /* Add normalizing constant */
    if(i==0){
      int n=f.ndim();
      arraytype zf=zeroVector(x.dim,n);
      q+=f(zf)*(scalartype(x.size())/scalartype(zf.size()));
      x=x.rotate(n);
      int m=g.ndim();
      arraytype zg=zeroVector(x.dim,m);
      q+=g(zg)*(scalartype(x.size())/scalartype(zg.size()));
      q-=log(sqrt(2.0*M_PI))*(zf.size()*zg.size());
    }
    /* done */
    return q;
  }

  arraytype sqrt_cov_scale(arraytype u) {
    vector<int> u_dim = u.dim;
    vector<int> f_dim = u_dim.tail(f.ndim());
    vector<int> g_dim = u_dim.head(g.ndim());
    int f_size = f_dim.prod();
    int g_size = g_dim.prod();
    // Collapse f dimension to a single dimension:
    vector<int> new_dim(g.ndim() + 1);
    new_dim << g_dim, f_size;
    u.setdim(new_dim);
    for(int i=0; i<f_size; i++) {
      u.col(i) = g.sqrt_cov_scale( u.col(i) );
    }
    u = u.rotate(1); // u.dim = (f_size, g_dim)
    // Collapse g dimension to a single dimension:
    new_dim.resize(f.ndim() + 1);
    new_dim << f_dim, g_size;
    u.setdim(new_dim);
    for(int i=0; i<g_size; i++) {
      u.col(i) = f.sqrt_cov_scale( u.col(i) );
    }
    u = u.rotate(1);
    u.setdim(u_dim);
    return u;
  }

  void simulate(arraytype &x) {
    rnorm_fill(x);
    x = sqrt_cov_scale(x);
    x = zero_derivatives(x);
  }

};

/** \brief Construct object to evaluate the separable extension of two
    multivariate zero-mean normal densities. See SEPARABLE_t for
    details.

    \param f_ First density object.
    \param g_ Second density object.

    Shortform version to combine two densities using the class \ref SEPARABLE_t .
    Typical use:
    \code
      PARAMETER_ARRAY(x); // 2D array
      SEPARABLE(AR1(phi), MVNORM(Sigma))(x);
    \endcode
    where \c MVNORM(Sigma) acts in the first dimension of x and \c
    AR1(phi) acts in the second dimension of x.
    \note The order of array dimensions is reversed in the sense that
    f_ acts in the *second* dimension and g_ acts in the *first*
    dimension (This order is consistent with how Kronecker products
    work in R).
*/
template <class distribution1, class distribution2>
SEPARABLE_t<distribution1,distribution2> SEPARABLE(distribution1 f_, distribution2 g_){
  return SEPARABLE_t<distribution1,distribution2>(f_,g_);
}

/** \brief Projection of multivariate gaussian variable.

    Preserves sparseness if possible. Generally it is not.

    \verbatim
    Given a gaussian density f:R^n -> R.
    Given an integer vector "proj" with elements in 1,...,n.
    Construct the mariginal density of "x[proj]".
   
    Details:
    --------
    Let x=[x_A]
          [x_B]
    with precision
        Q=[Q_AA  Q_AB]
          [Q_BA  Q_BB]
    and assume that proj=A.
    The marginal density is (with notation 0:=0*x_B )
    p_A(x_A)=p(x_A,x_B)/p(x_B|x_A)=p(x_A,0)/p(0|x_A)
    Now see that
    1. p(x_A,0) is easy because full precision is sparse.
    2. p(0|x_A) is N(-Q_BB^-1 * Q_BA x_A,  Q_BB^-1) so
       p(0|x_A) = |Q_BB|^.5 * exp(-.5*x_A Q_AB * Q_BB^-1 * Q_BA x_A)

       Trick to evaluate this with what we have available:
       Note 1: Q_BA x_A = [0 I_BB] * full_jacobian([ x_A  ]  
                                                   [ 0    ] )


	       Call this quantity "y_B" we have
	       p(0|x_A) = |Q_BB|^.5 * exp(-.5*y_B' * Q_BB^-1 * y_B)

       Note 2: Consider now a density with _covariance_ Q_BB 
               phi(y)=|Q_BB|^-.5 * exp(-.5*y' * Q_BB^-1 * y)
	       Then 
	       phi(y)/phi(0)^2=|Q_BB|^.5 * exp(-.5*y' * Q_BB^-1 * y)
	       which is actually the desired expression of p(0|x_A).

    Summary:
    -------
    Negative log-density of A-marginal is
    -log p(x_A,0) + log phi(y) - 2*log(phi(0))
    = f(x_A,0) - dmvnorm(y_B) + 2*dmvnorm(0)
    \endverbatim
*/
template <class distribution>
class PROJ_t{
  TYPEDEFS(typename distribution::scalartype);
private:
  distribution f;
  bool initialized;
public:
  vector<int> proj;
  vector<int> cproj; /* complementary proj _sorted_ */
  int n,nA,nB;
  matrixtype Q;  /* Full precision */
  MVNORM_t<scalartype> dmvnorm; /* mean zero gaussian with covariance Q_BB */
  PROJ_t(){}
  PROJ_t(distribution f_, vector<int> proj_){
    f=f_;
    proj=proj_;
    initialized=false;
  }
  void initialize(int n_){
    if(!initialized){
      n=n_;
      nA=proj.size();
      nB=n-nA;
      cproj.resize(nB);
      vector<int> mark(n);
      mark.setZero();
      for(int i=0;i<nA;i++)mark[proj[i]]=1;
      int k=0;
      for(int i=0;i<n;i++)if(!mark[i])cproj[k++]=i;
      // Full precision
      //matrixtype I(n,n);
      //I.setIdentity();
      //vectortype v(I);

      k=0;
      vectortype v(n*n);
      for(int i=0;i<n;i++)
	for(int j=0;j<n;j++)
	  v(k++)=scalartype(i==j);

      vector<int> dim(2);
      dim << n,n;
      arraytype a(v,dim);
      a=f.jacobian(a);
      Q.resize(n,n);
      for(int i=0;i<n*n;i++)Q(i)=a[i];
      // Get Q_BB
      matrixtype QBB(nB,nB);
      for(int i=0;i<nB;i++)
	for(int j=0;j<nB;j++)
	  QBB(i,j)=Q(cproj[i],cproj[j]);
      dmvnorm=MVNORM_t<scalartype>(QBB);
    }
    initialized=true;
  }
  vectortype projB(vectortype x){
    vectortype y(nB);
    for(int i=0;i<nB;i++)y[i]=x[cproj[i]];
    return y;
  }
  vectortype setZeroB(vectortype x){
    for(int i=0;i<nB;i++)x[cproj[i]]=scalartype(0);
    return x;    
  }
  scalartype operator()(vectortype x){
    initialize(x.size());
    x=setZeroB(x);
    vector<int> dim(1);
    dim << x.size();
    arraytype xa(x,dim);
    vectortype y=projB(f.jacobian(xa));
    // f(x_A,0) - dmvnorm(y_B) + 2*dmvnorm(0)
    return f(xa) - dmvnorm(y) + 2*dmvnorm(y*scalartype(0));
  }
  /* array versions  */
  arraytype projB(arraytype x){
    vectortype z((x.size()/n)*nB);
    vector<int> dim(x.dim);
    dim[dim.size()-1]=nB;
    arraytype y(z,dim);
    for(int i=0;i<nB;i++)y.col(i)=x.col(cproj[i]);
    return y;
  }
  arraytype setZeroB(arraytype x){
    for(int i=0;i<nB;i++)x.col(cproj[i])=x.col(0)*scalartype(0);
    return x;    
  }
  arraytype jacobian(arraytype x){
    initialize(x.dim[x.dim.size()-1]);
    arraytype xa=setZeroB(x);
    arraytype y=projB(f.jacobian(xa));
    // WRONG: ----> return f.jacobian(xa) - dmvnorm.jacobian(y);
    // y=P*Q*Z*x  so should be  (P*Q*Z)' * dmvnorm.jacobian(y).
    // Note: only P is not symmetric.
    arraytype tmp=f.jacobian(xa);
    arraytype tmp0=tmp*scalartype(0);
    arraytype tmp2=dmvnorm.jacobian(y);
    // apply P'
    for(int i=0;i<nB;i++){ 
      tmp0.col(cproj[i])=tmp2.col(i);
    }
    // apply Q'(=Q)
    tmp0=f.jacobian(tmp0);
    // apply Z'(=Z)
    tmp0=setZeroB(tmp0);
    // Done:
    return tmp-tmp0;
  }
  int ndim(){return f.ndim();}
  VARIANCE_NOT_YET_IMPLEMENTED
  SIMULATE_NOT_YET_IMPLEMENTED
};

template <class distribution>
PROJ_t<distribution> PROJ(distribution f_, vector<int> i){
  return PROJ_t<distribution>(f_,i);
}


#undef TYPEDEFS
