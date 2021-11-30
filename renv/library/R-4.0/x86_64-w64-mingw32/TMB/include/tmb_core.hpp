// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/** \file 
* \brief Interfaces to R and CppAD
*/

/*
  Call to external C++ code can potentially result in exeptions that
  will crash R. However, we do not want R to crash on failed memory
  allocations. Therefore:

  * All interface functions (those called with .Call from R) must have
    TMB_TRY wrapped around CppAD/Eigen code that allocates memory.

  * Special attention must be payed to parallel code, as each thread
    is responsible for catching its own exceptions.
*/

#define TMB_TRY try
#define TMB_CATCH catch(std::bad_alloc& ba)
#define TMB_ERROR_BAD_ALLOC Rf_error("Memory allocation fail in function '%s'\n", \
				  __FUNCTION__)

/* Memory manager:
   Count the number of external pointers alive.
   When total number is zero it is safe to dyn.unload
   the library.
*/
#include <set>
extern "C" void finalizeDoubleFun(SEXP x);
extern "C" void finalizeADFun(SEXP x);
extern "C" void finalizeparallelADFun(SEXP x);
extern "C" SEXP FreeADFunObject(SEXP f) CSKIP ({
  SEXP tag = R_ExternalPtrTag(f);
  if (tag == Rf_install("DoubleFun")) {
    finalizeDoubleFun(f);
  }
  else if (tag == Rf_install("ADFun")) {
    finalizeADFun(f);
  }
  else if (tag == Rf_install("parallelADFun")) {
    finalizeparallelADFun(f);
  }
  else {
    Rf_error("Unknown external ptr type");
  }
  R_ClearExternalPtr(f); // Set pointer to 'nil'
  return R_NilValue;
})
/** \internal \brief Controls the life span of objects created in the C++ template (jointly R/C++)*/
struct memory_manager_struct {
  int counter;
  /** \brief External pointers 'alive', i.e. not yet garbage collected */
  std::set<SEXP> alive;
  /** \brief Register `list` in memory_manager_struct (FIXME: Deprecated) */
  void RegisterCFinalizer(SEXP list);
  /** \brief Removes `x` from memory_manager_struct (FIXME: Rename) */
  void CallCFinalizer(SEXP x);
  /** \brief Free all pointers and set to 'nil' on R side */
  void clear();
  memory_manager_struct();
};
#ifndef WITH_LIBTMB
void memory_manager_struct::RegisterCFinalizer(SEXP x) {
  counter++;
  alive.insert(x);
}
void memory_manager_struct::CallCFinalizer(SEXP x){
  counter--;
  alive.erase(x);
}
void memory_manager_struct::clear(){
  std::set<SEXP>::iterator it;
  while (alive.size() > 0) {
    FreeADFunObject(*alive.begin());
  }
}
memory_manager_struct::memory_manager_struct(){
  counter=0;
}
#endif
TMB_EXTERN memory_manager_struct memory_manager;

/** \internal \brief Convert x to TMB-format for R/C++ communication

   All external pointers returned from TMB should be placed in a 
   list container of length one. Additional information should be set
   as attributes to the pointer. The memory_manager_struct above knows
   how to look up the list container given the external pointer. By 
   setting the list element to NULL the memory_manager can trigger the
   garbage collector (and thereby the finalizers) when the library is
   unloaded.
*/
#ifdef WITH_LIBTMB
SEXP ptrList(SEXP x);
#else
SEXP ptrList(SEXP x)
{
  SEXP ans,names;
  PROTECT(ans=Rf_allocVector(VECSXP,1));
  PROTECT(names=Rf_allocVector(STRSXP,1));
  SET_VECTOR_ELT(ans,0,x);
  SET_STRING_ELT(names,0,Rf_mkChar("ptr"));
  Rf_setAttrib(ans,R_NamesSymbol,names);
  memory_manager.RegisterCFinalizer(x);
  UNPROTECT(2);
  return ans;
}
#endif

extern "C"{
#ifdef LIB_UNLOAD
#include <R_ext/Rdynload.h>
  void LIB_UNLOAD(DllInfo *dll)
  {
    if(memory_manager.counter>0)Rprintf("Warning: %d external pointers will be removed\n",memory_manager.counter);
    memory_manager.clear();
    for(int i=0;i<1000;i++){ // 122 seems to be sufficient.
      if(memory_manager.counter>0){
	R_gc();
	R_RunExitFinalizers();
      } else break;
    }
    if(memory_manager.counter>0)Rf_error("Failed to clean. Please manually clean up before unloading\n");
  }
#endif
}

#ifdef _OPENMP
TMB_EXTERN bool _openmp CSKIP( =true; )
#else
TMB_EXTERN bool _openmp CSKIP( =false; )
#endif

/** \internal \brief Call the optimize method of an ADFun object pointer. */
template<class ADFunPointer>
void optimizeTape(ADFunPointer pf){
  if(!config.optimize.instantly){
    /* Drop out */
    return;
  }
  if (!config.optimize.parallel){
#ifdef _OPENMP
#pragma omp critical
#endif
    { /* Avoid multiple tape optimizations at the same time (to reduce memory) */
      if(config.trace.optimize)Rcout << "Optimizing tape... ";
      pf->optimize();
      if(config.trace.optimize)Rcout << "Done\n";
    }
  }
  else
    { /* Allow multiple tape optimizations at the same time */
      if(config.trace.optimize)Rcout << "Optimizing tape... ";
      pf->optimize();
      if(config.trace.optimize)Rcout << "Done\n";
    }
}

/* Helpers, to check that data and parameters are of the right types.
   "RObjectTester" denotes the type of a pointer to a test function.
   Examples of test functions are "isMatrix", "Rf_isArray", "isNumeric",
   etc (see Rinternals.h).
*/
typedef Rboolean (*RObjectTester)(SEXP);
#ifdef WITH_LIBTMB
void RObjectTestExpectedType(SEXP x, RObjectTester expectedtype, const char *nam);
Rboolean isValidSparseMatrix(SEXP x);
Rboolean isNumericScalar(SEXP x);
#else
void RObjectTestExpectedType(SEXP x, RObjectTester expectedtype, const char *nam){
  if(expectedtype != NULL){
    if(!expectedtype(x)){
      if(Rf_isNull(x)){
	Rf_warning("Expected object. Got NULL.");
      }
      Rf_error("Error when reading the variable: '%s'. Please check data and parameters.",nam);
    }
  }
}
Rboolean isValidSparseMatrix(SEXP x){
  if(!Rf_inherits(x,"dgTMatrix"))Rf_warning("Expected sparse matrix of class 'dgTMatrix'.");
  return Rf_inherits(x,"dgTMatrix");
}
Rboolean isNumericScalar(SEXP x){
  if(LENGTH(x)!=1){
    Rf_warning("Expected scalar. Got length=%i",LENGTH(x));
    return FALSE;
  }
  return Rf_isNumeric(x);
}
#endif

/* Macros to obtain data and parameters from R */

/** \brief Pointer to objective function used by DATA and PARAMETER
    macros.

    PARAMETER and DATA objects can be accessed from other classes and
    functions by redefining this macro.

    Example (incomplete) of use:
    \code
    // Re-define TMB_OBJECTIVE_PTR
    #undef  TMB_OBJECTIVE_PTR
    #define TMB_OBJECTIVE_PTR obj
    template<class Type>
    foo (objective_function<Type> *obj) {
      PARAMETER(a);
      PARAMETER(b);
      return a+b;
    }
    // Restore default TMB_OBJECTIVE_PTR
    #undef  TMB_OBJECTIVE_PTR
    #define TMB_OBJECTIVE_PTR this
    \endcode

    \ingroup macros */
#define TMB_OBJECTIVE_PTR                                               \
this

/** \brief Get parameter matrix from R and declare it as matrix<Type>
    \ingroup macros */
#define PARAMETER_MATRIX(name)                                          \
tmbutils::matrix<Type> name(TMB_OBJECTIVE_PTR -> fillShape(             \
asMatrix<Type> ( TMB_OBJECTIVE_PTR -> getShape( #name, &Rf_isMatrix) ), \
#name) );

/** \brief Get parameter vector from R and declare it as vector<Type> 
    \ingroup macros*/
#define PARAMETER_VECTOR(name)                                          \
vector<Type> name(TMB_OBJECTIVE_PTR -> fillShape(                       \
asVector<Type>(TMB_OBJECTIVE_PTR -> getShape(#name, &Rf_isNumeric)),    \
#name));

/** \brief Get parameter scalar from R and declare it as Type
    \ingroup macros */
#define PARAMETER(name)                                                 \
Type name(TMB_OBJECTIVE_PTR -> fillShape(                               \
asVector<Type>(TMB_OBJECTIVE_PTR -> getShape(#name,&isNumericScalar)),  \
#name)[0]);

/** \brief Get data vector from R and declare it as vector<Type>
    \note If name is found in the parameter list it will be read as a
    parameter vector.
    \ingroup macros */
#define DATA_VECTOR(name)                                               \
vector<Type> name;                                                      \
if (!Rf_isNull(getListElement(TMB_OBJECTIVE_PTR -> parameters,#name))){ \
  name = TMB_OBJECTIVE_PTR -> fillShape(asVector<Type>(                 \
         TMB_OBJECTIVE_PTR -> getShape(#name, &Rf_isNumeric)), #name);  \
} else {                                                                \
  name = asVector<Type>(getListElement(                                 \
         TMB_OBJECTIVE_PTR -> data,#name,&Rf_isNumeric));               \
}

/** \brief Get data matrix from R and declare it as matrix<Type>
    \ingroup macros */
#define DATA_MATRIX(name)                                               \
matrix<Type> name(asMatrix<Type>(                                       \
getListElement(TMB_OBJECTIVE_PTR -> data, #name, &Rf_isMatrix)));

/** \brief Get data scalar from R and declare it as Type
    \ingroup macros */
#define DATA_SCALAR(name)                                               \
Type name(asVector<Type>(getListElement(TMB_OBJECTIVE_PTR -> data,      \
#name,&isNumericScalar))[0]);

/** \brief Get data scalar from R and declare it as int
    \ingroup macros */
#define DATA_INTEGER(name) int name(CppAD::Integer(asVector<Type>(      \
getListElement(TMB_OBJECTIVE_PTR -> data,                               \
#name, &isNumericScalar))[0]));

/** \brief Get data vector of type "factor" from R and declare it as a
    zero-based integer vector.

    The following example (R code) shows what you have on the R side
    and what is being received by the C++ template:

    \verbatim
    > x=factor(letters[4:10])
    > x
    [1] d e f g h i j
    Levels: d e f g h i j

    # The zero-based integer vector that the C++ template sees
    > unclass(x) - 1
    [1] 0 1 2 3 4 5 6
    \endverbatim
    \ingroup macros */
#define DATA_FACTOR(name) vector<int> name(asVector<int>(               \
getListElement(TMB_OBJECTIVE_PTR -> data, #name, &Rf_isNumeric)));

/** \brief Get data vector of type "integer" from R and declare it
    vector<int>. (DATA_INTEGER() is for a scalar integer)
    \ingroup macros */
#define DATA_IVECTOR(name) vector<int> name(asVector<int>(              \
getListElement(TMB_OBJECTIVE_PTR -> data, #name, &Rf_isNumeric)));

/** \brief Get the number of levels of a data factor from R
    \ingroup macros */
#define NLEVELS(name)                                                   \
LENGTH(Rf_getAttrib(getListElement(TMB_OBJECTIVE_PTR -> data, #name),   \
Rf_install("levels")))

/** \brief Get sparse matrix from R and declare it as
    Eigen::SparseMatrix<Type>
    \ingroup macros */
#define DATA_SPARSE_MATRIX(name)                                        \
Eigen::SparseMatrix<Type> name(tmbutils::asSparseMatrix<Type>(          \
getListElement(TMB_OBJECTIVE_PTR -> data,                               \
#name, &isValidSparseMatrix)));

// NOTE: REPORT() constructs new SEXP so never report in parallel!
/** \brief Report scalar, vector or array back to R without derivative
    information.

    \warning \c REPORT(name) must not be used before \c name has been
    assigned a value.
    \note REPORT() does nothing in parallel mode (construction of
    R-objects is not allowed in parallel).
    \ingroup macros */
#define REPORT(name)                                                    \
if( isDouble<Type>::value &&                                            \
    TMB_OBJECTIVE_PTR -> current_parallel_region<0 )                    \
{                                                                       \
    SEXP _TMB_temporary_sexp_;                                          \
    PROTECT( _TMB_temporary_sexp_ = asSEXP(name) );                     \
    Rf_defineVar(Rf_install(#name),                                     \
                 _TMB_temporary_sexp_, TMB_OBJECTIVE_PTR -> report);    \
    UNPROTECT(1);                                                       \
}

/** \brief Mark code that is only executed during simulation.

    \note SIMULATE() does nothing in parallel mode.
    \ingroup macros
*/
#define SIMULATE                                                        \
if(isDouble<Type>::value && TMB_OBJECTIVE_PTR -> do_simulate)

/** \brief Report an expression (scalar, vector, matrix or array valued) back to R with derivative
    information.

    Typical use: obtain point estimate and standard deviation of the expression
    via the R function \c sdreport() (see details in R documentation).
    In the summary, the dimensions of the original expression is lost, and must
    be retrieved manually. 
    \warning \c ADREPORT(name) must not be used before \c name has
    been assigned a value.
    \ingroup macros */
#define ADREPORT(name)                                                  \
TMB_OBJECTIVE_PTR -> reportvector.push(name, #name);

#define PARALLEL_REGION                                                 \
if( TMB_OBJECTIVE_PTR -> parallel_region() )

/** \brief Get data array from R and declare it as array<Type>
    \note If name is found in the parameter list it will be read as a
    parameter array.
    \ingroup macros*/
#define DATA_ARRAY(name)                                                \
tmbutils::array<Type> name;                                             \
if (!Rf_isNull(getListElement(TMB_OBJECTIVE_PTR -> parameters,#name))){ \
  name = TMB_OBJECTIVE_PTR -> fillShape(tmbutils::asArray<Type>(        \
         TMB_OBJECTIVE_PTR -> getShape(#name, &Rf_isArray)), #name);    \
} else {                                                                \
  name = tmbutils::asArray<Type>(getListElement(                        \
         TMB_OBJECTIVE_PTR -> data, #name, &Rf_isArray));               \
}

/** \brief Get parameter array from R and declare it as array<Type>
    \ingroup macros */
#define PARAMETER_ARRAY(name)                                           \
tmbutils::array<Type> name(TMB_OBJECTIVE_PTR -> fillShape(              \
tmbutils::asArray<Type>(TMB_OBJECTIVE_PTR -> getShape(                  \
#name, &Rf_isArray)), #name));

/** \brief Get data matrix from R and declare it as matrix<int>
    \ingroup macros */
#define DATA_IMATRIX(name)                                              \
matrix<int> name(asMatrix<int>(                                         \
getListElement(TMB_OBJECTIVE_PTR -> data,#name, &Rf_isMatrix)));

/** \brief Get data array from R and declare it as array<int>
    \ingroup macros */
#define DATA_IARRAY(name)                                               \
tmbutils::array<int> name(tmbutils::asArray<int>(                       \
getListElement(TMB_OBJECTIVE_PTR -> data, #name, &Rf_isArray)));

/** \brief Get string from R and declare it as std::string

    Example (incomplete) of use:
    \code
    vector<std::string> options(2);
    options << "apple", "orange";
    DATA_STRING(choice);
    if(! (choice == options).any() )
      Rf_error( ("'" + choice + "'" + " not valid").c_str() );
    \endcode

    \ingroup macros
*/
#define DATA_STRING(name)                                               \
std::string name =                                                      \
  CHAR(STRING_ELT(getListElement(TMB_OBJECTIVE_PTR -> data, #name), 0));

/** \brief Get data list object from R and make it available in C++

Example (incomplete) of use:

In R: 
\code
data <- list()
data$object <- list(a=1:10, b=matrix(1:6,2))
obj <- MakeADFun(data,........) 
\endcode

In C++: 
\code
// Corresponding list object on the C++ side
template<class Type>
struct my_list {
  vector<Type> a;
  matrix<Type> b;
  my_list(SEXP x){ // Constructor
    a = asVector<Type>(getListElement(x,"a"));
    b = asMatrix<Type>(getListElement(x,"b"));
  }
};

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_STRUCT(object, my_list);
  REPORT(object.a); // Now you can use "a" and "b" as you like
  REPORT(object.b);
  return 0;
}
\endcode
\ingroup macros
*/ 
#define DATA_STRUCT(name, struct)                                       \
struct<Type> name(getListElement(TMB_OBJECTIVE_PTR -> data, #name));

/** \brief Utilities for OSA residuals
    \tparam VT Can be **vector<Type>** or **array<Type>**
    \warning When extracting subsets of a `data_indicator` note that in general the subset is not applied to `cdf_lower` and `cdf_upper`.
*/
template<class VT, class Type>
struct data_indicator : VT{
  /** \brief **Logarithm** of lower CDF */
  VT cdf_lower;
  /** \brief **Logarithm** of upper CDF */
  VT cdf_upper;
  /** \brief Default CTOR */
  data_indicator() { }
  /** \brief Construct from observation vector
      \param obs Observation vector or array
      \param init_one If true the data_indicator will be filled with ones signifying that all observations should be enabled.
  */
  data_indicator(VT obs, bool init_one = false){
    VT::operator=(obs);
    if (init_one) VT::fill(Type(1.0));
    cdf_lower = obs; cdf_lower.setZero();
    cdf_upper = obs; cdf_upper.setZero();
  }
  /** \brief Fill with parameter vector */
  void fill(vector<Type> p){
    int n = (*this).size();
    if(p.size() >= n  ) VT::operator=(p.segment(0, n));
    if(p.size() >= 2*n) cdf_lower = p.segment(n, n);
    if(p.size() >= 3*n) cdf_upper = p.segment(2 * n, n);
  }
  /** \brief Extract segment of indicator vector or array
      \note For this method the segment **is** applied to `cdf_lower` and `cdf_upper`. */
  data_indicator segment(int pos, int n) {
    data_indicator ans ( VT::segment(pos, n) );
    ans.cdf_lower = cdf_lower.segment(pos, n);
    ans.cdf_upper = cdf_upper.segment(pos, n);
    return ans;
  }
};

/** \brief Declare an indicator array 'name' of same shape as 'obs'. By default, the indicator array is filled with ones indicating that all observations are enabled.
    \details
    This is used in conjunction with one-step-ahead residuals - see
    ?oneStepPredict
    \ingroup macros */
#define DATA_ARRAY_INDICATOR(name, obs)                                 \
data_indicator<tmbutils::array<Type>, Type > name(obs, true);           \
if (!Rf_isNull(getListElement(TMB_OBJECTIVE_PTR -> parameters,#name))){ \
  name.fill( TMB_OBJECTIVE_PTR -> fillShape(asVector<Type>(             \
             TMB_OBJECTIVE_PTR -> getShape(#name, &Rf_isNumeric)),      \
                                           #name) );                    \
}

/** \brief Declare an indicator vector 'name' of same shape as 'obs'. By default, the indicator vector is filled with ones indicating that all observations are enabled.
    \details
    This is used in conjunction with one-step-ahead residuals - see
    ?oneStepPredict
    \ingroup macros */
#define DATA_VECTOR_INDICATOR(name, obs)                                \
data_indicator<tmbutils::vector<Type>, Type > name(obs, true);          \
if (!Rf_isNull(getListElement(TMB_OBJECTIVE_PTR -> parameters,#name))){ \
  name.fill( TMB_OBJECTIVE_PTR -> fillShape(asVector<Type>(             \
             TMB_OBJECTIVE_PTR -> getShape(#name, &Rf_isNumeric)),      \
                                           #name) );                    \
}

// kasper: Not sure used anywhere
/** \internal \brief Get the hessian sparsity pattern of ADFun object pointer
\deprecated Kasper is not sure that this code is used anywhere? 
*/
template<class Type>
matrix<int> HessianSparsityPattern(ADFun<Type> *pf){
  int n=pf->Domain();
  vector<bool> Px(n * n);
  for(int i = 0; i < n; i++)
    {
      for(int j = 0; j < n; j++)
	Px[ i * n + j ] = false;
      Px[ i * n + i ] = true;
    }
  pf->ForSparseJac(n, Px);
  vector<bool> Py(1); Py[0]=true;
  vector<int> tmp = (pf->RevSparseHes(n,Py)).template cast<int>();
  return asMatrix(tmp, n, n);
}

/** \internal \brief Get list element named "str", or return NULL */
#ifdef WITH_LIBTMB
SEXP getListElement(SEXP list, const char *str, RObjectTester expectedtype=NULL);
int  getListInteger(SEXP list, const char *str, int default_value = 0);
#else
SEXP getListElement(SEXP list, const char *str, RObjectTester expectedtype=NULL)
{
  if(config.debug.getListElement)Rcout << "getListElement: " << str << " ";
  SEXP elmt = R_NilValue, names = Rf_getAttrib(list, R_NamesSymbol);
  int i; 
  for (i = 0; i < Rf_length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) 
      {
	elmt = VECTOR_ELT(list, i); 
	break; 
      }
  if(config.debug.getListElement)Rcout << "Length: " << LENGTH(elmt) << " ";
  if(config.debug.getListElement)Rcout << "\n";
  RObjectTestExpectedType(elmt, expectedtype, str);
  return elmt; 
}
int getListInteger(SEXP list, const char *str, int default_value = 0) {
  SEXP tmp = getListElement(list, str);
  if ( tmp == R_NilValue ) {
    Rf_warning("Missing integer variable '%s'. Using default: %d. (Perhaps you are using a model object created with an old TMB version?)", str, default_value);
    return default_value;
  }
  return INTEGER(tmp)[0];
}
#endif

/** \internal \brief Do nothing if we are trying to tape non AD-types */
void Independent(vector<double> x)CSKIP({})

/** \internal \brief Used by ADREPORT */
template <class Type>
struct report_stack{
  vector<const char*> names;
  vector<vector<int> > namedim;
  vector<Type> result;
  void clear(){
    names.resize(0);
    namedim.resize(0);
    result.resize(0);
  }
  /* Make space for 'prod(dim)' new items of given name */
  void increase(vector<int> dim, const char* name){
    int n = dim.prod();
    names.conservativeResize(names.size() + 1);
    names[names.size() - 1] = name;
    namedim.conservativeResize(namedim.size() + 1);
    namedim[namedim.size() - 1] = dim;
    result.conservativeResize(result.size() + n);
  }
  // Get dimension of various object types
  vector<int> getDim(const matrix<Type> &x) {
    vector<int> dim(2);
    dim << x.rows(), x.cols();
    return dim;
  }
  vector<int> getDim(const tmbutils::array<Type> &x) {
    return x.dim;
  }
  template<class Other> // i.e. vector or expression
  vector<int> getDim(const Other &x) {
    vector<int> dim(1);
    dim << x.size();
    return dim;
  }
  // push vector, matrix or array
  template<class Vector_Matrix_Or_Array>
  void push(Vector_Matrix_Or_Array x, const char* name) {
    int n = x.size();
    int oldsize = result.size();
    vector<int> dim = getDim(x);
    increase(dim, name);
    Eigen::Array<Type, Eigen::Dynamic, Eigen::Dynamic> xvec = x;
    xvec.resize(xvec.size(), 1);
    result.segment(oldsize, n) = xvec;
  }
  // push scalar (convert to vector case)
  void push(Type x, const char* name){
    vector<Type> xvec(1);
    xvec[0] = x;
    push(xvec, name);
  }
  // Cast to vector
  operator vector<Type>(){
    return result;
  }
  /* Get names (with replicates) to R */
  SEXP reportnames()
  {
    int n = result.size();
    SEXP nam;
    PROTECT( nam = Rf_allocVector(STRSXP, n) );
    int k = 0;
    for(int i = 0; i < names.size(); i++) {
      int namelength = namedim(i).prod();
      for(int j = 0; j < namelength; j++) {
        SET_STRING_ELT(nam, k, Rf_mkChar(names[i]) );
        k++;
      }
    }
    UNPROTECT(1);
    return nam;
  }
  /* Get AD reported object dims */
  SEXP reportdims() {
    SEXP ans, nam;
    PROTECT( ans = asSEXP(namedim) );
    PROTECT( nam = Rf_allocVector(STRSXP, names.size()) );
    for(int i = 0; i < names.size(); i++) {
      SET_STRING_ELT(nam, i, Rf_mkChar(names[i]));
    }
    Rf_setAttrib(ans, R_NamesSymbol, nam);
    UNPROTECT(2);
    return ans;
  }
  EIGEN_DEFAULT_DENSE_INDEX_TYPE size(){return result.size();}
};  // report_stack

extern "C" {
  void GetRNGstate(void);
  void PutRNGstate(void);
}

/** \internal \brief Type definition of user-provided objective function (i.e. neg. log. like) */
template <class Type>
class objective_function
{
// private:
public:
  SEXP data;
  SEXP parameters;
  SEXP report;
  
  int index;
  vector<Type> theta; /**< \brief Consists of unlist(parameters_)*/ 
  vector<const char*> thetanames; /**< \brief In R notation: names(theta). Contains repeated values*/ 
  report_stack<Type> reportvector; /**< \brief Used by "ADREPORT" */
  bool reversefill; // used to find the parameter order in user template (not anymore - use pushParname instead)
  vector<const char*> parnames; /**< \brief One name for each PARAMETER_ in user template */

/** \brief Called once for each occurance of PARAMETER_ */
  void pushParname(const char* x){
    parnames.conservativeResize(parnames.size()+1);
    parnames[parnames.size()-1]=x;
  }

  /* ================== For parallel Hessian computation
     Need three different parallel evaluation modes:
     (1) *Parallel mode* where a parallel region is evaluated iff 
         current_parallel_region == selected_parallel_region
     (2) *Serial mode* where all parallel region tests are evaluated 
         to TRUE so that "PARALLEL_REGION" tests are effectively removed.
	 A negative value of "current_parallel_region" or "selected_parallel_region" 
	 is used to select this mode (the default).
     (3) *Count region mode* where statements inside "PARALLEL_REGION{...}"
         are *ignored* and "current_parallel_region" is increased by one each
	 time a parallel region is visited.
     NOTE: The macro "PARALLEL_REGION" is supposed to be defined as
           #define PARALLEL_REGION if(this->parallel_region())
	   where the function "parallel_region" does the book keeping.
   */
  bool parallel_ignore_statements;
  int current_parallel_region;       /* Identifier of a code-fragment of user template */
  int selected_parallel_region;      /* Consider _this_ code-fragment */
  int max_parallel_regions;          /* Max number of parallel region identifiers,
				        e.g. max_parallel_regions=omp_get_max_threads(); 
				        probably best in most cases. */
  bool parallel_region(){            /* Is this the selected parallel region ? */
    bool ans;
    if(current_parallel_region<0 || selected_parallel_region<0)return true; /* Serial mode */
    ans = (selected_parallel_region==current_parallel_region) && (!parallel_ignore_statements);
    current_parallel_region++;
    if(max_parallel_regions>0)current_parallel_region=current_parallel_region % max_parallel_regions;
    return ans;
  }
  /* Note: Some other functions rely on "count_parallel_regions" to run through the users code (!) */
  int count_parallel_regions(){
    current_parallel_region=0;       /* reset counter */
    selected_parallel_region=0;
    parallel_ignore_statements=true; /* Do not evaluate stuff inside PARALLEL_REGION{...} */
    this->operator()();              /* Run through users code */
    if(max_parallel_regions>0)return max_parallel_regions;
    else
    return current_parallel_region;
  }
  void set_parallel_region(int i){   /* Select parallel region (from within openmp loop) */
    current_parallel_region=0;
    selected_parallel_region=i;
    parallel_ignore_statements=false;
  }

  bool do_simulate;   /** \brief Flag set when in simulation mode */
  void set_simulate(bool do_simulate_) {
    do_simulate = do_simulate_;
  }

  /* data_ and parameters_ are R-lists containing R-vectors or R-matrices.
     report_ is an R-environment.  
     The elements of the vector "unlist(parameters_)" are filled into "theta"
     which contains the default parameter-values. This happens during the
     *construction* of the objective_function object.
     The user defined template "objective_function::operator()" is called
     from "MakeADFunObject" which tapes the operations and creates the final
     ADFun-object.
  */
  /** \brief Constructor which among other things gives a value to "theta" */
  objective_function(SEXP data, SEXP parameters, SEXP report) :
    data(data), parameters(parameters), report(report), index(0)
  {
    /* Fill theta with the default parameters. 
       Pass R-matrices column major. */
    theta.resize(nparms(parameters));
    int length_parlist = Rf_length(parameters);
    for(int i = 0, counter = 0; i < length_parlist; i++) {
      // x = parameters[[i]]
      SEXP x = VECTOR_ELT(parameters, i);
      int nx = Rf_length(x);
      double* px = REAL(x);
      for(int j = 0; j < nx; j++) {
        theta[counter++] = Type( px[j] );
      }
    }
    thetanames.resize(theta.size());
    for(int i=0;i<thetanames.size();i++)thetanames[i]="";
    current_parallel_region=-1;
    selected_parallel_region=-1;
    max_parallel_regions=-1;
    reversefill=false;
    do_simulate = false;
    GetRNGstate(); /* Read random seed from R. Note: by default we do
                      not write the seed back to R *after*
                      simulation. This ensures that multiple tapes for
                      one model object get the same seed. When in
                      simulation mode (enabled when calling
                      obj$simulate() from R) we *do* write the seed
                      back after simulation in order to get varying
                      replicates. */
  }

  /** \brief Syncronize user's data object. It could be changed between calls to e.g. EvalDoubleFunObject */
  void sync_data() {
    SEXP env = ENCLOS(this->report);
    this->data = findVar(install("data"), env);
  }

  /** \brief Extract theta vector from objetive function object */
  SEXP defaultpar()
  {
    int n=theta.size();
    SEXP res;
    SEXP nam;
    PROTECT(res=Rf_allocVector(REALSXP,n));
    PROTECT(nam=Rf_allocVector(STRSXP,n));
    for(int i=0;i<n;i++){
      //REAL(res)[i]=CppAD::Value(theta[i]);
      REAL(res)[i]=value(theta[i]);
      SET_STRING_ELT(nam,i,Rf_mkChar(thetanames[i]));
    }
    Rf_setAttrib(res,R_NamesSymbol,nam);
    UNPROTECT(2);
    return res;
  }

  /** \brief Extract parnames vector from objetive function object */
  SEXP parNames()
  {
    int n=parnames.size();
    SEXP nam;
    PROTECT(nam=Rf_allocVector(STRSXP,n));
    for(int i=0;i<n;i++){
      SET_STRING_ELT(nam,i,Rf_mkChar(parnames[i]));
    }
    UNPROTECT(1);
    return nam;
  }
  
  /* FIXME: "Value" should be "var2par" I guess 
     kasper: Why not use asDouble defined previously? */
  /** @name Value Functions
      Overloaded functions to extract the value from objects of various types;
      generally wrappers for CppAD::Value(x).
      @{
   */
  /** Extracts the value of from TMB objects.
  \param x The variable to be extracted.
  \return Object of type double containing the value of the argument.
  */
  double value(double x){return x;}
  double value(AD<double> x){return CppAD::Value(x);}
  double value(AD<AD<double> > x){return CppAD::Value(CppAD::Value(x));}
  double value(AD<AD<AD<double> > > x){return CppAD::Value(CppAD::Value(CppAD::Value(x)));}
  /** @} */

  /** \brief Find the length of theta, i.e. in application obj=parameters */
  int nparms(SEXP obj)
  {
    int count=0;
    for(int i=0;i<Rf_length(obj);i++){
      if(!Rf_isReal(VECTOR_ELT(obj,i)))Rf_error("PARAMETER COMPONENT NOT A VECTOR!");
      count+=Rf_length(VECTOR_ELT(obj,i));
    }
    return count;
  }

  /* The "fill functions" are all used to populate parameter vectors,
     arrays, matrices etc with the values of the parameter vector theta. */
  void fill(vector<Type> &x, const char *nam)
  {
    pushParname(nam);
    for(int i=0;i<x.size();i++){
      thetanames[index]=nam;
      if(reversefill)theta[index++]=x[i];else x[i]=theta[index++];
    }
  }
  void fill(matrix<Type> &x, const char *nam)
  {
    pushParname(nam);
    for(int j=0;j<x.cols();j++){
      for(int i=0;i<x.rows();i++){
	thetanames[index]=nam;
	if(reversefill)theta[index++]=x(i,j);else x(i,j)=theta[index++];
      }
    }
  }
  template<class ArrayType>
  void fill(ArrayType &x, const char *nam)
  {
    pushParname(nam);
    for(int i=0;i<x.size();i++){
	thetanames[index]=nam;
	if(reversefill)theta[index++]=x[i];else x[i]=theta[index++];
    }
  }

  /* Experiment: new map feature - currently arrays only */
  template<class ArrayType>
  void fillmap(ArrayType &x, const char *nam)
  {
    pushParname(nam);
    SEXP elm=getListElement(parameters,nam);
    int* map=INTEGER(Rf_getAttrib(elm,Rf_install("map")));
    int  nlevels=INTEGER(Rf_getAttrib(elm,Rf_install("nlevels")))[0];
    for(int i=0;i<x.size();i++){
      if(map[i]>=0){
	thetanames[index+map[i]]=nam;
	if(reversefill)theta[index+map[i]]=x(i);else x(i)=theta[index+map[i]];
      }
    }
    index+=nlevels;
  }
  // Auto detect whether we are in "map-mode"
  SEXP getShape(const char *nam, RObjectTester expectedtype=NULL){
    SEXP elm=getListElement(parameters,nam);
    SEXP shape=Rf_getAttrib(elm,Rf_install("shape"));
    SEXP ans;
    if(shape==R_NilValue)ans=elm; else ans=shape;
    RObjectTestExpectedType(ans, expectedtype, nam);
    return ans;
  }
  template<class ArrayType>
  //ArrayType fillShape(ArrayType &x, const char *nam){
  ArrayType fillShape(ArrayType x, const char *nam){
    SEXP elm=getListElement(parameters,nam);
    SEXP shape=Rf_getAttrib(elm,Rf_install("shape"));
    if(shape==R_NilValue)fill(x,nam);
    else fillmap(x,nam);
    return x;
  }

  void fill(Type &x, char const *nam)
  {
    pushParname(nam);
    thetanames[index]=nam;
    if(reversefill)theta[index++]=x;else x=theta[index++];
  }
   
  Type operator() ();

  Type evalUserTemplate(){
    Type ans=this->operator()();
    /* After evaluating the template, "index" should be equal to the length of "theta".
       If not, we assume that the "epsilon method" has been requested from R, I.e.
       that the un-used theta parameters are reserved for an inner product contribution
       with the numbers reported via ADREPORT. */
    if(index != theta.size()){
      PARAMETER_VECTOR( TMB_epsilon_ );
      ans += ( this->reportvector.result * TMB_epsilon_ ).sum();
    }
    return ans;
  }

}; // objective_function

/** \brief Helper to manage parallel accumulation.

    - A parallel accumulator only has two valid operators: increment
      (+=) and decrement (-=). Other usage would break the assumptions
      underlying the parallel accumulator.
    - In particular, direct assignment is forbidden. The parallel
      accumulator is automatically zero initialized.

    Example:
    \code
    // Initialize a variable for parallel summation:
    parallel_accumulator<Type> res(this);

    // Only two valid methods:
    res += ...;
    res -= ...;

    // Automatic cast to Type when exit from function
    // (cast to Type is not allowed elsewhere):
    return res;
    \endcode

    \note It is only recommended to apply the parallel accumulator for
    models that are known to work in serial (debugging becomes
    substantially more difficult with parallel accumulation turned
    on).

    \ingroup parallel
*/
template<class Type>
struct parallel_accumulator{
  Type result;
  objective_function<Type>* obj;
  parallel_accumulator(objective_function<Type>* obj_){
    result=Type(0);
    obj=obj_;
#ifdef _OPENMP
#include <omp.h>
    obj->max_parallel_regions=omp_get_max_threads();
#endif
  }
  inline void operator+=(Type x){
    if(obj->parallel_region())result+=x;
  }
  inline void operator-=(Type x){
    if(obj->parallel_region())result-=x;
  }
  operator Type(){
    return result;
  }
};


#ifndef WITH_LIBTMB

/** \internal \brief Evaluates an ADFun object from R

   Template argument can be "ADFun" or an object extending
   "ADFun" such as "parallelADFun".
   @param f R external pointer to ADFunType
   @param theta R vector of parameters
   @param control R list controlling what to be returned

   It is assumed that \f$f:R^n \rightarrow R^m\f$ where n and m are found from f.
   The list "control" can contain the following components:

   * order: mandatory integer 0,1,2, or 3 with order of derivatives to be calculated.\n
   * hessiancols, hessianrows: Optional one-based integer vectors of the same length.
     Used only in the case where order=2 to extract specific entries of hessian.\n
   * sparsitypattern: Integer flag. Return sparsity pattern instead of numerical values?\n
   * rangeweight: Optional R vector of doubles of length m. If supplied, a 1st order reverse
     mode sweep is performed in this range direction.\n
   * rangecomponent: Optional one-based integer (scalar) between 1 and m. Used to select a
     given component of the vector f(x).
   * dumpstack: Integer flag. If non zero the entire operation stack is dumped as text output
     during 0-order forward sweep.

   Possible output depends on "order".

   * order==0: Calculate f(x) output as vector of length m.\n
   * order==1: If "rangeweight" is supplied, calculate the gradient of the function
   x -> inner_prod(f(x),w) from R^n->R.
   Otherwise, calculate the full jacobian (of dimension m*n).\n
   * order==2: If nothing further is specified, calculate the full hessian of the function
   x->f(x)[rangecomponent] from R^n->R

   All other usage is considered deprecated/experimental and may be removed in the future.

*/
template<class ADFunType>
SEXP EvalADFunObjectTemplate(SEXP f, SEXP theta, SEXP control)
{
  if(!Rf_isNewList(control))Rf_error("'control' must be a list");
  ADFunType* pf;
  pf=(ADFunType*)R_ExternalPtrAddr(f);
  PROTECT(theta=Rf_coerceVector(theta,REALSXP));
  int n=pf->Domain();
  int m=pf->Range();
  if(LENGTH(theta)!=n)Rf_error("Wrong parameter length.");
  // Do forwardsweep ?
  int doforward = getListInteger(control, "doforward", 1);
  //R-index -> C-index
  int rangecomponent = getListInteger(control, "rangecomponent", 1) - 1;
  if(!((0<=rangecomponent)&(rangecomponent<=m-1)))
    Rf_error("Wrong range component.");
  int order = getListInteger(control, "order");
  if((order!=0) & (order!=1) & (order!=2) & (order!=3))
    Rf_error("order can be 0, 1, 2 or 3");
  int sparsitypattern = getListInteger(control, "sparsitypattern");
  int dumpstack = getListInteger(control, "dumpstack");
  SEXP hessiancols; // Hessian columns
  PROTECT(hessiancols=getListElement(control,"hessiancols"));
  int ncols=Rf_length(hessiancols);
  SEXP hessianrows; // Hessian rows
  PROTECT(hessianrows=getListElement(control,"hessianrows"));
  int nrows=Rf_length(hessianrows);
  if((nrows>0)&(nrows!=ncols))Rf_error("hessianrows and hessianrows must have same length");
  vector<size_t> cols(ncols);
  vector<size_t> cols0(ncols);
  vector<size_t> rows(nrows);
  if(ncols>0){
    for(int i=0;i<ncols;i++){
      cols[i]=INTEGER(hessiancols)[i]-1; //R-index -> C-index
      cols0[i]=0;
      if(nrows>0)rows[i]=INTEGER(hessianrows)[i]-1; //R-index -> C-index
    }
  }
  vector<double> x = asVector<double>(theta);
  SEXP res=R_NilValue;
  SEXP rangeweight=getListElement(control,"rangeweight");
  if(rangeweight!=R_NilValue){
    if(LENGTH(rangeweight)!=m)Rf_error("rangeweight must have length equal to range dimension");
    if(doforward)pf->Forward(0,x);
    res=asSEXP(pf->Reverse(1,asVector<double>(rangeweight)));
    UNPROTECT(3);
    return res;
  }
  if(order==3){
    vector<double> w(1);
    w[0]=1;
    if((nrows!=1) | (ncols!=1))Rf_error("For 3rd order derivatives a single hessian coordinate must be specified.");
    pf->ForTwo(x,rows,cols); /* Compute forward directions */
    PROTECT(res=asSEXP(asMatrix(pf->Reverse(3,w),n,3)));
  }
  if(order==0){
    if(dumpstack)CppAD::traceforward0sweep(1);
    PROTECT(res=asSEXP(pf->Forward(0,x)));
    if(dumpstack)CppAD::traceforward0sweep(0);
    SEXP rangenames=Rf_getAttrib(f,Rf_install("range.names"));
    if(LENGTH(res)==LENGTH(rangenames)){
      Rf_setAttrib(res,R_NamesSymbol,rangenames);
    }
  }
  if(order==1){
    if(doforward)pf->Forward(0,x);
    matrix<double> jac(m, n);
    vector<double> u(n);
    vector<double> v(m);
    v.setZero();
    for(int i=0; i<m; i++) {
      v[i] = 1.0; u = pf->Reverse(1,v);
      v[i] = 0.0;
      jac.row(i) = u;
    }
    PROTECT( res = asSEXP(jac) );
  }
  //if(order==2)res=asSEXP(pf->Hessian(x,0),1);
  if(order==2){
    if(ncols==0){
      if(sparsitypattern){
	PROTECT(res=asSEXP(HessianSparsityPattern(pf)));  
      } else {
	PROTECT(res=asSEXP(asMatrix(pf->Hessian(x,rangecomponent),n,n)));
      }
    }
    else if (nrows==0){
      /* Fixme: the cols0 argument should be user changeable */
      PROTECT(res=asSEXP(asMatrix(pf->RevTwo(x,cols0,cols),n,ncols)));
    }
    else PROTECT(res=asSEXP(asMatrix(pf->ForTwo(x,rows,cols),m,ncols)));
  }
  UNPROTECT(4);
  return res;
} // EvalADFunObjectTemplate

/** \internal \brief Garbage collect an ADFun or parallelADFun object pointer */
template <class ADFunType>
void finalize(SEXP x)
{
  ADFunType* ptr=(ADFunType*)R_ExternalPtrAddr(x);
  if(ptr!=NULL)delete ptr;
  memory_manager.CallCFinalizer(x);
}


/** \internal \brief Construct ADFun object */
ADFun<double>* MakeADFunObject_(SEXP data, SEXP parameters,
			       SEXP report, SEXP control, int parallel_region=-1,
			       SEXP &info=R_NilValue)
{
  int returnReport = getListInteger(control, "report");
  /* Create objective_function "dummy"-object */
  objective_function< AD<double> > F(data,parameters,report);
  F.set_parallel_region(parallel_region);
  /* Create ADFun pointer.
     We have the option to tape either the value returned by the
     objective_function template or the vector reported using the
     macro "ADREPORT" */
  Independent(F.theta);  // In both cases theta is the independent variable
  ADFun< double >* pf;
  if(!returnReport){ // Default case: no ad report - parallel run allowed
    vector< AD<double> > y(1);
    y[0]=F.evalUserTemplate();
    pf = new ADFun< double >(F.theta,y);
  } else { // ad report case
    F(); // Run through user template (modifies reportvector)
    pf = new ADFun< double >(F.theta,F.reportvector.result);
    info=F.reportvector.reportnames(); // parallel run *not* allowed
  }
  return pf;
}


extern "C"
{

  /** \internal \brief Garbage collect an ADFun object pointer */
  void finalizeADFun(SEXP x)
  {
    ADFun<double>* ptr=(ADFun<double>*)R_ExternalPtrAddr(x);
    if(ptr!=NULL)delete ptr;
    memory_manager.CallCFinalizer(x);
  }
  void finalizeparallelADFun(SEXP x)
  {
    parallelADFun<double>* ptr=(parallelADFun<double>*)R_ExternalPtrAddr(x);
    if(ptr!=NULL)delete ptr;
    memory_manager.CallCFinalizer(x);
  }

  /** \internal \brief Construct ADFun object */
  SEXP MakeADFunObject(SEXP data, SEXP parameters,
		       SEXP report, SEXP control)
  {
    ADFun<double>* pf = NULL;
    /* Some type checking */
    if(!Rf_isNewList(data))Rf_error("'data' must be a list");
    if(!Rf_isNewList(parameters))Rf_error("'parameters' must be a list");
    if(!Rf_isEnvironment(report))Rf_error("'report' must be an environment");
    if(!Rf_isNewList(control))Rf_error("'control' must be a list");
    int returnReport = getListInteger(control, "report");

    /* Get the default parameter vector (tiny overhead) */
    SEXP par,res=NULL,info;
    objective_function< double > F(data,parameters,report);
#ifdef _OPENMP
    int n=F.count_parallel_regions(); // Evaluates user template
#else
    F.count_parallel_regions(); // Evaluates user template
#endif
    if(returnReport && F.reportvector.size()==0){
      /* Told to report, but no ADREPORT in template: Get out quickly */
      return R_NilValue;
    }
    PROTECT(par=F.defaultpar());
    PROTECT(info=R_NilValue); // Important

    if(_openmp && !returnReport){ // Parallel mode
#ifdef _OPENMP
      if(config.trace.parallel)
	Rcout << n << " regions found.\n";
      start_parallel(); /* Start threads */
      vector< ADFun<double>* > pfvec(n);
      bool bad_thread_alloc = false;
#pragma omp parallel for if (config.tape.parallel)
      for(int i=0;i<n;i++){
	TMB_TRY {
	  pfvec[i] = NULL;
	  pfvec[i] = MakeADFunObject_(data, parameters, report, control, i, info);
	  if (config.optimize.instantly) pfvec[i]->optimize();
	}
	TMB_CATCH { bad_thread_alloc = true; }
      }
      if(bad_thread_alloc){
	for(int i=0; i<n; i++) if (pfvec[i] != NULL) delete pfvec[i];
	TMB_ERROR_BAD_ALLOC;
      }
      parallelADFun<double>* ppf=new parallelADFun<double>(pfvec);
      /* Convert parallel ADFun pointer to R_ExternalPtr */
      PROTECT(res=R_MakeExternalPtr((void*) ppf,Rf_install("parallelADFun"),R_NilValue));
#endif
    } else { // Serial mode
      TMB_TRY{
	/* Actual work: tape creation */
	pf = NULL;
	pf = MakeADFunObject_(data, parameters, report, control, -1, info);
	if (config.optimize.instantly) pf->optimize();
      }
      TMB_CATCH {
	if (pf != NULL) delete pf;
	TMB_ERROR_BAD_ALLOC;
      }
      /* Convert ADFun pointer to R_ExternalPtr */
      PROTECT(res=R_MakeExternalPtr((void*) pf,Rf_install("ADFun"),R_NilValue));
      Rf_setAttrib(res,Rf_install("range.names"),info);
    }

    /* Return list of external pointer and default-parameter */
    SEXP ans;
    Rf_setAttrib(res,Rf_install("par"),par);
    PROTECT(ans=ptrList(res));
    UNPROTECT(4);

    return ans;
  } // MakeADFunObject
  
  SEXP InfoADFunObject(SEXP f)
  {
    ADFun<double>* pf;
    pf = (ADFun<double>*) R_ExternalPtrAddr(f);
    SEXP ans, names;
    PROTECT(ans = Rf_allocVector(VECSXP, 12));
    PROTECT(names = Rf_allocVector(STRSXP, 12));
    int i = 0;
#define GET_MORE_INFO(MEMBER)                           \
    SET_VECTOR_ELT(ans, i, asSEXP(int(pf->MEMBER())));  \
    SET_STRING_ELT(names, i, Rf_mkChar(#MEMBER));       \
    i++;
    GET_MORE_INFO(Domain);
    GET_MORE_INFO(Range);
    GET_MORE_INFO(size_op);
    GET_MORE_INFO(size_op_arg);
    GET_MORE_INFO(size_op_seq);
    GET_MORE_INFO(size_par);
    GET_MORE_INFO(size_order);
    GET_MORE_INFO(size_direction);
    GET_MORE_INFO(size_text);
    GET_MORE_INFO(size_var);
    GET_MORE_INFO(size_VecAD);
    GET_MORE_INFO(Memory);
#undef GET_MORE_INFO
    Rf_setAttrib(ans,R_NamesSymbol,names);
    UNPROTECT(2);
    return ans;
  }

  /** \internal \brief Call tape optimization function in CppAD */
  SEXP optimizeADFunObject(SEXP f)
  {
    SEXP tag=R_ExternalPtrTag(f);
    if(tag == Rf_install("ADFun")){
      ADFun<double>* pf;
      pf=(ADFun<double>*)R_ExternalPtrAddr(f);
      pf->optimize();
    }
    if(tag == Rf_install("parallelADFun")){
      parallelADFun<double>* pf;
      pf=(parallelADFun<double>*)R_ExternalPtrAddr(f);
      pf->optimize();      
    }
    return R_NilValue;
  }

  /** \internal \brief Get tag of external pointer */
  SEXP getTag(SEXP f){
    return R_ExternalPtrTag(f);
  }

  SEXP EvalADFunObject(SEXP f, SEXP theta, SEXP control)
  {
    TMB_TRY {
      if(Rf_isNull(f))Rf_error("Expected external pointer - got NULL");
      SEXP tag=R_ExternalPtrTag(f);
      if(tag == Rf_install("ADFun"))
	return EvalADFunObjectTemplate<ADFun<double> >(f,theta,control);
      if(tag == Rf_install("parallelADFun"))
	return EvalADFunObjectTemplate<parallelADFun<double> >(f,theta,control);
      Rf_error("NOT A KNOWN FUNCTION POINTER");
    }
    TMB_CATCH {
      TMB_ERROR_BAD_ALLOC;
    }
  }
  
}

/* Double interface */
extern "C"
{

  /* How to garbage collect a DoubleFun object pointer */
  void finalizeDoubleFun(SEXP x)
  {
    objective_function<double>* ptr=(objective_function<double>*)R_ExternalPtrAddr(x);
    if(ptr!=NULL)delete ptr;
    memory_manager.CallCFinalizer(x);
  }
  
  SEXP MakeDoubleFunObject(SEXP data, SEXP parameters, SEXP report)
  {
    /* Some type checking */
    if(!Rf_isNewList(data))Rf_error("'data' must be a list");
    if(!Rf_isNewList(parameters))Rf_error("'parameters' must be a list");
    if(!Rf_isEnvironment(report))Rf_error("'report' must be an environment");
    
    /* Create DoubleFun pointer */
    objective_function<double>* pF = NULL;
    TMB_TRY {
      pF = new objective_function<double>(data,parameters,report);
    }
    TMB_CATCH {
      if (pF != NULL) delete pF;
      TMB_ERROR_BAD_ALLOC;
    }

    /* Convert DoubleFun pointer to R_ExternalPtr */
    SEXP res,ans;
    PROTECT(res=R_MakeExternalPtr((void*) pF,Rf_install("DoubleFun"),R_NilValue));
    PROTECT(ans=ptrList(res));
    UNPROTECT(2);
    return ans;
  }

  
  SEXP EvalDoubleFunObject(SEXP f, SEXP theta, SEXP control)
  {
    TMB_TRY {
      int do_simulate = getListInteger(control, "do_simulate");
      int get_reportdims = getListInteger(control, "get_reportdims");
      objective_function<double>* pf;
      pf = (objective_function<double>*) R_ExternalPtrAddr(f);
      pf -> sync_data();
      PROTECT( theta=Rf_coerceVector(theta,REALSXP) );
      int n = pf->theta.size();
      if (LENGTH(theta)!=n) Rf_error("Wrong parameter length.");
      vector<double> x(n);
      for(int i=0;i<n;i++) x[i] = REAL(theta)[i];
      pf->theta=x;
      /* Since we are actually evaluating objective_function::operator() (not
	 an ADFun object) we should remember to initialize parameter-index. */
      pf->index=0;
      pf->parnames.resize(0); // To avoid mem leak.
      pf->reportvector.clear();
      SEXP res;
      GetRNGstate();   /* Get seed from R */
      if(do_simulate) pf->set_simulate( true );
      PROTECT( res = asSEXP( pf->operator()() ) );
      if(do_simulate) {
        pf->set_simulate( false );
        PutRNGstate(); /* Write seed back to R */
      }
      if(get_reportdims) {
        SEXP reportdims;
        PROTECT( reportdims = pf -> reportvector.reportdims() );
        setAttrib( res, install("reportdims"), reportdims);
        UNPROTECT(1);
      }
      UNPROTECT(2);
      return res;
    }
    TMB_CATCH {
      TMB_ERROR_BAD_ALLOC;
    }
  }

  /** \internal \brief Gets parameter order by running the user template

   We spend a function evaluation on getting the parameter order (!) */
  SEXP getParameterOrder(SEXP data, SEXP parameters, SEXP report)
  {
    TMB_TRY {
      /* Some type checking */
      if(!Rf_isNewList(data))Rf_error("'data' must be a list");
      if(!Rf_isNewList(parameters))Rf_error("'parameters' must be a list");
      if(!Rf_isEnvironment(report))Rf_error("'report' must be an environment");
      objective_function<double> F(data,parameters,report);
      F(); // Run through user template
      return F.parNames();
    }
    TMB_CATCH {
      TMB_ERROR_BAD_ALLOC;
    }
  }

} /* Double interface */


ADFun< double >* MakeADGradObject_(SEXP data, SEXP parameters, SEXP report, int parallel_region=-1)
{
  /* Create ADFun pointer */
  objective_function< AD<AD<double> > > F(data,parameters,report);
  F.set_parallel_region(parallel_region);
  int n=F.theta.size();
  Independent(F.theta);
  vector< AD<AD<double> > > y(1);
  y[0]=F.evalUserTemplate();
  ADFun<AD<double> > tmp(F.theta,y);
  tmp.optimize(); /* Remove 'dead' operations (could result in nan derivatives) */
  vector<AD<double> > x(n);
  for(int i=0;i<n;i++)x[i]=CppAD::Value(F.theta[i]);
  vector<AD<double> > yy(n);
  Independent(x);
  yy=tmp.Jacobian(x);
  ADFun< double >* pf = new ADFun< double >(x,yy);
  return pf;
}

extern "C"
{

  /** \internal \brief Tape the gradient using nested AD types */
  SEXP MakeADGradObject(SEXP data, SEXP parameters, SEXP report)
  {
    ADFun<double>* pf = NULL;
    /* Some type checking */
    if(!Rf_isNewList(data))Rf_error("'data' must be a list");
    if(!Rf_isNewList(parameters))Rf_error("'parameters' must be a list");
    if(!Rf_isEnvironment(report))Rf_error("'report' must be an environment");

    /* Get the default parameter vector (tiny overhead) */
    SEXP par,res=NULL;
    objective_function< double > F(data,parameters,report);
#ifdef _OPENMP
    int n=F.count_parallel_regions(); // Evaluates user template
#else
    F.count_parallel_regions(); // Evaluates user template
#endif
    PROTECT(par=F.defaultpar());

    if(_openmp){ // Parallel mode
#ifdef _OPENMP
      if(config.trace.parallel)
	Rcout << n << " regions found.\n";
      start_parallel(); /* Start threads */
      vector< ADFun<double>* > pfvec(n);
      bool bad_thread_alloc = false;
#pragma omp parallel for if (config.tape.parallel)
      for(int i=0;i<n;i++){
	TMB_TRY {
	  pfvec[i] = NULL;
	  pfvec[i] = MakeADGradObject_(data, parameters, report, i);
	  if (config.optimize.instantly) pfvec[i]->optimize();
	}
	TMB_CATCH { bad_thread_alloc = true; }
      }
      if(bad_thread_alloc){
	for(int i=0; i<n; i++) if (pfvec[i] != NULL) delete pfvec[i];
	TMB_ERROR_BAD_ALLOC;
      }
      parallelADFun<double>* ppf=new parallelADFun<double>(pfvec);
      /* Convert parallel ADFun pointer to R_ExternalPtr */
      PROTECT(res=R_MakeExternalPtr((void*) ppf,Rf_install("parallelADFun"),R_NilValue));
#endif
    } else { // Serial mode
      /* Actual work: tape creation */
      TMB_TRY {
        pf = NULL;
        pf = MakeADGradObject_(data, parameters, report, -1);
        if(config.optimize.instantly)pf->optimize();
      }
      TMB_CATCH {
	if (pf != NULL) delete pf;
	TMB_ERROR_BAD_ALLOC;
      }
      /* Convert ADFun pointer to R_ExternalPtr */
      PROTECT(res=R_MakeExternalPtr((void*) pf,Rf_install("ADFun"),R_NilValue));
    }

    /* Return ptrList */
    SEXP ans;
    Rf_setAttrib(res,Rf_install("par"),par);
    PROTECT(ans=ptrList(res));
    UNPROTECT(3);
    return ans;
  } // MakeADGradObject
}


/** \internal \brief Tape the hessian[cbind(i,j)] using nested AD types.

    skip: integer vector of columns to skip from the hessian (will not
          change dimension - only treat h[:,skip] and h[skip,:] as
          zero). Negative subscripts are not allowed.
*/
sphess MakeADHessObject2_(SEXP data, SEXP parameters, SEXP report, SEXP skip, int parallel_region=-1)
{
  /* Some type checking */
  if(!Rf_isNewList(data))Rf_error("'data' must be a list");
  if(!Rf_isNewList(parameters))Rf_error("'parameters' must be a list");
  if(!Rf_isEnvironment(report))Rf_error("'report' must be an environment");
  
  /* Prepare stuff */
  objective_function< AD<AD<AD<double> > > > F(data,parameters,report);
  F.set_parallel_region(parallel_region);
  int n = F.theta.size();
  vector<bool> keepcol(n); // Scatter for fast lookup 
  for(int i=0; i<n; i++){
    keepcol[i]=true;
  }
  for(int i=0; i<LENGTH(skip); i++){
    keepcol[INTEGER(skip)[i]-1]=false; // skip is R-index !
  }
#define KEEP_COL(col) (keepcol[col])
#define KEEP_ROW(row,col) ( KEEP_COL(row) & (row>=col) )

  /* Tape 1: Function R^n -> R */
  Independent(F.theta);
  vector< AD<AD<AD<double> > > > y(1);
  y[0] = F.evalUserTemplate();
  ADFun<AD<AD<double> > > tape1(F.theta, y);

  /* Tape 2: Gradient R^n -> R^n   (and optimize) */
  vector<AD<AD<double> > > xx(n);
  for(int i=0; i<n; i++) xx[i] = CppAD::Value(F.theta[i]);
  vector<AD<AD<double> > > yy(n);
  Independent(xx);
  yy = tape1.Jacobian(xx);
  ADFun<AD<double > > tape2(xx,yy);
  if (config.optimize.instantly) tape2.optimize();

  /* Tape 3: Hessian  R^n -> R^m   (optimize later) */
  tape2.my_init(keepcol);
  int colisize;
  int m=0; // Count number of non-zeros (m)
  for(int i=0; i<int(tape2.colpattern.size()); i++){
    colisize = tape2.colpattern[i].size();
    if(KEEP_COL(i)){
      for(int j=0; j<colisize; j++){
	m += KEEP_ROW( tape2.colpattern[i][j] , i);
      }
    }
  }
  // Allocate index vectors of non-zero pairs
  vector<int> rowindex(m);
  vector<int> colindex(m);
  // Prepare reverse sweep for Hessian columns
  vector<AD<double> > u(n);
  vector<AD<double> > v(n);
  for(int i = 0; i < n; i++) v[i] = 0.0;
  vector<AD<double> > xxx(n);
  for(int i=0; i<n; i++) xxx[i]=CppAD::Value(CppAD::Value(F.theta[i]));
  vector<AD<double> > yyy(m);
  CppAD::vector<int>* icol;
  // Do sweeps and fill in non-zero index pairs
  Independent(xxx);
  tape2.Forward(0, xxx);
  int k=0;
  for(int i = 0; i < n; i++){
    if (KEEP_COL(i)) {
      tape2.myReverse(1, v, i /*range comp*/, u /*domain*/);
      icol = &tape2.colpattern[i];
      for(int j=0; j<int(icol->size()); j++){
	if(KEEP_ROW( icol->operator[](j), i )){
	  rowindex[k] = icol->operator[](j);
	  colindex[k] = i;
	  yyy[k] = u[icol->operator[](j)];
	  k++;
	}
      }
    }
  }
  ADFun< double >* ptape3 = new ADFun< double >;
  ptape3->Dependent(xxx,yyy);
  sphess ans(ptape3, rowindex, colindex);
  return ans;
} // MakeADHessObject2

// kasper: Move to new file e.g. "convert.hpp"
template <class ADFunType>
/** \internal \brief Convert sparse matrix H to SEXP format that can be returned to R */
SEXP asSEXP(const sphess_t<ADFunType> &H, const char* tag)
{
    SEXP par;
    par=R_NilValue;
    /* Convert ADFun pointer to R_ExternalPtr */
    SEXP res;
    PROTECT( res = R_MakeExternalPtr((void*) H.pf, Rf_install(tag), R_NilValue) );
    /* Return list */
    SEXP ans;
    /* Implicitly protected temporaries */
    SEXP par_symbol = Rf_install("par");
    SEXP i_symbol = Rf_install("i");
    SEXP j_symbol = Rf_install("j");
    Rf_setAttrib(res, par_symbol, par);
    Rf_setAttrib(res, i_symbol, asSEXP(H.i));
    Rf_setAttrib(res, j_symbol, asSEXP(H.j));
    PROTECT(ans=ptrList(res));
    UNPROTECT(2);
    return ans;
}


extern "C"
{
#ifdef _OPENMP
  SEXP MakeADHessObject2(SEXP data, SEXP parameters, SEXP report, SEXP skip){
    if(config.trace.parallel)
      Rcout << "Count num parallel regions\n";
    objective_function< double > F(data,parameters,report);
    int n=F.count_parallel_regions();
    if(config.trace.parallel)
      Rcout << n << " regions found.\n";

    start_parallel(); /* Start threads */

    /* parallel test */
    bool bad_thread_alloc = false;
    vector<sphess*> Hvec(n);
#pragma omp parallel for if (config.tape.parallel)
    for (int i=0; i<n; i++) {
      TMB_TRY {
	Hvec[i] = NULL;
	Hvec[i] = new sphess( MakeADHessObject2_(data, parameters, report, skip, i) );
	optimizeTape( Hvec[i]->pf );
      }
      TMB_CATCH { bad_thread_alloc = true; }
    }
    if (bad_thread_alloc) {
      for(int i=0; i<n; i++) {
	if (Hvec[i] != NULL) {
	  delete Hvec[i]->pf;
	  delete Hvec[i];
	}
      }
      TMB_ERROR_BAD_ALLOC;
    }
    parallelADFun<double>* tmp=new parallelADFun<double>(Hvec);
    for(int i=0; i<n; i++) {
      delete Hvec[i];
    }
    // Adds finalizer for 'tmp' !!! (so, don't delete tmp...)
    SEXP ans = asSEXP(tmp->convert(),"parallelADFun");
    return ans;
  } // MakeADHessObject2
#else
  SEXP MakeADHessObject2(SEXP data, SEXP parameters, SEXP report, SEXP skip){
    sphess* pH = NULL;
    SEXP ans;
    TMB_TRY {
      pH = new sphess( MakeADHessObject2_(data, parameters, report, skip, -1) );
      optimizeTape( pH->pf );
      ans = asSEXP(*pH, "ADFun");
    }
    TMB_CATCH {
      if (pH != NULL) {
	delete pH->pf;
	delete pH;
      }
      TMB_ERROR_BAD_ALLOC;
    }
    delete pH;
    return ans;
  } // MakeADHessObject2
#endif
}

extern "C"
{
  SEXP usingAtomics(){
    SEXP ans;
    PROTECT(ans = Rf_allocVector(INTSXP,1));
    INTEGER(ans)[0] = atomic::atomicFunctionGenerated;
    UNPROTECT(1);
    return ans;
  }
}

extern "C"
{
  void tmb_forward(SEXP f, const Eigen::VectorXd &x, Eigen::VectorXd &y) {
    SEXP tag=R_ExternalPtrTag(f);
    if(tag == Rf_install("ADFun")) {
      ADFun<double>* pf;
      pf = (ADFun<double>*) R_ExternalPtrAddr(f);
      y = pf->Forward(0, x);
    } else
      if(tag == Rf_install("parallelADFun")) {
        parallelADFun<double>* pf;
        pf = (parallelADFun<double>*) R_ExternalPtrAddr(f);
        y = pf->Forward(0, x);
      } else
        Rf_error("Unknown function pointer");
  }
  void tmb_reverse(SEXP f, const Eigen::VectorXd &v, Eigen::VectorXd &y) {
    SEXP tag=R_ExternalPtrTag(f);
    if(tag == Rf_install("ADFun")) {
      ADFun<double>* pf;
      pf = (ADFun<double>*) R_ExternalPtrAddr(f);
      y = pf->Reverse(1, v);
    } else
      if(tag == Rf_install("parallelADFun")) {
        parallelADFun<double>* pf;
        pf = (parallelADFun<double>*) R_ExternalPtrAddr(f);
        y = pf->Reverse(1, v);
      } else
        Rf_error("Unknown function pointer");
  }
}

#endif /* #ifndef WITH_LIBTMB */


#ifdef WITH_LIBTMB

template class objective_function<double>;
template class objective_function<AD<double> >;
template class objective_function<AD<AD<double> > >;
template class objective_function<AD<AD<AD<double> > > >;
extern "C"
{
  SEXP MakeADFunObject(SEXP data, SEXP parameters, SEXP report, SEXP control);
  SEXP InfoADFunObject(SEXP f);
  SEXP optimizeADFunObject(SEXP f);
  SEXP EvalADFunObject(SEXP f, SEXP theta, SEXP control);
  SEXP MakeDoubleFunObject(SEXP data, SEXP parameters, SEXP report);
  SEXP EvalDoubleFunObject(SEXP f, SEXP theta, SEXP control);
  SEXP getParameterOrder(SEXP data, SEXP parameters, SEXP report);
  SEXP MakeADGradObject(SEXP data, SEXP parameters, SEXP report);
  SEXP MakeADHessObject2(SEXP data, SEXP parameters, SEXP report, SEXP skip);
  SEXP usingAtomics();
  void tmb_forward(SEXP f, const Eigen::VectorXd &x, Eigen::VectorXd &y);
  void tmb_reverse(SEXP f, const Eigen::VectorXd &v, Eigen::VectorXd &y);
}

#endif /* #ifdef WITH_LIBTMB */

/* Register native routines (see 'Writing R extensions'). Especially
   relevant to avoid symbol lookup overhead for those routines that
   are called many times e.g. EvalADFunObject. */
extern "C"{
  /* Some string utilities */
#define xstringify(s) stringify(s)
#define stringify(s) #s
  /* May be used as part of custom calldef tables */
#define TMB_CALLDEFS                                            \
  {"MakeADFunObject",     (DL_FUNC) &MakeADFunObject,     4},   \
  {"FreeADFunObject",     (DL_FUNC) &FreeADFunObject,     1},   \
  {"InfoADFunObject",     (DL_FUNC) &InfoADFunObject,     1},   \
  {"EvalADFunObject",     (DL_FUNC) &EvalADFunObject,     3},   \
  {"MakeDoubleFunObject", (DL_FUNC) &MakeDoubleFunObject, 3},   \
  {"EvalDoubleFunObject", (DL_FUNC) &EvalDoubleFunObject, 3},   \
  {"getParameterOrder",   (DL_FUNC) &getParameterOrder,   3},   \
  {"MakeADGradObject",    (DL_FUNC) &MakeADGradObject,    3},   \
  {"MakeADHessObject2",   (DL_FUNC) &MakeADHessObject2,   4},   \
  {"usingAtomics",        (DL_FUNC) &usingAtomics,        0},   \
  {"TMBconfig",           (DL_FUNC) &TMBconfig,           2}
  /* May be used as part of custom R_init function
     C-callable routines (PACKAGE is 'const char*') */
#define TMB_CCALLABLES(PACKAGE)                                         \
  R_RegisterCCallable(PACKAGE, "tmb_forward", (DL_FUNC) &tmb_forward);  \
  R_RegisterCCallable(PACKAGE, "tmb_reverse", (DL_FUNC) &tmb_reverse);
  /* Default (optional) calldef table. */
#ifdef TMB_LIB_INIT
#include <R_ext/Rdynload.h>
static R_CallMethodDef CallEntries[] = {
  TMB_CALLDEFS
  ,
  /* User's R_unload_lib function must also be registered (because we
     disable dynamic lookup - see below). The unload function is
     mainly useful while developing models in order to clean up
     external pointers without restarting R. Should not be used by TMB
     dependent packages. */
#ifdef LIB_UNLOAD
  {xstringify(LIB_UNLOAD), (DL_FUNC) &LIB_UNLOAD, 1},
#endif
  /* End of table */
  {NULL, NULL, 0}
};
void TMB_LIB_INIT(DllInfo *dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, (Rboolean)FALSE);
  // Example: TMB_LIB_INIT = R_init_mypkg
  TMB_CCALLABLES(&(xstringify(TMB_LIB_INIT)[7]));
}
#endif /* #ifdef TMB_LIB_INIT */
#undef xstringify
#undef stringify
}
