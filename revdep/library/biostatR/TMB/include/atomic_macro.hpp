// Copyright (C) 2013-2015 Kasper Kristensen
// License: GPL-2

/* Flag to detect if any atomic functions have been created */
TMB_EXTERN bool atomicFunctionGenerated CSKIP(= false;)

/** \brief Construct atomic vector function based on known derivatives */
#define TMB_ATOMIC_VECTOR_FUNCTION(ATOMIC_NAME, OUTPUT_DIM, ATOMIC_DOUBLE,    \
                                   ATOMIC_REVERSE)                            \
                                                                              \
  template<class Double>                                                      \
  void ATOMIC_NAME(const CppAD::vector<Double>& tx,                           \
                   CppAD::vector<Double>& ty) CSKIP({                         \
    ATOMIC_DOUBLE;                                                            \
  })                                                                          \
  template<class Double>                                                      \
  CppAD::vector<double> ATOMIC_NAME(const CppAD::vector<Double>& tx) CSKIP({  \
    CppAD::vector<double> ty(OUTPUT_DIM);                                     \
    ATOMIC_NAME(tx, ty);                                                      \
    return ty;                                                                \
  })                                                                          \
  IF_TMB_PRECOMPILE(                                                          \
  template                                                                    \
  void ATOMIC_NAME<double>(const CppAD::vector<double>& tx,                   \
                           CppAD::vector<double>& ty);                        \
  template                                                                    \
  CppAD::vector<double> ATOMIC_NAME<double>(const CppAD::vector<double>& tx); \
  )                                                                           \
  template <class Type>                                                       \
  void ATOMIC_NAME(const CppAD::vector<AD<Type> >& tx,                        \
                   CppAD::vector<AD<Type> >& ty);                             \
  template <class Type>                                                       \
  CppAD::vector<AD<Type> > ATOMIC_NAME(const CppAD::vector<AD<Type> >& tx);   \
  template <class Type>                                                       \
  class atomic##ATOMIC_NAME : public CppAD::atomic_base<Type> {               \
   public:                                                                    \
    atomic##ATOMIC_NAME(const char* name) : CppAD::atomic_base<Type>(name) {  \
      atomic::atomicFunctionGenerated = true;                                 \
      if (config.trace.atomic)                                                \
        Rcout << "Constructing atomic " << #ATOMIC_NAME << "\n";          \
      this->option(CppAD::atomic_base<Type>::bool_sparsity_enum);             \
    }                                                                         \
                                                                              \
   private:                                                                   \
    virtual bool forward(size_t p, size_t q, const CppAD::vector<bool>& vx,   \
                         CppAD::vector<bool>& vy,                             \
                         const CppAD::vector<Type>& tx,                       \
                         CppAD::vector<Type>& ty) {                           \
      if (q > 0)                                                              \
        Rf_error("Atomic '" #ATOMIC_NAME "' order not implemented.\n");       \
      if (vx.size() > 0) {                                                    \
        bool anyvx = false;                                                   \
        for (size_t i = 0; i < vx.size(); i++) anyvx |= vx[i];                \
        for (size_t i = 0; i < vy.size(); i++) vy[i] = anyvx;                 \
      }                                                                       \
      ATOMIC_NAME(tx, ty);                                                    \
      return true;                                                            \
    }                                                                         \
    virtual bool reverse(size_t q, const CppAD::vector<Type>& tx,             \
                         const CppAD::vector<Type>& ty,                       \
                         CppAD::vector<Type>& px,                             \
                         const CppAD::vector<Type>& py) {                     \
      if (q > 0)                                                              \
        Rf_error("Atomic '" #ATOMIC_NAME "' order not implemented.\n");       \
      ATOMIC_REVERSE;                                                         \
      return true;                                                            \
    }                                                                         \
    virtual bool rev_sparse_jac(size_t q, const CppAD::vector<bool>& rt,      \
                                CppAD::vector<bool>& st) {                    \
      bool anyrt = false;                                                     \
      for (size_t i = 0; i < rt.size(); i++) anyrt |= rt[i];                  \
      for (size_t i = 0; i < st.size(); i++) st[i] = anyrt;                   \
      return true;                                                            \
    }                                                                         \
    virtual bool rev_sparse_jac(size_t q,                                     \
                                const CppAD::vector<std::set<size_t> >& rt,   \
                                CppAD::vector<std::set<size_t> >& st) {       \
      Rf_error("Should not be called");                                       \
    }                                                                         \
  };                                                                          \
  template <class Type>                                                       \
  void ATOMIC_NAME(const CppAD::vector<AD<Type> >& tx,                        \
                   CppAD::vector<AD<Type> >& ty) {                            \
    static atomic##ATOMIC_NAME<Type> afun##ATOMIC_NAME(                       \
        "atomic_" #ATOMIC_NAME);                                              \
    afun##ATOMIC_NAME(tx, ty);                                                \
  }                                                                           \
  template <class Type>                                                       \
  CppAD::vector<AD<Type> > ATOMIC_NAME(const CppAD::vector<AD<Type> >& tx) {  \
    CppAD::vector<AD<Type> > ty(OUTPUT_DIM);                                  \
    ATOMIC_NAME(tx, ty);                                                      \
    return ty;                                                                \
  }
