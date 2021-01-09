template<int base, int n>
struct pow_t {
  static const long int value = base * pow_t<base, n-1>::value;
};
template<int base>
struct pow_t<base, 0> {
  static const long int value = 1;
};
template<long int mask>
struct mask_t {
  template<int length, int i=0>
  struct set_length {
    static const int base = 8;
    static const long int power = pow_t<base, i>::value;
    typedef set_length<length, i+1> next_index_t;
    static const int flag = ( (mask / power) % base ) != 0;
    static const int count = flag + next_index_t::count;
    static const int Id = count - 1;
    static const int Index = length - i - 1;
    next_index_t next_index;
    template<class S, class T>
    void copy(S &dest, const T &orig) {
      dest[Index] = (flag ? orig[Id] : 0);
      next_index.copy(dest, orig);
    }
    template<class S, class T>
    void activate_derivs(S &var, T &value) {
      var[Index] = value[Index];
      if (flag) var[Index].setid(Id);
      next_index.activate_derivs(var, value);
    }
  };
  template<int length>
  struct set_length<length, length> {
    static const int count = 0;
    void trace() { }
    template<class S, class T>
    void copy(S &dest, const T &orig) { }
    template<class S, class T>
    void activate_derivs(S &var, T &value) { }
  };
};

template<int nvar>
struct ADTypes {
  typedef tiny_ad::variable<1, nvar> order1;
  typedef tiny_ad::variable<2, nvar> order2;
  typedef tiny_ad::variable<3, nvar> order3;
};

// 'TMB_BIND_ATOMIC' depends on these:
#define NCHAR(x) sizeof(#x)-1
#define OCTAL(x) 0 ## x

/** \brief Bind an atomic function to a forward differentiated
    template function using tiny_ad. Generates up to 3rd order
    derivatives.

    \param NAME Name of atomic function.

    \param MASK Binary mask denoting active variables. e.g. 0101 says
    there are a total of four input parameters of which two are
    active.

    \param CALL A call to a differentiable template function. The
    input parameters must be referred to as 'x'.

    \note: The argument list is expanded with a number denoting the
    order. So, in a case with four input parameters the generated
    atomic function actually has five parameters. The last parameter
    must be set to zero when calling the atomic function.
*/
#define TMB_BIND_ATOMIC(NAME,MASK,CALL)					\
TMB_ATOMIC_VECTOR_FUNCTION(						\
  NAME									\
  ,									\
  (size_t)     								\
  pow((double)								\
  atomic::mask_t<OCTAL(MASK)>::set_length< NCHAR(MASK) >::count,	\
      CppAD::Integer(tx[NCHAR(MASK)]))					\
  ,									\
  int order = CppAD::Integer(tx[NCHAR(MASK)]);				\
  typedef								\
  atomic::mask_t<OCTAL(MASK)>::set_length<NCHAR(MASK)> mask_type;	\
  mask_type mask;							\
  static const int nvar = mask_type::count;				\
  atomic::tiny_vec_ref<double> tyref(&ty[0], ty.size());                \
  if(order==0) {							\
    typedef double Float;						\
    CppAD::vector<Float> x(tx);						\
    ty[0] = CALL;							\
  }									\
  else if (order==1) {							\
    typedef typename atomic::ADTypes<nvar>::order1 Float;		\
    Float x[NCHAR(MASK)];						\
    mask.activate_derivs(x, tx);					\
    tyref = CALL.getDeriv();                                            \
  }									\
  else if (order==2) {							\
    typedef typename atomic::ADTypes<nvar>::order2 Float;		\
    Float x[NCHAR(MASK)];						\
    mask.activate_derivs(x, tx);					\
    tyref = CALL.getDeriv();                                            \
  }									\
  else if (order==3) {							\
    typedef typename atomic::ADTypes<nvar>::order3 Float;		\
    Float x[NCHAR(MASK)];						\
    mask.activate_derivs(x, tx);					\
    tyref = CALL.getDeriv();                                            \
  }									\
  else									\
    Rf_error("Order not implemented");					\
  ,									\
  typedef								\
  atomic::mask_t<OCTAL(MASK)>::set_length<NCHAR(MASK)> mask_type;	\
  mask_type mask;							\
  static const int nvar = mask_type::count;				\
  CppAD::vector<Type> tx_(tx);						\
  tx_[NCHAR(MASK)] = tx_[NCHAR(MASK)] + Type(1.0);			\
  vector<Type> tmp = NAME(tx_);						\
  matrix<Type> m = tmp.matrix();					\
  m.resize(nvar, m.size() / nvar);					\
  vector<Type> w = py;							\
  vector<Type> px_ = m * w.matrix();					\
  mask.copy(px, px_);							\
  px[NCHAR(MASK)] = 0;							\
  )
