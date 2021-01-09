namespace atomic {
namespace dynamic_data {
  /* Represent SEXP as double so it can be put on the tape. Same for
     char pointers.

     FIXME: Portability.
     Currently assuming sizeof(double) >= sizeof(pointer) which should
     be OK for most common 32/64 bit operating systems.
  */

#ifdef WITH_LIBTMB
  double sexp_to_double(SEXP x);
  SEXP double_to_sexp(double x);
  double charptr_to_double(const char *x);
  const char* double_to_charptr(double x);
#else
  double sexp_to_double(SEXP x) {
    SEXP*   px = &x;
    double* py = (double*) px;
    return py[0];
  }
  SEXP double_to_sexp(double x) {
    double* px = &x;
    SEXP*   py = (SEXP*) px;
    return py[0];
  }
  double charptr_to_double(const char *x) {
    const char**  px = &x;
    double* py = (double*) px;
    return py[0];
  }
  const char* double_to_charptr(double x) {
    double* px = &x;
    const char** py = (const char**) px;
    return py[0];
  }
#endif // #ifdef WITH_LIBTMB

  TMB_ATOMIC_VECTOR_FUNCTION(
                             // atomic name
                             list_lookup_by_index
                             ,
                             // output dim
                             1
                             ,
                             // forward double
                             SEXP data  = double_to_sexp( tx[0] );
                             int  index = (int)  tx[1];
                             ty[0] = sexp_to_double( VECTOR_ELT(data, index) );
                             ,
                             // reverse
                             px[0] = 0; px[1] = 0;
                             )

  TMB_ATOMIC_VECTOR_FUNCTION(
                             // atomic name
                             list_lookup_by_name
                             ,
                             // output dim
                             1
                             ,
                             // forward double
                             SEXP list = double_to_sexp( tx[0] );
                             const char* str = double_to_charptr( tx[1] );
                             SEXP elmt = R_NilValue;
                             SEXP names = Rf_getAttrib(list, R_NamesSymbol);
                             int i;
                             for (i = 0; i < Rf_length(list); i++) {
                               if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
                                 elmt = VECTOR_ELT(list, i);
                                 break;
                               }
                             }
                             ty[0] = sexp_to_double( elmt );
                             ,
                             // reverse
                             px[0] = 0; px[1] = 0;
                             )

  TMB_ATOMIC_VECTOR_FUNCTION(
                             // atomic name
                             envir_lookup_by_name
                             ,
                             // output dim
                             1
                             ,
                             // forward double
                             SEXP envir = double_to_sexp( tx[0] );
                             const char* nam = double_to_charptr( tx[1] );
                             SEXP res = findVar(install(nam), envir);
                             ty[0] = sexp_to_double( res );
                             ,
                             // reverse
                             px[0] = 0; px[1] = 0;
                             )

  // Convert SEXP to vector
  TMB_ATOMIC_VECTOR_FUNCTION(
                             // atomic name
                             sexp_to_vector
                             ,
                             // output dim
                             LENGTH( double_to_sexp( asDouble(tx[0]) ))
                             ,
                             // forward double
                             SEXP data = double_to_sexp( tx[0] );
                             int n = LENGTH( data );
                             if (n != (int) ty.size())
                               Rf_error("Data update: "
                                        "number of items to replace (%i) "
                                        "does not match replacement length (%i)",
                                        ty.size(), n);
                             for (int i = 0; i<n; i++) ty[i] = REAL(data)[i];
                             ,
                             // reverse
                             px[0] = 0;
                             )

  
  // Input: double x, Type y
  // Output double x (now with fake dependence on y)
  TMB_ATOMIC_VECTOR_FUNCTION(
                             // atomic name
                             set_dependent
                             ,
                             // output dim
                             1
                             ,
                             // forward double
                             ty[0] = tx[0];
                             ,
                             // reverse
                             px[0] = 0; px[1] = 0;
                             )


  /*  Interfaces   */
  template<class Type>
  Type set_dependent(double x, Type fake_parameter) {
    CppAD::vector<Type> tx(2);
    tx[0] = x;
    tx[1] = fake_parameter;
    return set_dependent(tx)[0];
  }

  template<class Type>
  vector<Type> sexp_to_vector(Type sexp) {
    CppAD::vector<Type> tx(1);
    tx[0] = sexp;
    CppAD::vector<Type> ty(sexp_to_vector(tx));
    return ty;
  }

  // Output: SEXP represented by Type
  template<class Type>
  Type envir_lookup_by_name(Type envir, const char* name) {
    CppAD::vector<Type> tx(2);
    tx[0] = envir;
    tx[1] = charptr_to_double( name );
    return envir_lookup_by_name(tx)[0];
  }

  // Output: SEXP represented by Type
  template<class Type>
  Type list_lookup_by_name(Type list, const char* name) {
    CppAD::vector<Type> tx(2);
    tx[0] = list;
    tx[1] = charptr_to_double( name );
    return list_lookup_by_name(tx)[0];
  }

  // Output: SEXP represented by Type
  template<class Type>
  Type list_lookup_by_index(Type list, Type index) {
    CppAD::vector<Type> tx(2);
    tx[0] = list;
    tx[1] = index;
    return list_lookup_by_index(tx)[0];
  }

  // Fill elements from y into x
  template<class T1, class T2>
  void cpy(T1 &x, T2 y) {
    for (int i=0; i<y.size(); i++) {
      x.coeffRef(i) = y.coeffRef(i);
    }
  }

  // Scalar case
  template<class Type>
  void cpy(Type &x, vector<Type> y) {
    x = y[0];
  }

}  
}

/** \brief Update a data object without re-taping.

    Placing DATA_UPDATE(x) **after** e.g. DATA_VECTOR(x) will allow to
    change x on the R-side (through obj$env$data$x) without re-taping.
    Updatable data items behave like *parameters*. In particular,
    if-else branching on such data should be avoided.

    \note Only works for DATA_VECTOR(), DATA_MATRIX(), DATA_ARRAY()
    and DATA_SCALAR().
    \warning It is the user's responsibility not to reshape the data
    from R (i.e. change length or dimension). Storage mode must also
    remain constant.
    \ingroup macros
    \internal
    Explanation: The SEXP pointer of the model environment (`obj$env`)
    is put on the tape with a fake dependency on an arbitrary model
    parameter (`theta[0]`). This ensures that the corresponding node
    in the computational graph is traversed on each forward sweep. In
    contrast to other SEXPs, it is safe to put environments on the
    tape because they do not change on copy or modification of the
    variables they contain.
*/
#define DATA_UPDATE(name)                               \
atomic::dynamic_data::cpy(name,                         \
  atomic::dynamic_data::sexp_to_vector(                 \
    atomic::dynamic_data::list_lookup_by_name(          \
      atomic::dynamic_data::envir_lookup_by_name(       \
        atomic::dynamic_data::set_dependent(            \
          atomic::dynamic_data::sexp_to_double(         \
            ENCLOS(TMB_OBJECTIVE_PTR -> report)         \
          ),                                            \
          TMB_OBJECTIVE_PTR -> theta[0]                 \
        ),                                              \
        "data"                                          \
      ),                                                \
      #name                                             \
    )                                                   \
  )                                                     \
);
