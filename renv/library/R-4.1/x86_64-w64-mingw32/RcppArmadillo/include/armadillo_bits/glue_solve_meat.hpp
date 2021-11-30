// Copyright 2008-2016 Conrad Sanderson (http://conradsanderson.id.au)
// Copyright 2008-2016 National ICT Australia (NICTA)
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ------------------------------------------------------------------------


//! \addtogroup glue_solve
//! @{



//
// glue_solve_gen


template<typename T1, typename T2>
inline
void
glue_solve_gen::apply(Mat<typename T1::elem_type>& out, const Glue<T1,T2,glue_solve_gen>& X)
  {
  arma_extra_debug_sigprint();
  
  const bool status = glue_solve_gen::apply( out, X.A, X.B, X.aux_uword );
  
  if(status == false)
    {
    out.soft_reset();
    arma_stop_runtime_error("solve(): solution not found");
    }
  }



template<typename eT, typename T1, typename T2>
inline
bool
glue_solve_gen::apply(Mat<eT>& out, const Base<eT,T1>& A_expr, const Base<eT,T2>& B_expr, const uword flags)
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  const bool fast         = bool(flags & solve_opts::flag_fast        );
  const bool equilibrate  = bool(flags & solve_opts::flag_equilibrate );
  const bool no_approx    = bool(flags & solve_opts::flag_no_approx   );
  const bool no_band      = bool(flags & solve_opts::flag_no_band     );
  const bool no_sympd     = bool(flags & solve_opts::flag_no_sympd    );
  const bool allow_ugly   = bool(flags & solve_opts::flag_allow_ugly  );
  const bool likely_sympd = bool(flags & solve_opts::flag_likely_sympd);
  const bool refine       = bool(flags & solve_opts::flag_refine      );
  const bool no_trimat    = bool(flags & solve_opts::flag_no_trimat   );
  
  arma_extra_debug_print("glue_solve_gen::apply(): enabled flags:");
  
  if(fast        )  { arma_extra_debug_print("fast");         }
  if(equilibrate )  { arma_extra_debug_print("equilibrate");  }
  if(no_approx   )  { arma_extra_debug_print("no_approx");    }
  if(no_band     )  { arma_extra_debug_print("no_band");      }
  if(no_sympd    )  { arma_extra_debug_print("no_sympd");     }
  if(allow_ugly  )  { arma_extra_debug_print("allow_ugly");   }
  if(likely_sympd)  { arma_extra_debug_print("likely_sympd"); }
  if(refine      )  { arma_extra_debug_print("refine");       }
  if(no_trimat   )  { arma_extra_debug_print("no_trimat");    }
  
  arma_debug_check( (fast     && equilibrate ), "solve(): options 'fast' and 'equilibrate' are mutually exclusive"      );
  arma_debug_check( (fast     && refine      ), "solve(): options 'fast' and 'refine' are mutually exclusive"           );
  arma_debug_check( (no_sympd && likely_sympd), "solve(): options 'no_sympd' and 'likely_sympd' are mutually exclusive" );
  
  T    rcond  = T(0);
  bool status = false;
  
  Mat<eT> A = A_expr.get_ref();
  
  if(A.n_rows == A.n_cols)
    {
    arma_extra_debug_print("glue_solve_gen::apply(): detected square system");
    
    uword KL = 0;
    uword KU = 0;
    
    #if defined(ARMA_OPTIMISE_BAND)
      const bool is_band  = (no_band || auxlib::crippled_lapack(A)) ? false : band_helper::is_band(KL, KU, A, uword(32));
    #else
      const bool is_band  = false;
    #endif
    
    const bool is_triu = (no_trimat || refine || equilibrate || likely_sympd || is_band           ) ? false : trimat_helper::is_triu(A);
    const bool is_tril = (no_trimat || refine || equilibrate || likely_sympd || is_band || is_triu) ? false : trimat_helper::is_tril(A);
    
    #if defined(ARMA_OPTIMISE_SYMPD)
      const bool try_sympd = (no_sympd || auxlib::crippled_lapack(A) || is_band || is_triu || is_tril) ? false : (likely_sympd ? true : sympd_helper::guess_sympd(A));
    #else
      const bool try_sympd = false;
    #endif
    
    if(fast)
      {
      // fast mode: solvers without refinement and without rcond estimate
      
      arma_extra_debug_print("glue_solve_gen::apply(): fast mode");
      
      if(is_band)
        {
        if( (KL == 1) && (KU == 1) )
          {
          arma_extra_debug_print("glue_solve_gen::apply(): fast + tridiagonal");
          
          status = auxlib::solve_tridiag_fast(out, A, B_expr.get_ref());
          }
        else
          {
          arma_extra_debug_print("glue_solve_gen::apply(): fast + band");
          
          status = auxlib::solve_band_fast(out, A, KL, KU, B_expr.get_ref());
          }
        }
      else
      if(is_triu || is_tril)
        {
        if(is_triu)  { arma_extra_debug_print("glue_solve_gen::apply(): fast + upper triangular matrix"); }
        if(is_tril)  { arma_extra_debug_print("glue_solve_gen::apply(): fast + lower triangular matrix"); }
        
        const uword layout = (is_triu) ? uword(0) : uword(1);
        
        status = auxlib::solve_trimat_fast(out, A, B_expr.get_ref(), layout);
        }
      else
      if(try_sympd)
        {
        arma_extra_debug_print("glue_solve_gen::apply(): fast + try_sympd");
        
        status = auxlib::solve_sympd_fast(out, A, B_expr.get_ref());  // A is overwritten
        
        if(status == false)
          {
          arma_extra_debug_print("glue_solve_gen::apply(): auxlib::solve_sympd_fast() failed; retrying");
          
          // auxlib::solve_sympd_fast() may have failed because A isn't really sympd
          A = A_expr.get_ref();
          status = auxlib::solve_square_fast(out, A, B_expr.get_ref());  // A is overwritten
          }
        }
      else
        {
        arma_extra_debug_print("glue_solve_gen::apply(): fast + dense");
        
        status = auxlib::solve_square_fast(out, A, B_expr.get_ref());  // A is overwritten
        }
      }
    else
    if(refine || equilibrate)
      {
      // refine mode: solvers with refinement and with rcond estimate
      
      arma_extra_debug_print("glue_solve_gen::apply(): refine mode");
      
      if(is_band)
        {
        arma_extra_debug_print("glue_solve_gen::apply(): refine + band");
        
        status = auxlib::solve_band_refine(out, rcond, A, KL, KU, B_expr, equilibrate, allow_ugly);
        }
      else
      if(try_sympd)
        {
        arma_extra_debug_print("glue_solve_gen::apply(): refine + try_sympd");
        
        status = auxlib::solve_sympd_refine(out, rcond, A, B_expr.get_ref(), equilibrate, allow_ugly);  // A is overwritten
        
        if(status == false)
          {
          arma_extra_debug_print("glue_solve_gen::apply(): auxlib::solve_sympd_refine() failed; retrying");
          
          // auxlib::solve_sympd_refine() may have failed because A isn't really sympd
          A = A_expr.get_ref();
          status = auxlib::solve_square_refine(out, rcond, A, B_expr.get_ref(), equilibrate, allow_ugly);  // A is overwritten
          }
        }
      else
        {
        arma_extra_debug_print("glue_solve_gen::apply(): refine + dense");
        
        status = auxlib::solve_square_refine(out, rcond, A, B_expr, equilibrate, allow_ugly);  // A is overwritten
        }
      }
    else
      {
      // default mode: solvers without refinement but with rcond estimate
      
      arma_extra_debug_print("glue_solve_gen::apply(): default mode");
      
      if(is_band)
        {
        arma_extra_debug_print("glue_solve_gen::apply(): rcond + band");
        
        status = auxlib::solve_band_rcond(out, rcond, A, KL, KU, B_expr.get_ref(), allow_ugly);
        }
      else
      if(is_triu || is_tril)
        {
        if(is_triu)  { arma_extra_debug_print("glue_solve_gen::apply(): rcond + upper triangular matrix"); }
        if(is_tril)  { arma_extra_debug_print("glue_solve_gen::apply(): rcond + lower triangular matrix"); }
        
        const uword layout = (is_triu) ? uword(0) : uword(1);
        
        status = auxlib::solve_trimat_rcond(out, rcond, A, B_expr.get_ref(), layout, allow_ugly);
        }
      else
      if(try_sympd)
        {
        status = auxlib::solve_sympd_rcond(out, rcond, A, B_expr.get_ref(), allow_ugly);  // A is overwritten
        
        if(status == false)
          {
          arma_extra_debug_print("glue_solve_gen::apply(): auxlib::solve_sympd_rcond() failed; retrying");
          
          // auxlib::solve_sympd_rcond() may have failed because A isn't really sympd
          A = A_expr.get_ref();
          status = auxlib::solve_square_rcond(out, rcond, A, B_expr.get_ref(), allow_ugly);  // A is overwritten
          }
        }
      else
        {
        status = auxlib::solve_square_rcond(out, rcond, A, B_expr.get_ref(), allow_ugly);  // A is overwritten
        }
      }
    
    
    
    if( (status == true) && (rcond > T(0)) && (rcond < auxlib::epsilon_lapack(A)) )
      {
      arma_debug_warn_level(2, "solve(): solution computed, but system is singular to working precision (rcond: ", rcond, ")");
      }
    
    
    if( (status == false) && (no_approx == false) )
      {
      arma_extra_debug_print("glue_solve_gen::apply(): solving rank deficient system");
      
      if(rcond > T(0))
        {
        arma_debug_warn_level(2, "solve(): system is singular (rcond: ", rcond, "); attempting approx solution");
        }
      else
        {
        arma_debug_warn_level(2, "solve(): system is singular; attempting approx solution");
        }
      
      // TODO: conditionally recreate A: have a separate state flag which indicates whether A was previously overwritten
      
      A = A_expr.get_ref();  // as A may have been overwritten
      
      status = auxlib::solve_approx_svd(out, A, B_expr.get_ref());  // A is overwritten
      }
    }
  else
    {
    arma_extra_debug_print("glue_solve_gen::apply(): detected non-square system");
    
    if(equilibrate)   { arma_debug_warn_level(1,  "solve(): option 'equilibrate' ignored for non-square matrix"  ); }
    if(refine)        { arma_debug_warn_level(1,  "solve(): option 'refine' ignored for non-square matrix"       ); }
    if(likely_sympd)  { arma_debug_warn_level(1,  "solve(): option 'likely_sympd' ignored for non-square matrix" ); }
    
    if(fast)
      {
      status = auxlib::solve_rect_fast(out, A, B_expr.get_ref());  // A is overwritten
      }
    else
      {
      status = auxlib::solve_rect_rcond(out, rcond, A, B_expr.get_ref(), allow_ugly);  // A is overwritten
      }

    if( (status == true) && (rcond > T(0)) && (rcond < auxlib::epsilon_lapack(A)) )
      {
      arma_debug_warn_level(2, "solve(): solution computed, but system is singular to working precision (rcond: ", rcond, ")");
      }
    
    if( (status == false) && (no_approx == false) )
      {
      arma_extra_debug_print("glue_solve_gen::apply(): solving rank deficient system");
      
      if(rcond > T(0))
        {
        arma_debug_warn_level(2, "solve(): system is singular (rcond: ", rcond, "); attempting approx solution");
        }
      else
        {
        arma_debug_warn_level(2, "solve(): system is singular; attempting approx solution");
        }
      
      A = A_expr.get_ref();  // as A was overwritten
      
      status = auxlib::solve_approx_svd(out, A, B_expr.get_ref());  // A is overwritten
      }
    }
  
  
  return status;
  }



//
// glue_solve_tri


template<typename T1, typename T2>
inline
void
glue_solve_tri_default::apply(Mat<typename T1::elem_type>& out, const Glue<T1,T2,glue_solve_tri_default>& X)
  {
  arma_extra_debug_sigprint();
  
  const bool status = glue_solve_tri_default::apply( out, X.A, X.B, X.aux_uword );
  
  if(status == false)
    {
    out.soft_reset();
    arma_stop_runtime_error("solve(): solution not found");
    }
  }



template<typename eT, typename T1, typename T2>
inline
bool
glue_solve_tri_default::apply(Mat<eT>& actual_out, const Base<eT,T1>& A_expr, const Base<eT,T2>& B_expr, const uword flags)
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  const bool triu       = bool(flags & solve_opts::flag_triu);
  const bool tril       = bool(flags & solve_opts::flag_tril);
  const bool allow_ugly = false;
  
  arma_extra_debug_print("glue_solve_tri_default::apply(): enabled flags:");
  
  if(triu)  { arma_extra_debug_print("triu"); }
  if(tril)  { arma_extra_debug_print("tril"); }
  
  const quasi_unwrap<T1> U(A_expr.get_ref());
  const Mat<eT>& A     = U.M;
  
  arma_debug_check( (A.is_square() == false), "solve(): matrix marked as triangular must be square sized" );
  
  const uword layout   = (triu) ? uword(0) : uword(1);
  const bool  is_alias = U.is_alias(actual_out);
  
  T    rcond  = T(0);
  bool status = false;
  
  Mat<eT>  tmp;
  Mat<eT>& out = (is_alias) ? tmp : actual_out;
  
  status = auxlib::solve_trimat_rcond(out, rcond, A, B_expr.get_ref(), layout, allow_ugly);  // A is not modified
  
  if( (status == true) && (rcond > T(0)) && (rcond < auxlib::epsilon_lapack(A)) )
    {
    arma_debug_warn_level(2, "solve(): solution computed, but system is singular to working precision (rcond: ", rcond, ")");
    }
  
  
  if(status == false)
    {
    arma_extra_debug_print("glue_solve_tri::apply(): solving rank deficient system");
    
    if(rcond > T(0))
      {
      arma_debug_warn_level(2, "solve(): system is singular (rcond: ", rcond, "); attempting approx solution");
      }
    else
      {
      arma_debug_warn_level(2, "solve(): system is singular; attempting approx solution");
      }
    
    Mat<eT> triA = (triu) ? trimatu(A) : trimatl(A);  // trimatu() and trimatl() return the same type
    
    status = auxlib::solve_approx_svd(out, triA, B_expr.get_ref());  // triA is overwritten
    }
  
  
  if(is_alias)  { actual_out.steal_mem(out); }
  
  return status;
  }



template<typename T1, typename T2>
inline
void
glue_solve_tri::apply(Mat<typename T1::elem_type>& out, const Glue<T1,T2,glue_solve_tri>& X)
  {
  arma_extra_debug_sigprint();
  
  const bool status = glue_solve_tri::apply( out, X.A, X.B, X.aux_uword );
  
  if(status == false)
    {
    out.soft_reset();
    arma_stop_runtime_error("solve(): solution not found");
    }
  }



template<typename eT, typename T1, typename T2>
inline
bool
glue_solve_tri::apply(Mat<eT>& actual_out, const Base<eT,T1>& A_expr, const Base<eT,T2>& B_expr, const uword flags)
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  const bool fast         = bool(flags & solve_opts::flag_fast        );
  const bool equilibrate  = bool(flags & solve_opts::flag_equilibrate );
  const bool no_approx    = bool(flags & solve_opts::flag_no_approx   );
  const bool triu         = bool(flags & solve_opts::flag_triu        );
  const bool tril         = bool(flags & solve_opts::flag_tril        );
  const bool allow_ugly   = bool(flags & solve_opts::flag_allow_ugly  );
  const bool likely_sympd = bool(flags & solve_opts::flag_likely_sympd);
  const bool refine       = bool(flags & solve_opts::flag_refine      );
  const bool no_trimat    = bool(flags & solve_opts::flag_no_trimat   );
  
  arma_extra_debug_print("glue_solve_tri::apply(): enabled flags:");
  
  if(fast        )  { arma_extra_debug_print("fast");         }
  if(equilibrate )  { arma_extra_debug_print("equilibrate");  }
  if(no_approx   )  { arma_extra_debug_print("no_approx");    }
  if(triu        )  { arma_extra_debug_print("triu");         }
  if(tril        )  { arma_extra_debug_print("tril");         }
  if(allow_ugly  )  { arma_extra_debug_print("allow_ugly");   }
  if(likely_sympd)  { arma_extra_debug_print("likely_sympd"); }
  if(refine      )  { arma_extra_debug_print("refine");       }
  if(no_trimat   )  { arma_extra_debug_print("no_trimat");    }
  
  if(no_trimat || equilibrate || refine)
    {
    const uword mask = ~(solve_opts::flag_triu | solve_opts::flag_tril);
    
    return glue_solve_gen::apply(actual_out, ((triu) ? trimatu(A_expr.get_ref()) : trimatl(A_expr.get_ref())), B_expr, (flags & mask));
    }
  
  if(likely_sympd)  { arma_debug_warn_level(1, "solve(): option 'likely_sympd' ignored for triangular matrix"); }
  
  const quasi_unwrap<T1> U(A_expr.get_ref());
  const Mat<eT>& A     = U.M;
  
  arma_debug_check( (A.is_square() == false), "solve(): matrix marked as triangular must be square sized" );
  
  const uword layout   = (triu) ? uword(0) : uword(1);
  const bool  is_alias = U.is_alias(actual_out);
  
  T    rcond  = T(0);
  bool status = false;
  
  Mat<eT>  tmp;
  Mat<eT>& out = (is_alias) ? tmp : actual_out;
  
  if(fast)
    {
    status = auxlib::solve_trimat_fast(out, A, B_expr.get_ref(), layout);  // A is not modified
    }
  else
    {
    status = auxlib::solve_trimat_rcond(out, rcond, A, B_expr.get_ref(), layout, allow_ugly);  // A is not modified
    }
  
  if( (status == true) && (rcond > T(0)) && (rcond < auxlib::epsilon_lapack(A)) )
    {
    arma_debug_warn_level(2, "solve(): solution computed, but system is singular to working precision (rcond: ", rcond, ")");
    }
  
  
  if( (status == false) && (no_approx == false) )
    {
    arma_extra_debug_print("glue_solve_tri::apply(): solving rank deficient system");
    
    if(rcond > T(0))
      {
      arma_debug_warn_level(2, "solve(): system is singular (rcond: ", rcond, "); attempting approx solution");
      }
    else
      {
      arma_debug_warn_level(2, "solve(): system is singular; attempting approx solution");
      }
    
    Mat<eT> triA = (triu) ? trimatu(A) : trimatl(A);  // trimatu() and trimatl() return the same type
    
    status = auxlib::solve_approx_svd(out, triA, B_expr.get_ref());  // triA is overwritten
    }
  
  
  if(is_alias)  { actual_out.steal_mem(out); }
  
  return status;
  }



//! @}
