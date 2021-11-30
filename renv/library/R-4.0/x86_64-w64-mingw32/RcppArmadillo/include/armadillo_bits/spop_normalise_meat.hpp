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


//! \addtogroup spop_normalise
//! @{



template<typename T1>
inline
void
spop_normalise::apply(SpMat<typename T1::elem_type>& out, const SpOp<T1,spop_normalise>& expr)
  {
  arma_extra_debug_sigprint();
  
  const uword p   = expr.aux_uword_a;
  const uword dim = expr.aux_uword_b;
  
  arma_debug_check( (p   == 0), "normalise(): parameter 'p' must be greater than zero" );
  arma_debug_check( (dim >  1), "normalise(): parameter 'dim' must be 0 or 1"          );
  
  const unwrap_spmat<T1> U(expr.m);
  
  spop_normalise::apply_direct(out, U.M, p, dim);
  }



template<typename eT>
inline
void
spop_normalise::apply_direct(SpMat<eT>& out, const SpMat<eT>& X, const uword p, const uword dim)
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  X.sync();
  
  if( X.is_empty() || (X.n_nonzero == 0) )  { return; }
  
  if(dim == 0)
    {
    podarray<T> norm_vals(X.n_cols);
    
    T* norm_vals_mem = norm_vals.memptr();
    
    for(uword i=0; i < norm_vals.n_elem; ++i)
      {
      const uword      col_offset = X.col_ptrs[i    ];
      const uword next_col_offset = X.col_ptrs[i + 1];
      
      const eT* start_ptr = &X.values[     col_offset];
      const eT*   end_ptr = &X.values[next_col_offset];
      
      const uword n_elem = end_ptr - start_ptr;
      
      const Col<eT> fake_vec(const_cast<eT*>(start_ptr), n_elem, false, false);
      
      const T norm_val = norm(fake_vec, p);
      
      norm_vals_mem[i] = (norm_val != T(0)) ? norm_val : T(1);
      }
    
    const uword N = X.n_nonzero;
    
    umat    locs(2, N);
    Col<eT> vals(   N);
    
    uword* locs_mem = locs.memptr();
    eT*    vals_mem = vals.memptr();
    
    typename SpMat<eT>::const_iterator it = X.begin();
    
    uword new_n_nonzero = 0;
    
    for(uword i=0; i < N; ++i)
      {
      const uword row = it.row();
      const uword col = it.col();
        
      const eT val = (*it) / norm_vals_mem[col];
      
      if(val != eT(0))
        {
        (*vals_mem) = val;  vals_mem++;
        
        (*locs_mem) = row;  locs_mem++;
        (*locs_mem) = col;  locs_mem++;
        
        new_n_nonzero++;
        }
      
      ++it;
      }
    
    const umat    tmp_locs(locs.memptr(), 2, new_n_nonzero, false, false);
    const Col<eT> tmp_vals(vals.memptr(),    new_n_nonzero, false, false);
    
    SpMat<eT> tmp(tmp_locs, tmp_vals, X.n_rows, X.n_cols, false, false);
    
    out.steal_mem(tmp);
    }
  else
  if(dim == 1)
    {
    podarray< T> norm_vals(X.n_rows);
    podarray<eT>  row_vals(X.n_cols);  // worst case scenario
    
    T* norm_vals_mem = norm_vals.memptr();
    eT* row_vals_mem =  row_vals.memptr();
    
    for(uword i=0; i < norm_vals.n_elem; ++i)
      {
      // typename SpMat<eT>::const_row_iterator row_it     = X.begin_row(i);
      // typename SpMat<eT>::const_row_iterator row_it_end = X.end_row(i);
      // 
      // uword count = 0;
      // 
      // for(; row_it != row_it_end; ++row_it)
      //   {
      //   row_vals_mem[count] = (*row_it);
      //   ++count;
      //   }
      
      
      // using the .at() accessor, as it's faster than const_row_iterator for accessing a single row
      
      uword count = 0;
      
      for(uword col=0; col < X.n_cols; ++col)
        {
        const eT val = X.at(i,col);
        
        if(val != eT(0))
          {
          row_vals_mem[count] = val;
          ++count;
          }
        }
      
      const Row<eT> fake_vec(row_vals_mem, count, false, false);
      
      const T norm_val = norm(fake_vec, p);
      
      norm_vals_mem[i] = (norm_val != T(0)) ? norm_val : T(1);
      }
    
    const uword N = X.n_nonzero;
    
    umat    locs(2, N);
    Col<eT> vals(   N);
    
    uword* locs_mem = locs.memptr();
    eT*    vals_mem = vals.memptr();
    
    typename SpMat<eT>::const_iterator it = X.begin();
    
    uword new_n_nonzero = 0;
    
    for(uword i=0; i < N; ++i)
      {
      const uword row = it.row();
      const uword col = it.col();
        
      const eT val = (*it) / norm_vals_mem[row];
      
      if(val != eT(0))
        {
        (*vals_mem) = val;  vals_mem++;
        
        (*locs_mem) = row;  locs_mem++;
        (*locs_mem) = col;  locs_mem++;
        
        new_n_nonzero++;
        }
      
      ++it;
      }
    
    const umat    tmp_locs(locs.memptr(), 2, new_n_nonzero, false, false);
    const Col<eT> tmp_vals(vals.memptr(),    new_n_nonzero, false, false);
    
    SpMat<eT> tmp(tmp_locs, tmp_vals, X.n_rows, X.n_cols, false, false);
    
    out.steal_mem(tmp);
    }
  }



//! @}
