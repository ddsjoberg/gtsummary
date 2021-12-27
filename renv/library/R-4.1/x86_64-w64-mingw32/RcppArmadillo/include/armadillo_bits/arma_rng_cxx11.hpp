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


//! \addtogroup arma_rng_cxx11
//! @{


class arma_rng_cxx11
  {
  public:
  
  typedef std::mt19937_64::result_type seed_type;
  
  inline void set_seed(const seed_type val);
  
  arma_inline int    randi_val();
  arma_inline double randu_val();
  arma_inline double randn_val();
  
  template<typename eT>
  arma_inline void randn_dual_val(eT& out1, eT& out2);
  
  template<typename eT>
  inline void randi_fill(eT* mem, const uword N, const int a, const int b);
  
  inline static int randi_max_val();
  
  template<typename eT>
  inline void randg_fill_simple(eT* mem, const uword N, const double a, const double b);
  
  template<typename eT>
  inline void randg_fill(eT* mem, const uword N, const double a, const double b);
  
  
  private:
  
  arma_aligned std::mt19937_64 engine;                           // typedef for std::mersenne_twister_engine with preset parameters
  
  arma_aligned std::uniform_int_distribution<int>     i_distr;   // by default uses a=0, b=std::numeric_limits<int>::max()
  
  arma_aligned std::uniform_real_distribution<double> u_distr;   // by default uses [0,1) interval
  
  arma_aligned std::normal_distribution<double>       n_distr;   // by default uses mean=0.0 and stddev=1.0
  };



inline
void
arma_rng_cxx11::set_seed(const arma_rng_cxx11::seed_type val)
  {
  engine.seed(val);
  
  i_distr.reset();
  u_distr.reset();
  n_distr.reset();
  }



arma_inline
int
arma_rng_cxx11::randi_val()
  {
  return i_distr(engine);
  }



arma_inline
double
arma_rng_cxx11::randu_val()
  {
  return u_distr(engine);
  }



arma_inline
double
arma_rng_cxx11::randn_val()
  {
  return n_distr(engine);
  }



template<typename eT>
arma_inline
void
arma_rng_cxx11::randn_dual_val(eT& out1, eT& out2)
  {
  out1 = eT( n_distr(engine) );
  out2 = eT( n_distr(engine) );
  }



template<typename eT>
inline
void
arma_rng_cxx11::randi_fill(eT* mem, const uword N, const int a, const int b)
  {
  std::uniform_int_distribution<int> local_i_distr(a, b);
  
  for(uword i=0; i<N; ++i)
    {
    mem[i] = eT(local_i_distr(engine));
    }
  }



inline
int
arma_rng_cxx11::randi_max_val()
  {
  return std::numeric_limits<int>::max();
  }



template<typename eT>
inline
void
arma_rng_cxx11::randg_fill_simple(eT* mem, const uword N, const double a, const double b)
  {
  std::gamma_distribution<double> g_distr(a,b);
  
  for(uword i=0; i<N; ++i)
    {
    mem[i] = eT(g_distr(engine));
    }
  }



template<typename eT>
inline
void
arma_rng_cxx11::randg_fill(eT* mem, const uword N, const double a, const double b)
  {
  #if defined(ARMA_USE_OPENMP)
    {
    if((N < 512) || omp_in_parallel())  { (*this).randg_fill_simple(mem, N, a, b); return; }
    
    typedef std::mt19937_64                  motor_type;
    typedef std::mt19937_64::result_type      ovum_type;
    typedef std::gamma_distribution<double>  distr_type;
    
    const uword n_threads = uword( mp_thread_limit::get() );
    
    std::vector<motor_type> g_motor(n_threads);
    std::vector<distr_type> g_distr(n_threads);
    
    const distr_type g_distr_base(a,b);
    
    for(uword t=0; t < n_threads; ++t)
      {
      motor_type& g_motor_t = g_motor[t];
      distr_type& g_distr_t = g_distr[t];
      
      g_motor_t.seed( ovum_type(t) + ovum_type((*this).randi_val()) );
      
      g_distr_t.param( g_distr_base.param() );
      }
    
    const uword chunk_size = N / n_threads;
    
    #pragma omp parallel for schedule(static) num_threads(int(n_threads))
    for(uword t=0; t < n_threads; ++t)
      {
      const uword start = (t+0) * chunk_size;
      const uword endp1 = (t+1) * chunk_size;
      
      motor_type& g_motor_t = g_motor[t];
      distr_type& g_distr_t = g_distr[t];
      
      for(uword i=start; i < endp1; ++i)  { mem[i] = eT( g_distr_t(g_motor_t)); }
      }
    
    motor_type& g_motor_0 = g_motor[0];
    distr_type& g_distr_0 = g_distr[0];
    
    for(uword i=(n_threads*chunk_size); i < N; ++i)  { mem[i] = eT( g_distr_0(g_motor_0)); }
    }
  #else
    {
    (*this).randg_fill_simple(mem, N, a, b);
    }
  #endif
  }


//! @}
