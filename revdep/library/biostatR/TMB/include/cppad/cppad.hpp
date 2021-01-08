/* $Id$ */
# ifndef CPPAD_CPPAD_INCLUDED
# define CPPAD_CPPAD_INCLUDED
/* --------------------------------------------------------------------------
CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-14 Bradley M. Bell

CppAD is distributed under multiple licenses. This distribution is under
the terms of the
                    GNU General Public License Version 3.

A copy of this license is included in the COPYING file of this distribution.
Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
-------------------------------------------------------------------------- */
/*!
\file cppad.hpp
\brief includes the entire CppAD package in the necessary order.

\namespace CppAD
\brief contains all the variables and functions defined by the CppAD package.
*/

# include <cppad/base_require.hpp> // all base type requirements
// ---------------------------------------------------------------------------
// CppAD general purpose library routines (can be included separately)

# include <cppad/check_numeric_type.hpp>
# include <cppad/check_simple_vector.hpp>
# include <cppad/index_sort.hpp>
# include <cppad/local/cppad_assert.hpp>
# include <cppad/lu_solve.hpp>
# include <cppad/memory_leak.hpp>
# include <cppad/near_equal.hpp>
# include <cppad/ode_err_control.hpp>
# include <cppad/ode_gear.hpp>
# include <cppad/ode_gear_control.hpp>
# include <cppad/omp_alloc.hpp>
# include <cppad/poly.hpp>
# include <cppad/pow_int.hpp>
# include <cppad/romberg_mul.hpp>
# include <cppad/romberg_one.hpp>
# include <cppad/rosen_34.hpp>
# include <cppad/runge_45.hpp>
# include <cppad/speed_test.hpp>
# include <cppad/time_test.hpp>
# include <cppad/track_new_del.hpp>
# include <cppad/thread_alloc.hpp>
# include <cppad/vector.hpp>

// --------------------------------------------------------------------------
// System routines that can be used by rest of CppAD with out including 

# include <cstddef>
# include <iostream>
# include <complex>
# include <cmath>

// ---------------------------------------------------------------------------
// definitions needed by rest of includes

// definitions that come from the installation
# include <cppad/configure.hpp>

// definitions that are local to the CppAD include files
# include <cppad/local/define.hpp>

// vectors used with CppAD
# include <cppad/local/testvector.hpp>

// deprecated vectors used with CppAD
# include <cppad/local/test_vector.hpp>

// Declare classes and fucntions that are used before defined
# include <cppad/local/declare_ad.hpp>

// ---------------------------------------------------------------------------
// declare the AD<Base> template class

# include <cppad/local/ad.hpp>

// ---------------------------------------------------------------------------

# include <cppad/local/user_ad.hpp>  // AD class methods available to the user
// tape that tape for AD<Base> acts as a user of Base operations
// so user_ad.hpp must come before op.hpp
# include <cppad/local/op.hpp>       // executes taped operations
# include <cppad/local/ad_fun.hpp>   // ADFun objects

// ---------------------------------------------------------------------------
// library routines that require the rest of CppAD
# include <cppad/local/lu_ratio.hpp>
# include <cppad/local/bender_quad.hpp>
# include <cppad/local/opt_val_hes.hpp>

// undo definitions in Define.h
# include <cppad/local/undef.hpp>   

# endif
