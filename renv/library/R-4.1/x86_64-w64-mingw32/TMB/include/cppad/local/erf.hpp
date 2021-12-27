// $Id:$
# ifndef CPPAD_ERF_INCLUDED
# define CPPAD_ERF_INCLUDED

/* --------------------------------------------------------------------------
CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-12 Bradley M. Bell

CppAD is distributed under multiple licenses. This distribution is under
the terms of the 
                    GNU General Public License Version 3.

A copy of this license is included in the COPYING file of this distribution.
Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
-------------------------------------------------------------------------- */

/*
-------------------------------------------------------------------------------
$begin erf$$

$section The AD Error Function$$
$spell
	std
	cmath
	Vedder
	Cpp
	namespace
	Vec
	erf
	const
$$

$index erf, AD function$$
$index error, AD function$$
$index function, error AD$$

$head Syntax$$
$icode%y% = erf(%x%)%$$


$head Description$$
Returns the value of the error function which is defined by
$latex \[
{\rm erf} (x) = \frac{2}{ \sqrt{\pi} } \int_0^x \exp( - t * t ) \; {\bf d} t
\] $$

$head x$$
The argument $icode x$$, and the result $icode y$$
have one of the following paris of prototypes:
$codei%
	const float%%                  &%x%,     float%%    %y%
	const double%%                 &%x%,     double%%   %y%
	const AD<%Base%>               &%x%,     AD<%Base%> %y%
	const VecAD<%Base%>::reference &%x%,     AD<%Base%> %y%
%$$


$head Operation Sequence$$
The AD of $icode Base$$
operation sequence used to calculate $icode y$$ is
$cref/independent/glossary/Operation/Independent/$$
of $icode x$$.

$head Method$$

$subhead CPPAD_COMPILER_HAS_ERF$$
$index CPPAD_COMPILER_HAS_ERF$$
This preprocessor symbol is one if
the function $codei%std::erf(double %x%)%$$ is defined the in the
include file $code <cmath>$$.
Otherwise this preprocessor symbol is zero.
If this preprocessor symbols is one,
CppAD uses the compiler's version of $code erf$$
and it corresponds to an $cref/atomic/glossary/Operation/Atomic/$$ operation.

$subhead Other$$
If the function $codei%std::erf(double %x%)%$$ is not defined, 
CppAD uses a fast approximation (few numerical operations) 
with relative error bound $latex 4 \times 10^{-4}$$; see
Vedder, J.D.,
$icode Simple approximations for the error function and its inverse$$,
American Journal of Physics, 
v 55, 
n 8, 
1987, 
p 762-3.

$head Example$$
$children%
	example/erf.cpp
%$$
The file
$cref erf.cpp$$
contains an example and test of this function.   
It returns true if it succeeds and false otherwise.

$end
-------------------------------------------------------------------------------
*/
# include <cppad/configure.hpp>
# include <cppad/local/cppad_assert.hpp>

// needed before one can use CPPAD_ASSERT_FIRST_CALL_NOT_PARALLEL
# include <cppad/thread_alloc.hpp>

# if ! CPPAD_COMPILER_HAS_ERF

// BEGIN CppAD namespace
namespace CppAD {   

template <class Type>
Type erf_template(const Type &x)
{	using CppAD::exp;
	const Type a = static_cast<Type>(993./880.);
	const Type b = static_cast<Type>(89./880.); 

	return tanh( (a + b * x * x) * x );
}

inline float erf(const float &x)
{	return erf_template(x); }

inline double erf(const double &x)
{	return erf_template(x); }

template <class Base>
inline AD<Base> erf(const AD<Base> &x)
{	return erf_template(x); }

template <class Base>
inline AD<Base> erf(const VecAD_reference<Base> &x)
{	return erf_template( x.ADBase() ); }


} // END CppAD namespace

# endif // CPPAD_COMPILER_HAS_ERF
# endif // CPPAD_ERF_INCLUDED
