/* $Id$ */
# ifndef CPPAD_BASE_STD_MATH_INCLUDED
# define CPPAD_BASE_STD_MATH_INCLUDED

/* --------------------------------------------------------------------------
CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-14 Bradley M. Bell

CppAD is distributed under multiple licenses. This distribution is under
the terms of the 
                    GNU General Public License Version 3.

A copy of this license is included in the COPYING file of this distribution.
Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
-------------------------------------------------------------------------- */

/* 
$begin base_std_math$$
$spell
	inline
	fabs
	isnan
	alloc
	std
	acos
	asin
	atan
	cos
	exp
	sqrt
	const
	CppAD
	namespace
	erf
$$

$section Base Type Requirements for Standard Math Functions$$
$index math, base require$$
$index base, math require$$
$index require, base math$$

$head Purpose$$
These definitions are required for the user's code to use the type
$codei%AD<%Base%>%$$:

$head Unary Standard Math$$ 
$index math, base unary$$
$index base, unary math$$
$index unary, base math$$
The type $icode Base$$ must support the following functions
unary standard math functions (in the CppAD namespace):
$table
$bold Syntax$$ $cnext $bold Result$$ 
$rnext
$icode%y% = abs(%x%)%$$  $cnext absolute value     $rnext
$icode%y% = acos(%x%)%$$ $cnext inverse cosine     $rnext
$icode%y% = asin(%x%)%$$ $cnext inverse sine       $rnext
$icode%y% = atan(%x%)%$$ $cnext inverse tangent    $rnext
$icode%y% = cos(%x%)%$$  $cnext cosine             $rnext
$icode%y% = cosh(%x%)%$$ $cnext hyperbolic cosine  $rnext
$icode%y% = exp(%x%)%$$  $cnext exponential        $rnext
$icode%y% = fabs(%x%)%$$ $cnext absolute value     $rnext
$icode%y% = log(%x%)%$$  $cnext natural logarithm  $rnext
$icode%y% = sin(%x%)%$$  $cnext sine               $rnext
$icode%y% = sinh(%x%)%$$ $cnext hyperbolic sine    $rnext
$icode%y% = sqrt(%x%)%$$ $cnext square root        $rnext
$icode%y% = tan(%x%)%$$  $cnext tangent           
$tend
where the arguments and return value have the prototypes
$codei%
	const %Base%& %x%
	%Base%        %y%
%$$
For example,
$cref/base_alloc/base_alloc.hpp/Unary Standard Math/$$,


$head CPPAD_STANDARD_MATH_UNARY$$
$index CPPAD_STANDARD_MATH_UNARY$$
The macro invocation, within the CppAD namespace,
$codei%
	CPPAD_STANDARD_MATH_UNARY(%Base%, %Fun%)
%$$
defines the syntax
$codei%
	%y% = CppAD::%Fun%(%x%)
%$$
This macro uses the functions $codei%std::%Fun%$$ which
must be defined and have the same prototype as $codei%CppAD::%Fun%$$.
For example, 
$cref/float/base_float.hpp/Unary Standard Math/$$.

$head erf$$
$index erf, base require$$
$index base, erf require$$
$index require, base erf$$
If the error function is supported by the compiler,
$cref/CPPAD_COMPILER_HAS_ERF/erf/Method/CPPAD_COMPILER_HAS_ERF/$$
is one,
the type $icode Base$$ must support the syntax
$codei%
	%y% = CppAD::erf(%x%)
%$$
where $icode x$$ and $icode y$$ have the same prototype as above.
For example, see
$cref/base_alloc/base_alloc.hpp/erf/$$.

$head sign$$
$index sign, base require$$
$index base, sign require$$
$index require, base sign$$
The type $icode Base$$ must support the syntax
$codei%
	%y% = CppAD::sign(%x%)
%$$
which computes
$latex \[
y = \left\{ \begin{array}{ll}
	+1 & {\rm if} \; x > 0 \\
	 0 & {\rm if} \; x = 0 \\
	-1 & {\rm if} \; x < 0
\end{array} \right.
\] $$
where $icode x$$ and $icode y$$ have the same prototype as above.
For example, see
$cref/base_alloc/base_alloc.hpp/sign/$$.
Note that, if ordered comparisons are not defined for the type $icode Base$$,
the $code code sign$$ function should generate an assert if it is used; see
$cref/complex invalid unary math/base_complex.hpp/Invalid Unary Math/$$.

$head pow$$
$index pow, base require$$
$index base, pow require$$
$index require, base pow$$
The type $icode Base$$ must support the syntax
$codei%
	%z% = CppAD::pow(%x%, %y%)
%$$
which computes $latex z = x^y$$.
The arguments $icode x$$ and $icode y$$ have prototypes
$codei%
	const %Base%& %x%
	const %Base%& %y%
%$$
and the return value $icode z$$ has prototype
$codei%
	%Base% %z%
%$$
For example, see
$cref/base_alloc/base_alloc.hpp/pow/$$.


$head isnan$$
$index isnan, base type$$
$index base, isnan$$ 
If $icode Base$$ defines the $code isnan$$ function,
you may also have to provide a definition in the CppAD namespace
(to avoid a function ambiguity).
For example, see
$cref/base_complex/base_complex.hpp/isnan/$$.


$head limits$$
$index numeric, limits Base$$
$index limits, numeric Base$$
$index Base, numeric_limits$$
The $cref/numeric_limits/limits/$$ functions
$codei%
	%Base% CppAD::numeric_limits<%Base%>::epsilon()
	%Base% CppAD::numeric_limits<%Base%>::min()
	%Base% CppAD::numeric_limits<%Base%>::max()
%$$
must return machine epsilon,
minimum positive normalize value,
and maximum finite value for the type $icode Base$$.

$subhead epsilon$$
The deprecated function $cref epsilon$$ function
must also be included. It can be implemented using
$codei%
namespace CppAD {
	template <> inline %Base% epsilon<%Base%>(void)
	{	return numeric_limits<%Base%>::epsilon(); }
}
%$$

$end
-------------------------------------------------------------------------------
*/

# include <cmath>

namespace CppAD { // BEGIN_CPPAD_NAMESPACE

/*!
\file base_std_math.hpp
Defintions that aid meeting Base type requirements for standard math functions.
*/

/*!
\def CPPAD_STANDARD_MATH_UNARY(Type, Fun)
This macro defines the function
\verbatim
	y = CppAD:Fun(x)
\endverbatim
where the argument \c x and return value \c y have type \c Type
using the corresponding function <code>std::Fun</code>.
*/
# define CPPAD_STANDARD_MATH_UNARY(Type, Fun) \
	inline Type Fun(const Type& x)            \
	{	return std::Fun(x); }

} // END_CPPAD_NAMESPACE

# endif
