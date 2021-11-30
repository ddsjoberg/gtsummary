// $Id$
# ifndef CPPAD_BASE_REQUIRE_INCLUDED
# define CPPAD_BASE_REQUIRE_INCLUDED

/* --------------------------------------------------------------------------
CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-15 Bradley M. Bell

CppAD is distributed under multiple licenses. This distribution is under
the terms of the
                    GNU General Public License Version 3.

A copy of this license is included in the COPYING file of this distribution.
Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
-------------------------------------------------------------------------- */

/*
$begin base_require$$
$spell
	ostream
	alloc
	eps
	std
	Lt
	Le
	Eq
	Ge
	Gt
	cppad.hpp
	namespace
	acos
	asin
	atan
	cos
	sqrt
	optimizations
	bool
	const
	CppAD
	enum
	Lt
	Le
	Eq
	Ge
	Gt
	inline
	Op
	std
	CondExp
$$

$index Base, require$$
$index require, Base type$$
$index type, Base require$$

$section AD<Base> Requirements for Base Type$$

$head Syntax$$
$code include <cppad/base_require.hpp>$$

$head Warning$$
This is a preliminary version of these specifications
and it is subject to change in future versions of CppAD.

$head Purpose$$
This section lists the requirements that the type
$icode Base$$ so that the type $codei%AD<%Base%>%$$ can be used.

$subhead Standard Base Types$$
In the case where $icode Base$$ is
$code float$$,
$code double$$,
$code std::complex<float>$$,
$code std::complex<double>$$,
or $codei%AD<%Other%>%$$,
these requirements are provided by including the file
$code cppad/cppad.hpp$$.

$head Include Order$$
If you are linking a non-standard base type to CppAD,
you must first include the file $code cppad/base_require.hpp$$,
then provide the specifications below,
and then include the file $code cppad/cppad.hpp$$.

$head Numeric Type$$
The type $icode Base$$ must support all the operations for a
$cref NumericType$$.

$head Output Operator$$
$index output, base operator$$
$index base, output operator$$
$index operator, base output$$
The type $icode Base$$ must support the syntax
$codei%
	%os% << %x%
%$$
where $icode os$$ is an $code std::ostream&$$
and $icode x$$ is a $code const base_alloc&$$.
For example, see
$cref/base_alloc/base_alloc.hpp/Output Operator/$$.

$head Integer$$
$index Integer, base require$$
$index base, Integer require$$
$index require, base Integer$$
The type $icode Base$$ must support the syntax
$codei%
	%i% = CppAD::Integer(%x%)
%$$
which converts $icode x$$ to an $code int$$.
The argument $icode x$$ has prototype
$codei%
	const %Base%& %x%
%$$
and the return value $icode i$$ has prototype
$codei%
	int %i%
%$$

$subhead Suggestion$$
In many cases, the $icode Base$$ version of the $code Integer$$ function
can be defined by
$codei%
namespace CppAD {
	inline int Integer(const %Base%& x)
	{	return static_cast<int>(x); }
}
%$$
For example, see
$cref/base_float/base_float.hpp/Integer/$$ and
$cref/base_alloc/base_alloc.hpp/Integer/$$.

$childtable%
	omh/base_require/base_member.omh%
	cppad/local/base_cond_exp.hpp%
	omh/base_require/base_identical.omh%
	omh/base_require/base_ordered.omh%
	cppad/local/base_std_math.hpp%
	omh/base_require/base_example.omh
%$$

$end
*/

// definitions that must come before base implementations
# include <cppad/error_handler.hpp>
# include <cppad/local/define.hpp>
# include <cppad/local/cppad_assert.hpp>
# include <cppad/local/declare_ad.hpp>

// grouping documentation by feature
# include <cppad/local/base_cond_exp.hpp>
# include <cppad/local/base_std_math.hpp>

// must define template class numeric_limits before the base cases
# include <cppad/local/limits.hpp>
# include <cppad/local/epsilon.hpp> // deprecated

// base cases that come with CppAD
# include <cppad/local/base_float.hpp>
# include <cppad/local/base_double.hpp>
# include <cppad/local/base_complex.hpp>

# endif
