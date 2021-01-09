/* $Id$ */
# ifndef CPPAD_BASE_DOUBLE_INCLUDED
# define CPPAD_BASE_DOUBLE_INCLUDED
/* --------------------------------------------------------------------------
CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-12 Bradley M. Bell

CppAD is distributed under multiple licenses. This distribution is under
the terms of the
                    GNU General Public License Version 3.

A copy of this license is included in the COPYING file of this distribution.
Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
-------------------------------------------------------------------------- */
# include <cppad/configure.hpp>
# include <limits>

/*
$begin base_double.hpp$$
$spell
	erf
	endif
	abs_geq
	acos
	asin
	atan
	cos
	sqrt
	tanh
	std
	fabs
	bool
	Lt Le Eq Ge Gt
	Rel
	CppAD
	CondExpOp
	namespace
	inline
	enum
	const
	exp
	const
$$

$index double, Base$$
$index Base, double$$
$index double, Base$$

$section Enable use of AD<Base> where Base is double$$

$head CondExpOp$$
The type $code double$$ is a relatively simple type that supports
$code <$$, $code <=$$, $code ==$$, $code >=$$, and $code >$$ operators; see
$cref/ordered type/base_cond_exp/CondExpTemplate/Ordered Type/$$.
Hence its $code CondExpOp$$ function is defined by
$codep */
namespace CppAD {
	inline double CondExpOp( 
		enum CompareOp     cop          ,
		const double&       left         ,
		const double&       right        , 
		const double&       exp_if_true  , 
		const double&       exp_if_false )
	{	return CondExpTemplate(cop, left, right, exp_if_true, exp_if_false);
	}
}
/* $$

$head CondExpRel$$
The $cref/CPPAD_COND_EXP_REL/base_cond_exp/CondExpRel/$$ macro invocation
$codep */
namespace CppAD {
	CPPAD_COND_EXP_REL(double)
}
/* $$
uses $code CondExpOp$$ above to
define $codei%CondExp%Rel%$$ for $code double$$ arguments
and $icode%Rel%$$ equal to
$code Lt$$, $code Le$$, $code Eq$$, $code Ge$$, and $code Gt$$.

$head EqualOpSeq$$
The type $code double$$ is simple (in this respect) and so we define
$codep */
namespace CppAD {
	inline bool EqualOpSeq(const double& x, const double& y)
	{	return x == y; }
}
/* $$

$head Identical$$
The type $code double$$ is simple (in this respect) and so we define
$codep */
namespace CppAD {
	inline bool IdenticalPar(const double& x)
	{	return true; }
	inline bool IdenticalZero(const double& x)
	{	return (x == 0.); }
	inline bool IdenticalOne(const double& x)
	{	return (x == 1.); }
	inline bool IdenticalEqualPar(const double& x, const double& y)
	{	return (x == y); }
}
/* $$

$head Integer$$
$codep */
namespace CppAD {
	inline int Integer(const double& x)
	{	return static_cast<int>(x); }
}
/* $$

$head Ordered$$
The $code double$$ type supports ordered comparisons
$codep */
namespace CppAD {
	inline bool GreaterThanZero(const double& x)
	{	return x > 0.; }
	inline bool GreaterThanOrZero(const double& x)
	{	return x >= 0.; }
	inline bool LessThanZero(const double& x)
	{	return x < 0.; }
	inline bool LessThanOrZero(const double& x)
	{	return x <= 0.; }
	inline bool abs_geq(const double& x, const double& y)
	{	return std::fabs(x) >= std::fabs(y); }
}
/* $$

$head Unary Standard Math$$
The following macro invocations define the unary standard math functions
required to use $code AD<double>$$:
$codep */
namespace CppAD {
	CPPAD_STANDARD_MATH_UNARY(double, acos)
	CPPAD_STANDARD_MATH_UNARY(double, asin)
	CPPAD_STANDARD_MATH_UNARY(double, atan)
	CPPAD_STANDARD_MATH_UNARY(double, cos)
	CPPAD_STANDARD_MATH_UNARY(double, cosh)
	CPPAD_STANDARD_MATH_UNARY(double, exp)
	CPPAD_STANDARD_MATH_UNARY(double, fabs)
	CPPAD_STANDARD_MATH_UNARY(double, log)
	CPPAD_STANDARD_MATH_UNARY(double, log10)
	CPPAD_STANDARD_MATH_UNARY(double, sin)
	CPPAD_STANDARD_MATH_UNARY(double, sinh)
	CPPAD_STANDARD_MATH_UNARY(double, sqrt)
	CPPAD_STANDARD_MATH_UNARY(double, tan)
	CPPAD_STANDARD_MATH_UNARY(double, tanh)
# if CPPAD_COMPILER_HAS_ERF
	CPPAD_STANDARD_MATH_UNARY(double, erf)
# endif
}
/* $$
The absolute value function is special because its $code std$$ name is 
$code fabs$$
$codep */
namespace CppAD {
	inline double abs(const double& x)
	{	return std::fabs(x); }
}
/* $$

$head sign$$
The following defines the $code CppAD::sign$$ function that
is required to use $code AD<double>$$:
$codep */
namespace CppAD {
	inline double sign(const double& x)
	{	if( x > 0. )
			return 1.;
		if( x == 0. )
			return 0.;
		return -1.;
	}
}
/* $$

$head pow $$
The following defines a $code CppAD::pow$$ function that
is required to use $code AD<double>$$:
$codep */
namespace CppAD {
	inline double pow(const double& x, const double& y)
	{ return std::pow(x, y); }
}
/*$$

$head limits$$
The following defines the numeric limits functions
$code epsilon$$, $code min$$, and $code max$$ for the type
$code double$$.
It also defines the deprecated $code epsilon$$ function:
$codep */
namespace CppAD {
	template <>
	class numeric_limits<double> {
	public:
		// machine epsilon
		static double epsilon(void)
		{	return std::numeric_limits<double>::epsilon(); }
		// minimum positive normalized value
		static double min(void)
		{	return std::numeric_limits<double>::min(); }
		// maximum finite value
		static double max(void)
		{	return std::numeric_limits<double>::max(); }
	};
	// deprecated machine epsilon
	template <> 
	inline double epsilon<double>(void)
	{	return numeric_limits<double>::epsilon(); }
}
/* $$
$end
*/

# endif
