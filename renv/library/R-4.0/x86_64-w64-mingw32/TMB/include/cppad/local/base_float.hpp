/* $Id$ */
# ifndef CPPAD_BASE_FLOAT_INCLUDED
# define CPPAD_BASE_FLOAT_INCLUDED
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
$begin base_float.hpp$$
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

$index float, Base$$
$index Base, float$$
$index float, Base$$

$section Enable use of AD<Base> where Base is float$$

$head CondExpOp$$
The type $code float$$ is a relatively simple type that supports
$code <$$, $code <=$$, $code ==$$, $code >=$$, and $code >$$ operators; see
$cref/ordered type/base_cond_exp/CondExpTemplate/Ordered Type/$$.
Hence its $code CondExpOp$$ function is defined by
$codep */
namespace CppAD {
	inline float CondExpOp( 
		enum CompareOp     cop          ,
		const float&       left         ,
		const float&       right        , 
		const float&       exp_if_true  , 
		const float&       exp_if_false )
	{	return CondExpTemplate(cop, left, right, exp_if_true, exp_if_false);
	}
}
/* $$

$head CondExpRel$$
The $cref/CPPAD_COND_EXP_REL/base_cond_exp/CondExpRel/$$ macro invocation
$codep */
namespace CppAD {
	CPPAD_COND_EXP_REL(float)
}
/* $$
uses $code CondExpOp$$ above to
define $codei%CondExp%Rel%$$ for $code float$$ arguments
and $icode%Rel%$$ equal to
$code Lt$$, $code Le$$, $code Eq$$, $code Ge$$, and $code Gt$$.

$head EqualOpSeq$$
The type $code float$$ is simple (in this respect) and so we define
$codep */
namespace CppAD {
	inline bool EqualOpSeq(const float& x, const float& y)
	{	return x == y; }
}
/* $$

$head Identical$$
The type $code float$$ is simple (in this respect) and so we define
$codep */
namespace CppAD {
	inline bool IdenticalPar(const float& x)
	{	return true; }
	inline bool IdenticalZero(const float& x)
	{	return (x == 0.f); }
	inline bool IdenticalOne(const float& x)
	{	return (x == 1.f); }
	inline bool IdenticalEqualPar(const float& x, const float& y)
	{	return (x == y); }
}
/* $$

$head Integer$$
$codep */
namespace CppAD {
	inline int Integer(const float& x)
	{	return static_cast<int>(x); }
}
/* $$

$head Ordered$$
The $code float$$ type supports ordered comparisons
$codep */
namespace CppAD {
	inline bool GreaterThanZero(const float& x)
	{	return x > 0.f; }
	inline bool GreaterThanOrZero(const float& x)
	{	return x >= 0.f; }
	inline bool LessThanZero(const float& x)
	{	return x < 0.f; }
	inline bool LessThanOrZero(const float& x)
	{	return x <= 0.f; }
	inline bool abs_geq(const float& x, const float& y)
	{	return std::fabs(x) >= std::fabs(y); }
}
/* $$

$head Unary Standard Math$$
The following macro invocations define the unary standard math functions
required to use $code AD<float>$$:
(in the CppAD namespace)
$codep */
namespace CppAD {
	CPPAD_STANDARD_MATH_UNARY(float, acos)
	CPPAD_STANDARD_MATH_UNARY(float, asin)
	CPPAD_STANDARD_MATH_UNARY(float, atan)
	CPPAD_STANDARD_MATH_UNARY(float, cos)
	CPPAD_STANDARD_MATH_UNARY(float, cosh)
	CPPAD_STANDARD_MATH_UNARY(float, exp)
	CPPAD_STANDARD_MATH_UNARY(float, fabs)
	CPPAD_STANDARD_MATH_UNARY(float, log)
	CPPAD_STANDARD_MATH_UNARY(float, log10)
	CPPAD_STANDARD_MATH_UNARY(float, sin)
	CPPAD_STANDARD_MATH_UNARY(float, sinh)
	CPPAD_STANDARD_MATH_UNARY(float, sqrt)
	CPPAD_STANDARD_MATH_UNARY(float, tan)
	CPPAD_STANDARD_MATH_UNARY(float, tanh)
# if CPPAD_COMPILER_HAS_ERF
	CPPAD_STANDARD_MATH_UNARY(float, erf)
# endif
}
/* $$
The absolute value function is special because its $code std$$ name is 
$code fabs$$
$codep */
namespace CppAD {
	inline float abs(const float& x)
	{	return std::fabs(x); }
}
/* $$

$head sign$$
The following defines the $code CppAD::sign$$ function that
is required to use $code AD<float>$$:
$codep */
namespace CppAD {
	inline float sign(const float& x)
	{	if( x > 0.f )
			return 1.f;
		if( x == 0.f )
			return 0.f;
		return -1.f;
	}
}
/* $$
 
$head pow $$
The following defines a $code CppAD::pow$$ function that
is required to use $code AD<float>$$:
$codep */
namespace CppAD {
	inline float pow(const float& x, const float& y)
	{ return std::pow(x, y); }
}
/*$$

$head limits$$
The following defines the numeric limits functions
$code epsilon$$, $code min$$, and $code max$$ for the type
$code float$$:
$codep */
namespace CppAD {
	template <>
	class numeric_limits<float> {
	public:
		// machine epsilon
		static float epsilon(void)
		{	return std::numeric_limits<float>::epsilon(); }
		// minimum positive normalized value
		static float min(void)
		{	return std::numeric_limits<float>::min(); }
		// maximum finite value
		static float max(void)
		{	return std::numeric_limits<float>::max(); }
	};
	// deprecated machine epsilon
	template <> 
	inline float epsilon<float>(void)
	{	return numeric_limits<float>::epsilon(); }
}
/* $$
$end
*/


# endif
