/* $Id$ */
# ifndef CPPAD_LIMITS_INCLUDED
# define CPPAD_LIMITS_INCLUDED
/* --------------------------------------------------------------------------
CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-14 Bradley M. Bell

CppAD is distributed under multiple licenses. This distribution is under
the terms of the 
                    GNU General Public License Version 3.

A copy of this license is included in the COPYING file of this distribution.
Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
-------------------------------------------------------------------------- */

/*
------------------------------------------------------------------------------
$begin limits$$
$spell
	std
	eps
	CppAD
	namespace
	const
$$
$index limits, AD$$
$index AD, limits$$
$index epsilon, AD$$
$index limit, max$$
$index limit, min$$
$index limit, epsilon$$

$section Numeric Limits For an AD and Base Types$$

$head Syntax$$
$icode%eps% = numeric_limits<%Float%>::epsilon()
%$$
$icode%min% = numeric_limits<%Float%>::min()
%$$
$icode%max% = numeric_limits<%Float%>::max()
%$$

$head Purpose$$
Obtain the value of some of the C++ standard numeric limits
using the CppAD namespace version of $code numeric_limits$$.
These are all functions and have the prototype
$codei%
	%Float% numeric_limits<%Float%>::%fun%(%void%)
%$$
where $icode fun$$ is $code epsilon$$, $code min$$, or $code max$$.
$pre

$$
Note that C++ standard specifies that Non-fundamental standard
types, such as $codei%std::complex<%T%>%$$ shall not have specializations
of $code std::numeric_limits$$; see Section 18.2 of
ISO/IEC 14882:1998(E).

$head Float$$
These functions are defined for all $codei%AD<%Base%>%$$,
and for all corresponding $icode Base$$ types;
see $icode Base$$ type $cref/limits/base_std_math/limits/$$.

$head eps$$
The result $icode eps$$ is equal to machine epsilon and has prototype
$codei%
	%Float% %eps%
%$$
CppAD tests the value $icode eps$$ by checking that the following are true
$codei%
	1 != 1 + %eps%
	1 == 1 + %eps% / 2
%$$
where all the values, and calculations, are done with the precision
corresponding to $icode Float$$.
	

$head min$$
The result $icode min$$ is equal to 
the minimum positive normalized value and has prototype
$codei%
	%Float% %min%
%$$
CppAD tests the value $icode min$$ by checking that the following are true
$codei%
	abs( ((%min% / 100) * 100) / %min% - 1 ) > 3 * %eps%
	abs( ((%min% * 100) / (100 * (1 - %eps%)) / %min% - 1 ) < 3 * %eps%
%$$
where all the values, and calculations, are done with the precision
corresponding to $icode Float$$.

$head max$$
The result $icode max$$ is equal to 
the maximum finite value and has prototype
$codei%
	%Float% %max%
%$$
CppAD tests the value $icode max$$ by checking that the following are true
$codei%
	abs( ((%max% * 100) / 100) / %max% - 1 ) > 3 * %eps%
	abs( ((%max% / 100) * (100 * (1 - %eps%)) / %max% - 1 ) < 3 * %eps%
%$$
where all the values, and calculations, are done with the precision
corresponding to $icode Float$$.

$head Example$$
$children%
	example/limits.cpp
%$$
The file
$cref limits.cpp$$
contains an example and test of these functions.

$end 
------------------------------------------------------------------------------
*/
# include <iostream>

# include <cppad/configure.hpp>
# include <cppad/local/define.hpp>
# include <cppad/local/cppad_assert.hpp>
# include <cppad/local/declare_ad.hpp>

namespace CppAD { // BEGIN_CPPAD_NAMESPACE
/*!
\file limits.hpp
File that defines CppAD numeric_limits for AD types
*/

/// Default value for all undefined numeric_limits types
template <class Float>
class numeric_limits {
public:
	/// machine epsilon
	static Float epsilon(void)
	{	CPPAD_ASSERT_KNOWN(
		false,
		"numeric_limits<Float>::epsilon() is not specialized for this Float"
		);
		return Float(0);
	}
	/// minimum positive normalized value
	static Float min(void)
	{	CPPAD_ASSERT_KNOWN(
		false,
		"numeric_limits<Float>::min() is not specialized for this Float"
		);
		return Float(0);
	}
	/// maximum finite value
	static Float max(void)
	{	CPPAD_ASSERT_KNOWN(
		false,
		"numeric_limits<Float>::max() is not specialized for this Float"
		);
		return Float(0);
	}
};

/// Partial specialization that defines limits for for all AD types
template <class Base>
class numeric_limits< AD<Base> > {
public:
	/// machine epsilon
	static AD<Base> epsilon(void)
	{	return AD<Base>( numeric_limits<Base>::epsilon() ); }
	/// minimum positive normalized value
	static AD<Base> min(void)
	{	return AD<Base>( numeric_limits<Base>::min() ); }
	/// maximum finite value
	static AD<Base> max(void)
	{	return AD<Base>( numeric_limits<Base>::max() ); }
};

} // END_CPPAD_NAMESPACE
# endif
