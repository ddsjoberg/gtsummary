/* $Id$ */
# ifndef CPPAD_AD_BINARY_INCLUDED
# define CPPAD_AD_BINARY_INCLUDED

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
$begin ad_binary$$
$spell
	Op
	VecAD
	const
$$

$section AD Binary Arithmetic Operators$$

$index binary, operator$$
$index operator, binary$$

$index +, binary operator$$
$index add, binary operator$$
$index plus, binary operator$$

$index -, binary operator$$
$index subtract, binary operator$$
$index minus, binary operator$$

$index *, binary operator$$
$index multiply, binary operator$$
$index times, binary operator$$

$index /, binary operator$$
$index divide, binary operator$$


$head Syntax$$
$icode%z% = %x% %Op% %y%$$

$head Purpose$$
Performs arithmetic operations where either $icode x$$ or $icode y$$
has type 
$codei%AD<%Base%>%$$ or
$cref%VecAD<Base>::reference%VecAD%VecAD<Base>::reference%$$. 

$head Op$$
The operator $icode Op$$ is one of the following
$table
$bold Op$$  $cnext $bold Meaning$$ $rnext
$code +$$   $cnext $icode z$$ is $icode x$$ plus $icode y$$ $rnext
$code -$$   $cnext $icode z$$ is $icode x$$ minus $icode y$$ $rnext
$code *$$   $cnext $icode z$$ is $icode x$$ times $icode y$$ $rnext
$code /$$   $cnext $icode z$$ is $icode x$$ divided by $icode y$$ 
$tend

$head Base$$
The type $icode Base$$ is determined by the operand that
has type $codei%AD<%Base%>%$$ or $codei%VecAD<%Base%>::reference%$$.

$head x$$
The operand $icode x$$ has the following prototype
$codei%
	const %Type% &%x%
%$$
where $icode Type$$ is
$codei%VecAD<%Base%>::reference%$$,
$codei%AD<%Base%>%$$,
$icode Base$$, or
$code double$$.

$head y$$
The operand $icode y$$ has the following prototype
$codei%
	const %Type% &%y%
%$$
where $icode Type$$ is
$codei%VecAD<%Base%>::reference%$$,
$codei%AD<%Base%>%$$,
$icode Base$$, or
$code double$$.


$head z$$
The result $icode z$$ has the following prototype
$codei%
	%Type% %z%
%$$
where $icode Type$$ is
$codei%AD<%Base%>%$$.

$head Operation Sequence$$
This is an $cref/atomic/glossary/Operation/Atomic/$$
$cref/AD of Base/glossary/AD of Base/$$ operation
and hence it is part of the current
AD of $icode Base$$ 
$cref/operation sequence/glossary/Operation/Sequence/$$.

$children%
	example/add.cpp%
	example/sub.cpp%
	example/mul.cpp%
	example/div.cpp
%$$

$head Example$$
The following files contain examples and tests of these functions.
Each test returns true if it succeeds and false otherwise.
$table
$rref add.cpp$$
$rref sub.cpp$$
$rref mul.cpp$$
$rref div.cpp$$
$tend

$head Derivative$$
If $latex f$$ and $latex g$$ are 
$cref/Base functions/glossary/Base Function/$$

$subhead Addition$$
$latex \[
	\D{[ f(x) + g(x) ]}{x} = \D{f(x)}{x} + \D{g(x)}{x}
\] $$

$subhead Subtraction$$
$latex \[
	\D{[ f(x) - g(x) ]}{x} = \D{f(x)}{x} - \D{g(x)}{x}
\] $$

$subhead Multiplication$$
$latex \[
	\D{[ f(x) * g(x) ]}{x} = g(x) * \D{f(x)}{x} + f(x) * \D{g(x)}{x}
\] $$

$subhead Division$$
$latex \[
	\D{[ f(x) / g(x) ]}{x} = 
		[1/g(x)] * \D{f(x)}{x} - [f(x)/g(x)^2] * \D{g(x)}{x}
\] $$

$end 
-----------------------------------------------------------------------------
*/
# include <cppad/local/add.hpp>
# include <cppad/local/sub.hpp>
# include <cppad/local/mul.hpp>
# include <cppad/local/div.hpp>

# endif
