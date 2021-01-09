/* $Id$ */
# ifndef CPPAD_LOG_OP_INCLUDED
# define CPPAD_LOG_OP_INCLUDED

/* --------------------------------------------------------------------------
CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-15 Bradley M. Bell

CppAD is distributed under multiple licenses. This distribution is under
the terms of the
                    GNU General Public License Version 3.

A copy of this license is included in the COPYING file of this distribution.
Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
-------------------------------------------------------------------------- */

namespace CppAD { // BEGIN_CPPAD_NAMESPACE
/*!
\file log_op.hpp
Forward and reverse mode calculations for z = log(x).
*/

/*!
Compute forward mode Taylor coefficient for result of op = LogOp.

The C++ source code corresponding to this operation is
\verbatim
	z = log(x)
\endverbatim

\copydetails forward_unary1_op
*/
template <class Base>
inline void forward_log_op(
	size_t p           ,
	size_t q           ,
	size_t i_z         ,
	size_t i_x         ,
	size_t cap_order   ,
	Base*  taylor      )
{
	size_t k;

	// check assumptions
	CPPAD_ASSERT_UNKNOWN( NumArg(LogOp) == 1 );
	CPPAD_ASSERT_UNKNOWN( NumRes(LogOp) == 1 );
	CPPAD_ASSERT_UNKNOWN( q < cap_order );
	CPPAD_ASSERT_UNKNOWN( p <= q );

	// Taylor coefficients corresponding to argument and result
	Base* x = taylor + i_x * cap_order;
	Base* z = taylor + i_z * cap_order;

	if( p == 0 )
	{	z[0] = log( x[0] );
		p++;
		if( q == 0 )
			return;
	}
	if ( p == 1 )
	{	z[1] = x[1] / x[0];
		p++;
	}
	for(size_t j = p; j <= q; j++)
	{
		z[j] = -z[1] * x[j-1];
		for(k = 2; k < j; k++)
			z[j] -= Base(k) * z[k] * x[j-k];
		z[j] /= Base(j);
		z[j] += x[j];
		z[j] /= x[0];
	}
}

/*!
Muiltiple directions Taylor coefficient for op = LogOp.

The C++ source code corresponding to this operation is
\verbatim
	z = log(x)
\endverbatim

\copydetails forward_unary1_op_dir
*/
template <class Base>
inline void forward_log_op_dir(
	size_t q           ,
	size_t r           ,
	size_t i_z         ,
	size_t i_x         ,
	size_t cap_order   ,
	Base*  taylor      )
{

	// check assumptions
	CPPAD_ASSERT_UNKNOWN( NumArg(LogOp) == 1 );
	CPPAD_ASSERT_UNKNOWN( NumRes(LogOp) == 1 );
	CPPAD_ASSERT_UNKNOWN( 0 < q );
	CPPAD_ASSERT_UNKNOWN( q < cap_order );

	// Taylor coefficients corresponding to argument and result
	size_t num_taylor_per_var = (cap_order-1) * r + 1;
	Base* x = taylor + i_x * num_taylor_per_var;
	Base* z = taylor + i_z * num_taylor_per_var;

	size_t m = (q-1) * r + 1;
	for(size_t ell = 0; ell < r; ell++)
	{	z[m+ell] = Base(q) * x[m+ell];
		for(size_t k = 1; k < q; k++)
			z[m+ell] -= Base(k) * z[(k-1)*r+1+ell] * x[(q-k-1)*r+1+ell];
		z[m+ell] /= (Base(q) * x[0]);
	}
}

/*!
Compute zero order forward mode Taylor coefficient for result of op = LogOp.

The C++ source code corresponding to this operation is
\verbatim
	z = log(x)
\endverbatim

\copydetails forward_unary1_op_0
*/
template <class Base>
inline void forward_log_op_0(
	size_t i_z         ,
	size_t i_x         ,
	size_t cap_order   ,
	Base*  taylor      )
{

	// check assumptions
	CPPAD_ASSERT_UNKNOWN( NumArg(LogOp) == 1 );
	CPPAD_ASSERT_UNKNOWN( NumRes(LogOp) == 1 );
	CPPAD_ASSERT_UNKNOWN( 0 < cap_order );

	// Taylor coefficients corresponding to argument and result
	Base* x = taylor + i_x * cap_order;
	Base* z = taylor + i_z * cap_order;

	z[0] = log( x[0] );
}

/*!
Compute reverse mode partial derivatives for result of op = LogOp.

The C++ source code corresponding to this operation is
\verbatim
	z = log(x)
\endverbatim

\copydetails reverse_unary1_op
*/

template <class Base>
inline void reverse_log_op(
	size_t      d            ,
	size_t      i_z          ,
	size_t      i_x          ,
	size_t      cap_order    ,
	const Base* taylor       ,
	size_t      nc_partial   ,
	Base*       partial      )
{	size_t j, k;

	// check assumptions
	CPPAD_ASSERT_UNKNOWN( NumArg(LogOp) == 1 );
	CPPAD_ASSERT_UNKNOWN( NumRes(LogOp) == 1 );
	CPPAD_ASSERT_UNKNOWN( d < cap_order );
	CPPAD_ASSERT_UNKNOWN( d < nc_partial );

	// Taylor coefficients and partials corresponding to argument
	const Base* x  = taylor  + i_x * cap_order;
	Base* px       = partial + i_x * nc_partial;

	// Taylor coefficients and partials corresponding to result
	const Base* z  = taylor  + i_z * cap_order;
	Base* pz       = partial + i_z * nc_partial;

	// If pz is zero, make sure this operation has no effect
	// (zero times infinity or nan would be non-zero).
	bool skip(true);
	for(size_t i_d = 0; i_d <= d; i_d++)
		skip &= IdenticalZero(pz[i_d]);
	if( skip )
		return;

	j = d;
	while(j)
	{	// scale partial w.r.t z[j]
		pz[j]   /= x[0];

		px[0]   -= pz[j] * z[j];
		px[j]   += pz[j];

		// further scale partial w.r.t. z[j]
		pz[j]   /= Base(j);

		for(k = 1; k < j; k++)
		{	pz[k]   -= pz[j] * Base(k) * x[j-k];
			px[j-k] -= pz[j] * Base(k) * z[k];
		}
		--j;
	}
	px[0] += pz[0] / x[0];
}

} // END_CPPAD_NAMESPACE
# endif
