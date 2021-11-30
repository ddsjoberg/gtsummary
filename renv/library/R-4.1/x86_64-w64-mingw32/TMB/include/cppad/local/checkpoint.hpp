/* $Id$ */
# ifndef CPPAD_CHECKPOINT_INCLUDED
# define CPPAD_CHECKPOINT_INCLUDED

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
\file checkpoint.hpp
defining checkpoint functions.
*/

/*
$begin checkpoint$$
$spell
	cppad.hpp
	CppAD
	checkpoint
	checkpointing
	algo
	afun
	const
$$

$section Checkpointing Functions$$
$index function, checkpoint$$
$index checkpoint, function$$

$head Syntax$$
$codei%checkpoint<%Base%> %afun%(%name%, %algo%, %ax%, %ay%)
%afun%.option(%option_value%)
%algo%(%ax%, %ay%)
%afun%(%ax%, %ay%)
checkpoint<%Base%>::clear()%$$

$head Purpose$$
You can reduce the size of the tape and memory required for AD by 
checkpointing functions of the form $latex y = f(x)$$ where
$latex f : B^n \rightarrow B^m$$.

$head Method$$
The $code checkpoint$$ class is derived from $code atomic_base$$
and makes this easy.
It implements all the $code atomic_base$$
$cref/virtual functions/atomic_base/Virtual Functions/$$
and hence its source code $code cppad/local/checkpoint.hpp$$
provides an example implementation of $cref atomic_base$$.
The difference is that $code checkpoint.hpp$$ uses AD 
instead of user provided derivatives.

$head constructor$$
The constructor 
$codei%
	checkpoint<%Base%> %afun%(%name%, %algo%, %ax%, %ay%)
%$$
cannot be called in $cref/parallel/ta_in_parallel/$$ mode.
In addition, you cannot currently be recording 
$codei%AD<%Base%>%$$ operations when the constructor is called.
This class is implemented as a derived class of
$cref/atomic_base/atomic_ctor/atomic_base/$$ and hence 
some of its error message will refer to $code atomic_base$$.

$head Base$$
The type $icode Base$$ specifies the base type for AD operations.

$head ADVector$$
The type $icode ADVector$$ must be a
$cref/simple vector class/SimpleVector/$$ with elements of type
$codei%AD<%Base%>%$$.

$head name$$
This $icode checkpoint$$ constructor argument has prototype
$codei%
	const char* %name%
%$$
It is the name used for error reporting.
The suggested value for $icode name$$ is $icode afun$$; i.e.,
the same name as used for the function.

$head ax$$
This argument has prototype
$codei%
	const %ADVector%& %ax%
%$$
and size must be equal to $icode n$$.
It specifies vector $latex x \in B^n$$ 
at which an $codei%AD<%Base%>%$$ version of 
$latex y = f(x)$$ is to be evaluated.

$head ay$$
This argument has prototype
$codei%
	%ADVector%& %ay%
%$$
Its input size must be equal to $icode m$$ and does not change.
The input values of its elements do not matter.
Upon return, it is an $codei%AD<%Base%>%$$ version of 
$latex y = f(x)$$.

$head option$$
The $code option$$ syntax can be used to set the type of sparsity
pattern used by $icode afun$$.
This is an $codei%atomic_base<%Base%>%$$ function and its documentation
can be found at $cref atomic_option$$.

$head algo$$
The type of $icode algo$$ is arbitrary, except for the fact that
the syntax
$codei%
	%algo%(%ax%, %ay%)
%$$ 
must evaluate the function $latex y = f(x)$$ using
$codei%AD<%Base%>%$$ operations.
In addition, we assume that the 
$cref/operation sequence/glossary/Operation/Sequence/$$
does not depend on the value of $icode ax$$.

$head afun$$
Given $icode ax$$ it computes the corresponding value of $icode ay$$
using the operation sequence corresponding to $icode algo$$. 
If $codei%AD<%Base%>%$$ operations are being recorded,
it enters the computation as single operation in the recording
see $cref/start recording/Independent/Start Recording/$$.
(Currently each use of $icode afun$$ actually corresponds to
$icode%m%+%n%+2%$$ operations and creates $icode m$$ new variables, 
but this is not part of the CppAD specifications and my change.)

$head clear$$
The $code atomic_base$$ class holds onto static work space in order to
increase speed by avoiding system memory allocation calls.
This call makes to work space $cref/available/ta_available/$$ to
for other uses by the same thread.
This should be called when you are done using the 
user atomic functions for a specific value of $icode Base$$.

$subhead Restriction$$
The $code clear$$ routine cannot be called
while in $cref/parallel/ta_in_parallel/$$ execution mode.

$children%
	example/atomic/checkpoint.cpp
%$$
$head Example$$
The file $cref checkpoint.cpp$$ contains an example and test
of these operations.
It returns true if it succeeds and false if it fails.

$end
*/
template <class Base>
class checkpoint : public atomic_base<Base> {
private:
	vector<ADFun<Base> > f_;
public:
	/*!
 	Constructor of a checkpoint object

	\param name [in]
	is the user's name for the AD version of this atomic operation.

	\param algo [in/out]
	user routine that compute AD function values
	(not const because state may change during evaluation).

	\param ax [in]
	argument value where algo operation sequence is taped.

	\param ay [out]
	function value at specified argument value.
	*/
	template <class Algo, class ADVector>
	checkpoint(const char* name, 
		Algo& algo, const ADVector& ax, ADVector& ay)
	: atomic_base<Base>(name)
	{	CheckSimpleVector< CppAD::AD<Base> , ADVector>();

#ifdef _OPENMP
#define NTHREADS omp_get_max_threads()
#define THREAD omp_get_thread_num()
#else
#define NTHREADS 1
#define THREAD 0
#endif
		f_.resize(NTHREADS);
		// make a copy of ax because Independent modifies AD information
		ADVector x_tmp(ax);
		// delcare x_tmp as the independent variables
	 	Independent(x_tmp);
		// record mapping from x_tmp to ay
		algo(x_tmp, ay); 
		// create function f_ : x -> y
		f_[0].Dependent(ay);
		// suppress checking for nan in f_ results
		// (see optimize documentation for atomic functions)
		f_[0].check_for_nan(false);
		// now optimize (we expect to use this function many times).
		f_[0].optimize();
		// Copy for other threads
		for(size_t i=1;i<NTHREADS;i++)f_[i]=f_[0];
		// now disable checking of comparison opertaions
		// 2DO: add a debugging mode that checks for changes and aborts
		f_[0].compare_change_count(0);
	}
	/*!
	Implement the user call to <tt>afun(ax, ay)</tt>.
	
	\tparam ADVector
	A simple vector class with elements of type <code>AD<Base></code>.
	
	\param id
	optional parameter which must be zero if present.
	
	\param ax
	is the argument vector for this call,
	<tt>ax.size()</tt> determines the number of arguments.
	
	\param ay
	is the result vector for this call,
	<tt>ay.size()</tt> determines the number of results.
	*/
	template <class ADVector>
	void operator()(const ADVector& ax, ADVector& ay, size_t id = 0)
	{	CPPAD_ASSERT_KNOWN(
			id == 0,
			"checkpoint: id is non-zero in afun(ax, ay, id)"
		);
		this->atomic_base<Base>::operator()(ax, ay, id);
	}
	/*!
 	Link from user_atomic to forward mode 

	\copydetails atomic_base::forward
 	*/
	virtual bool forward(
		size_t                    p ,
		size_t                    q ,
		const vector<bool>&      vx , 
		      vector<bool>&      vy , 
		const vector<Base>&      tx ,
		      vector<Base>&      ty )
	{
		CPPAD_ASSERT_UNKNOWN( f_[THREAD].size_var() > 0 );
		CPPAD_ASSERT_UNKNOWN( tx.size() % (q+1) == 0 );
		CPPAD_ASSERT_UNKNOWN( ty.size() % (q+1) == 0 );
		size_t n = tx.size() / (q+1);
		size_t m = ty.size() / (q+1);
		bool ok  = true;	
		size_t i, j;

		// 2DO: test both forward and reverse vy information
		if( vx.size() > 0 )
		{	//Compute Jacobian sparsity pattern.
			vector< std::set<size_t> > s(m);
			if( n <= m )
			{	vector< std::set<size_t> > r(n);
				for(j = 0; j < n; j++)
					r[j].insert(j);
				s = f_[THREAD].ForSparseJac(n, r);
			}
			else
			{	vector< std::set<size_t> > r(m);
				for(i = 0; i < m; i++)
					r[i].insert(i);
				s = f_[THREAD].RevSparseJac(m, r);
			}
			std::set<size_t>::const_iterator itr;
			for(i = 0; i < m; i++)
			{	vy[i] = false;
				for(itr = s[i].begin(); itr != s[i].end(); itr++)
				{	j = *itr;
					assert( j < n );
					// y[i] depends on the value of x[j]
					vy[i] |= vx[j];
				}
			}
		}
		ty = f_.Forward(q, tx);

		// no longer need the Taylor coefficients in f_
		// (have to reconstruct them every time)
		size_t c = 0;
		size_t r = 0;
		f_.capacity_order(c, r);
		return ok;
	}
	/*!
 	Link from user_atomic to reverse mode 

	\copydetails atomic_base::reverse
 	*/
	virtual bool reverse(
		size_t                    q  ,
		const vector<Base>&       tx ,
		const vector<Base>&       ty ,
		      vector<Base>&       px ,
		const vector<Base>&       py )
	{
		CPPAD_ASSERT_UNKNOWN( f_[THREAD].size_var() > 0 );
		CPPAD_ASSERT_UNKNOWN( tx.size() % (q+1) == 0 );
		CPPAD_ASSERT_UNKNOWN( ty.size() % (q+1) == 0 );
		bool ok  = true;	

		// put proper forward mode coefficients in f_
# ifdef NDEBUG
		f_[THREAD].Forward(q, tx);
# else
		size_t n = tx.size() / (q+1);
		size_t m = ty.size() / (q+1);
		CPPAD_ASSERT_UNKNOWN( px.size() == n * (q+1) );
		CPPAD_ASSERT_UNKNOWN( py.size() == m * (q+1) );
		size_t i, j, k;
		//
		vector<Base> check_ty = f_[THREAD].Forward(q, tx);
		for(i = 0; i < m; i++)
		{	for(k = 0; k <= q; k++)
			{	j = i * (q+1) + k;
				CPPAD_ASSERT_UNKNOWN( check_ty[j] == ty[j] );
			}
		}
# endif
		// now can run reverse mode
		px = f_[THREAD].Reverse(q+1, py);

		// no longer need the Taylor coefficients in f_
		// (have to reconstruct them every time)
		size_t c = 0;
		size_t r = 0;
		f_[THREAD].capacity_order(c, r);
		return ok;
	}
	/*!
 	Link from user_atomic to forward sparse Jacobian 

	\copydetails atomic_base::for_sparse_jac
 	*/
	virtual bool for_sparse_jac(
		size_t                                  q  ,
		const vector< std::set<size_t> >&       r  ,
		      vector< std::set<size_t> >&       s  )
	{
		bool ok = true;
		s = f_[THREAD].ForSparseJac(q, r);

		// no longer need the forward mode sparsity pattern
		// (have to reconstruct them every time)
		f_[THREAD].size_forward_set(0);
		
		return ok; 
	}
	/*!
 	Link from user_atomic to forward sparse Jacobian 

	\copydetails atomic_base::for_sparse_jac
 	*/
	virtual bool for_sparse_jac(
		size_t                                  q  ,
		const vector<bool>&                     r  ,
		      vector<bool>&                     s  )
	{
		bool ok = true;
		s = f_[THREAD].ForSparseJac(q, r);

		// no longer need the forward mode sparsity pattern
		// (have to reconstruct them every time)
		f_[THREAD].size_forward_bool(0);
		
		return ok; 
	}
	/*!
 	Link from user_atomic to forward sparse Jacobian 

	\copydetails atomic_base::rev_sparse_jac
 	*/
	virtual bool rev_sparse_jac(
		size_t                                  q  ,
		const vector< std::set<size_t> >&       rt ,
		      vector< std::set<size_t> >&       st )
	{
		bool ok  = true;

		// compute rt
		// 2DO: remove need for nz_compare all the time. It is only really
		// necessary when optimizer calls this member function.
		bool transpose = true;
		bool nz_compare = true;
		st = f_[THREAD].RevSparseJac(q, rt, transpose, nz_compare);

		return ok; 
	}
	/*!
 	Link from user_atomic to forward sparse Jacobian 

	\copydetails atomic_base::rev_sparse_jac
 	*/
	virtual bool rev_sparse_jac(
		size_t                                  q  ,
		const vector<bool>&                     rt ,
		      vector<bool>&                     st )
	{
		bool ok  = true;

		// compute rt
		bool transpose  = true;
		bool nz_compare = true;
		// 2DO: remove need for nz_compare all the time. It is only really
		// necessary when optimizer calls this member function.
		st = f_[THREAD].RevSparseJac(q, rt, transpose, nz_compare);

		return ok; 
	}
	/*!
 	Link from user_atomic to forward sparse Jacobian 

	\copydetails atomic_base::rev_sparse_hes
 	*/
	virtual bool rev_sparse_hes(
		const vector<bool>&                     vx ,
		const vector<bool>&                     s  ,
		      vector<bool>&                     t  ,
		size_t                                  q  ,
		const vector< std::set<size_t> >&       r  ,
		const vector< std::set<size_t> >&       u  ,
		      vector< std::set<size_t> >&       v  )
	{	size_t n       = v.size();
		size_t m       = u.size();
		CPPAD_ASSERT_UNKNOWN( r.size() == v.size() );
		CPPAD_ASSERT_UNKNOWN( s.size() == m );
		CPPAD_ASSERT_UNKNOWN( t.size() == n );
		bool ok        = true;
		bool transpose = true;
		std::set<size_t>::const_iterator itr;

		// compute sparsity pattern for T(x) = S(x) * f'(x)
		t = f_[THREAD].RevSparseJac(1, s);
# ifndef NDEBUG
		for(size_t j = 0; j < n; j++)
			CPPAD_ASSERT_UNKNOWN( vx[j] || ! t[j] )
# endif

		// V(x) = f'(x)^T * g''(y) * f'(x) * R  +  g'(y) * f''(x) * R 
		// U(x) = g''(y) * f'(x) * R
		// S(x) = g'(y)
		
		// compute sparsity pattern for A(x) = f'(x)^T * U(x)
		vector< std::set<size_t> > a(n);
		a = f_[THREAD].RevSparseJac(q, u, transpose);

		// set version of s
		vector< std::set<size_t> > set_s(1);
		CPPAD_ASSERT_UNKNOWN( set_s[0].empty() );
		size_t i;
		for(i = 0; i < m; i++)
			if( s[i] )
				set_s[0].insert(i);

		// compute sparsity pattern for H(x) = (S(x) * F)''(x) * R
		// (store it in v)
		f_[THREAD].ForSparseJac(q, r);
		v = f_[THREAD].RevSparseHes(q, set_s, transpose);

		// compute sparsity pattern for V(x) = A(x) + H(x)
		for(i = 0; i < n; i++)
		{	for(itr = a[i].begin(); itr != a[i].end(); itr++)
			{	size_t j = *itr;
				CPPAD_ASSERT_UNKNOWN( j < q );
				v[i].insert(j);
			}
		}

		// no longer need the forward mode sparsity pattern
		// (have to reconstruct them every time)
		f_[THREAD].size_forward_set(0);

		return ok;
	}
	/*!
 	Link from user_atomic to forward sparse Jacobian 

	\copydetails atomic_base::rev_sparse_hes
 	*/
	virtual bool rev_sparse_hes(
		const vector<bool>&                     vx ,
		const vector<bool>&                     s  ,
		      vector<bool>&                     t  ,
		size_t                                  q  ,
		const vector<bool>&                     r  ,
		const vector<bool>&                     u  ,
		      vector<bool>&                     v  )
	{
		CPPAD_ASSERT_UNKNOWN( r.size() == v.size() );
		CPPAD_ASSERT_UNKNOWN( s.size() == u.size() / q );
		CPPAD_ASSERT_UNKNOWN( t.size() == v.size() / q );
		size_t n       = t.size();
		bool ok        = true;
		bool transpose = true;
		std::set<size_t>::const_iterator itr;
		size_t i, j;

		// compute sparsity pattern for T(x) = S(x) * f'(x)
		t = f_[THREAD].RevSparseJac(1, s);
# ifndef NDEBUG
		for(j = 0; j < n; j++)
			CPPAD_ASSERT_UNKNOWN( vx[j] || ! t[j] )
# endif

		// V(x) = f'(x)^T * g''(y) * f'(x) * R  +  g'(y) * f''(x) * R 
		// U(x) = g''(y) * f'(x) * R
		// S(x) = g'(y)

		// compute sparsity pattern for A(x) = f'(x)^T * U(x)
		vector<bool> a(n * q);
		a = f_[THREAD].RevSparseJac(q, u, transpose);

		// compute sparsity pattern for H(x) =(S(x) * F)''(x) * R
		// (store it in v)
		f_[THREAD].ForSparseJac(q, r);
		v = f_[THREAD].RevSparseHes(q, s, transpose);

		// compute sparsity pattern for V(x) = A(x) + H(x)
		for(i = 0; i < n; i++)
		{	for(j = 0; j < q; j++)
				v[ i * q + j ] |= a[ i * q + j];
		}

		// no longer need the forward mode sparsity pattern
		// (have to reconstruct them every time)
		f_[THREAD].size_forward_set(0);

		return ok;
	}
};

} // END_CPPAD_NAMESPACE
# endif

#undef NTHREADS
#undef THREAD
