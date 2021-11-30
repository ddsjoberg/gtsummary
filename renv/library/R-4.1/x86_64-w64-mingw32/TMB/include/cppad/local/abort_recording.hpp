/* $Id$ */
# ifndef CPPAD_ABORT_RECORDING_INCLUDED
# define CPPAD_ABORT_RECORDING_INCLUDED

/* --------------------------------------------------------------------------
CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-12 Bradley M. Bell

CppAD is distributed under multiple licenses. This distribution is under
the terms of the 
                    GNU General Public License Version 3.

A copy of this license is included in the COPYING file of this distribution.
Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
-------------------------------------------------------------------------- */
/*
$begin abort_recording$$
$spell 
$$

$section Abort Recording of an Operation Sequence$$

$index abort, operation sequence$$
$index operation, sequence abort$$
$index sequence, operation abort$$
$index recording, abort$$
$index tape, abort recording$$

$head Syntax$$
$codei%AD<%Base%>::abort_recording()%$$

$head Purpose$$
Sometimes it is necessary to abort the recording of an operation sequence
that started with a call of the form
$codei%
	Independent(%x%)
%$$
If such a recording is currently in progress,
$code abort_recording$$ will stop the recording and delete the
corresponding information.
Otherwise, $code abort_recording$$ has no effect.

$children%
	example/abort_recording.cpp
%$$
$head Example$$
The file
$cref abort_recording.cpp$$ 
contains an example and test of this operation.
It returns true if it succeeds and false otherwise.

$end
----------------------------------------------------------------------------
*/


namespace CppAD {
	template <typename Base>
	void AD<Base>::abort_recording(void)
	{	ADTape<Base>* tape = AD<Base>::tape_ptr();
		if( tape != CPPAD_NULL )
			AD<Base>::tape_manage(tape_manage_delete);
	}
}

# endif
