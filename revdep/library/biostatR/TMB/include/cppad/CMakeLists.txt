# $Id:$
# -----------------------------------------------------------------------------
# CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-15 Bradley M. Bell
#
# CppAD is distributed under multiple licenses. This distribution is under
# the terms of the
#                     GNU General Public License Version 3.
#
# A copy of this license is included in the COPYING file of this distribution.
# Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
# -----------------------------------------------------------------------------
# Build the cppad/configure.hpp file.
# Inherit environment from ../CMakeLists.txt
# -----------------------------------------------------------------------------
MACRO(check_match match_variable match_constant output_variable)
	STRING(COMPARE EQUAL ${${match_variable}} ${match_constant} match_flag )
	IF( match_flag )
		SET(${output_variable} 1)
	ELSE( match_flag )
		SET(${output_variable} 0)
	ENDIF( match_flag )
	MESSAGE(STATUS "${output_variable} = ${${output_variable}}" )
ENDMACRO(check_match)
# -----------------------------------------------------------------------------
# command line arguments
#
# cppad_testvector
command_line_arg(cppad_testvector cppad STRING
"Namespace of vector used for testing, one of: boost, cppad, eigen, std"
)
#
# cppad_max_num_threads
command_line_arg(cppad_max_num_threads 48 STRING
	"maximum number of threads that CppAD can use use"
)
# cppad_sparse_list
command_line_arg(cppad_sparse_list YES BOOL
	"use sparse list for internal sparse set representation"
)
#
# cppad_tape_id_type
command_line_arg(cppad_tape_id_type "unsigned int" STRING
	"type used to identify different tapes, size must be <= sizeof(size_t)"
)
#
# cppad_tape_addr_type
command_line_arg(cppad_tape_addr_type "unsigned int" STRING
"type used to identify variables on one tape, size must be <= sizeof(size_t)"
)
#
# cppad_implicit_ctor_from_any_type
command_line_arg(cppad_implicit_ctor_from_any_type NO BOOL
	"implicit (instead of explicit) conversion from any type to AD<Base>"
)
# -----------------------------------------------------------------------------
# cppad_explicit_ctor
IF( cppad_implicit_ctor_from_any_type )
	SET(cppad_implicit_ctor_from_any_type 1)
ELSE( cppad_implicit_ctor_from_any_type )
	SET(cppad_implicit_ctor_from_any_type 0)
ENDIF( cppad_implicit_ctor_from_any_type )
# -----------------------------------------------------------------------------
# cppad_internal_sparse_set
#
IF( cppad_sparse_list )
	SET(cppad_internal_sparse_set  sparse_list )
ELSE( cppad_sparse_list )
	SET(cppad_internal_sparse_set  sparse_set )
ENDIF( cppad_sparse_list )
MESSAGE(STATUS "cppad_internal_sparse_set = ${cppad_internal_sparse_set}" )
#
# -----------------------------------------------------------------------------
# cppad_boostvector, cppad_cppadvector, cppad_eigenvector, cppad_stdvector
#
check_match(cppad_testvector boost cppad_boostvector)
check_match(cppad_testvector cppad cppad_cppadvector)
check_match(cppad_testvector eigen cppad_eigenvector)
check_match(cppad_testvector std   cppad_stdvector)
IF( NOT cppad_boostvector )
IF( NOT cppad_cppadvector )
IF( NOT cppad_eigenvector )
IF( NOT cppad_stdvector )
MESSAGE(FATAL_ERROR
"cppad_testvector not one of following: boost, cppad, eigen, std."
)
ENDIF( NOT cppad_stdvector )
ENDIF( NOT cppad_eigenvector )
ENDIF( NOT cppad_cppadvector )
ENDIF( NOT cppad_boostvector )

IF( cppad_boostvector )
	# FIND_PACKAGE(Boost) done by ../CMakeLists.txt
	IF( NOT Boost_FOUND )
		MESSAGE(FATAL_ERROR
"cppad_testvector == boost but cannot find boost include files"
		)
	ENDIF( NOT Boost_FOUND )
ENDIF( cppad_boostvector )
#
IF( cppad_eigenvector )
	IF( NOT eigen_prefix )
		MESSAGE(FATAL_ERROR
"cppad_testvector == eigen but eigen_prefix is not specified"
		)
	ENDIF( NOT eigen_prefix )
ENDIF( cppad_eigenvector )

# =============================================================================
# Begin C++11 features
# -----------------------------------------------------------------------------
# cppad_has_rvalue
#
SET(source "
int main(void)
{	int&& value = 1 + 2;
	if( value != 3 )
		return 1;
	return 0;
}"
)
check_source_runs("${source}" cppad_has_rvalue)
# -----------------------------------------------------------------------------
# cppad_has_nullptr
#
SET(source "
int main(void)
{	char *c = nullptr;
	return 0;
}"
)
check_source_runs("${source}" cppad_has_nullptr)
# -----------------------------------------------------------------------------
# cppad_has_cstdint_8_to_64
#
SET(source "
# include <cstdint>
template <class T> inline bool is_pod(void)               { return false; }
template <>        inline bool is_pod<uint8_t>(void)      { return true;  }
template <>        inline bool is_pod<uint16_t>(void)     { return true;  }
template <>        inline bool is_pod<uint32_t>(void)     { return true;  }
template <>        inline bool is_pod<uint64_t>(void)     { return true;  }
int main(void)
{	return 0; }
"
)
check_source_runs("${source}" cppad_has_cstdint_8_to_64)
# -----------------------------------------------------------------------------
# cppad_compiler_has_erf
#
SET(source "
# include <cmath>
int main(void)
{	if( std::erf(0.0) == 0.0 )
		return 0;
	return 1;
}
"
)
check_source_runs("${source}" cppad_compiler_has_erf)
# -----------------------------------------------------------------------------
# cppad_has_high_resolution_clock
#
SET(source "
# include <chrono>
int main(void)
{	std::chrono::time_point<std::chrono::high_resolution_clock> start, end;
    start = std::chrono::high_resolution_clock::now();
    end   = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> difference = end - start;
	double elapsed_seconds = difference.count();
	if( elapsed_seconds > 0.0 )
		return 0;
	return 1;
}
"
)
check_source_runs("${source}" cppad_has_high_resolution_clock)
# =============================================================================
# End C++11 features
# -----------------------------------------------------------------------------
# cppad_has_gettimeofday
#
SET(source "
# include<sys/time.h>
int main(void)
{	struct timeval time;
	gettimeofday(&time, 0);
	return 0;
}"
)
check_source_runs("${source}" cppad_has_gettimeofday)
# -----------------------------------------------------------------------------
# cppad_size_t_not_unsigned_int
#
SET(source "
# include <cstring>
template <class T> inline bool is_pod(void)               { return false; }
template <>        inline bool is_pod<unsigned int>(void) { return true;  }
template <>        inline bool is_pod<size_t>(void)       { return true;  }
int main(void)
{	return 0; }
"
)
check_source_runs("${source}" cppad_size_t_not_unsigned_int)
# -----------------------------------------------------------------------------
# cppad_tape_addr_type, cppad_tape_id_type
#
FOREACH(cmake_var cppad_tape_id_type cppad_tape_addr_type )
	SET(source "
# include <limits>
int main(void)
{	bool is_unsigned = ! std::numeric_limits<${${cmake_var}}>::is_signed;
	return int(! is_unsigned);
}
"
	)
	check_source_runs("${source}" ${cmake_var}_is_unsigned)
	IF( ${cmake_var}_is_unsigned STREQUAL 0  )
		MESSAGE(STATUS
"Warning: using a signed ${cmake_var} is for CppAD developers only !"
	)
	ENDIF( ${cmake_var}_is_unsigned STREQUAL 0  )
ENDFOREACH( cmake_var )
# -----------------------------------------------------------------------------
# cppad_max_num_threads
#
SET(CMAKE_REQUIRED_INCLUDES  "")
SET(CMAKE_REQUIRED_LIBRARIES "")
SET(CMAKE_REQUIRED_FLAGS     )
SET(source "
int main(void)
{	const char* number = \"${cppad_max_num_threads}\";
	int value = 0;
	while( *number == ' ' )
		number++;
	while( '0' <= *number && *number <= '9' )
	{	value = 10 * value + (int)(*number - '0');
		number++;
	}
	while( *number == ' ' )
		number++;
	if( *number != char(0) )
		return 1;
	if( value < 4 )
		return 1;
	return 0;
}
" )
CHECK_CXX_SOURCE_RUNS("${source}" cppad_max_num_threads_is_integer_ge_4)
IF( NOT cppad_max_num_threads_is_integer_ge_4 )
	MESSAGE(FATAL_ERROR
	"cppad_max_num_threads is not an integer greater than or equal 4"
	)
ENDIF( NOT cppad_max_num_threads_is_integer_ge_4 )

# -----------------------------------------------------------------------------
# Copy a file to another location and modify its contents.
# configure_file(InputFile OutputFile [COPYONLY] [ESCAPE_QUOTES] [@ONLY])
CONFIGURE_FILE(
	${CMAKE_CURRENT_SOURCE_DIR}/configure.hpp.in
	${CMAKE_CURRENT_SOURCE_DIR}/configure.hpp
)
