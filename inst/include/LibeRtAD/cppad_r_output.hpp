#ifndef LIBERTAD_CPPAD_R_OUTPUT_HPP
#define LIBERTAD_CPPAD_R_OUTPUT_HPP

#include <Rcpp.h>

// CppAD contains diagnostic and error branches that write directly to
// std::cout. R extensions must write through R's console API instead. Keep the
// redirection local to the CppAD include so it cannot alter client code.
//
// CppAD spells the stream both as `std::cout` and as an unqualified `cout`
// after `using std::cout`. Replacing the identifier while parsing the headers
// therefore requires the replacement stream to be a member of `std`.
namespace std {
inline Rcpp::Rostream<true> cppad_r_output;
}

#define cout cppad_r_output
#include <cppad/cppad.hpp>
#undef cout

#endif
