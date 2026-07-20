#ifndef LIBERTAD_CPPAD_R_OUTPUT_HPP
#define LIBERTAD_CPPAD_R_OUTPUT_HPP

#include <Rcpp.h>

// LibeRtAD bundles the official CppAD release 20260000.0 at commit
// 5d51b2aa6d6874c8d561da298a90b3721550d45d. CppAD contains
// diagnostic and error branches that write directly to std::cout/std::cerr,
// and its last-resort default error handler calls std::exit. R extensions must
// use R's console and exception machinery instead. Keep the redirection local
// to the CppAD include so it cannot alter client code or the official headers.
//
// CppAD spells the streams both as qualified names and as unqualified names
// after `using`. Replacing each identifier while parsing the headers therefore
// requires the replacement to be a member of `std`.
namespace std {
inline Rcpp::Rostream<true> cppad_r_output;
inline Rcpp::Rostream<false> cppad_r_error;
[[noreturn]] inline void cppad_r_exit(int) {
  throw Rcpp::exception(
    "CppAD requested process termination; execution was returned safely to R.",
    false
  );
}
}

#define cout cppad_r_output
#define cerr cppad_r_error
#define exit cppad_r_exit
#include <cppad/cppad.hpp>
#undef exit
#undef cerr
#undef cout

namespace libertad {
inline constexpr const char* cppad_source_commit =
  "5d51b2aa6d6874c8d561da298a90b3721550d45d";
}

#endif
