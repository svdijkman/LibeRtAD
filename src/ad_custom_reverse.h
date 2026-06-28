#pragma once
#include <Rcpp.h>
#include <string>
#include <unordered_map>

typedef void (*ad_custom_reverse_fn)(SEXP node, SEXP grad);

namespace ad_custom {

void register_reverse(const char* op, ad_custom_reverse_fn fn);
bool dispatch(const std::string& op, SEXP node, SEXP grad);

}  // namespace ad_custom

extern "C" void LibeRtAD_register_custom_reverse(const char* op, ad_custom_reverse_fn fn);
