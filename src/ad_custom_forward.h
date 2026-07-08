#pragma once
#include <Rcpp.h>
#include <string>

typedef void (*ad_custom_forward_replay_fn)(SEXP node);

namespace ad_custom_fwd {

void register_replay(const char* op, ad_custom_forward_replay_fn fn);
bool dispatch(const std::string& op, SEXP node);

}  // namespace ad_custom_fwd

extern "C" void LibeRtAD_register_custom_forward_replay(const char* op,
                                                      ad_custom_forward_replay_fn fn);
