#include "ad_custom_forward.h"
#include <R.h>
#include <unordered_map>

namespace ad_custom_fwd {

static std::unordered_map<std::string, ad_custom_forward_replay_fn>& registry() {
  static std::unordered_map<std::string, ad_custom_forward_replay_fn> g;
  return g;
}

void register_replay(const char* op, ad_custom_forward_replay_fn fn) {
  registry()[op] = fn;
}

bool dispatch(const std::string& op, SEXP node) {
  auto& g = registry();
  auto it = g.find(op);
  if (it == g.end()) {
    return false;
  }
  it->second(node);
  return true;
}

}  // namespace ad_custom_fwd

extern "C" void LibeRtAD_register_custom_forward_replay(
    const char* op, ad_custom_forward_replay_fn fn) {
  ad_custom_fwd::register_replay(op, fn);
}

// [[Rcpp::init]]
void ad_forward_init_c_callables(DllInfo* dll) {
  R_RegisterCCallable("LibeRtAD", "register_custom_forward_replay",
                      (DL_FUNC) LibeRtAD_register_custom_forward_replay);
}
