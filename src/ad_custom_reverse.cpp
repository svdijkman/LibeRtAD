#include "ad_custom_reverse.h"
#include <R.h>

namespace ad_custom {

static std::unordered_map<std::string, ad_custom_reverse_fn>& registry() {
  static std::unordered_map<std::string, ad_custom_reverse_fn> g;
  return g;
}

void register_reverse(const char* op, ad_custom_reverse_fn fn) {
  registry()[op] = fn;
}

bool dispatch(const std::string& op, SEXP node, SEXP grad) {
  auto& g = registry();
  auto it = g.find(op);
  if (it == g.end()) {
    return false;
  }
  it->second(node, grad);
  return true;
}

}  // namespace ad_custom

void ad_add_grad_sidecar(Rcpp::Environment node, SEXP increment);

extern "C" void LibeRtAD_register_custom_reverse(const char* op, ad_custom_reverse_fn fn) {
  ad_custom::register_reverse(op, fn);
}

extern "C" void LibeRtAD_add_grad_sidecar(SEXP node, SEXP increment) {
  ad_add_grad_sidecar(Rcpp::Environment(node), increment);
}

// [[Rcpp::init]]
void ad_init_c_callables(DllInfo* dll) {
  R_RegisterCCallable("LibeRtAD", "register_custom_reverse",
                      (DL_FUNC) LibeRtAD_register_custom_reverse);
  R_RegisterCCallable("LibeRtAD", "add_grad_sidecar",
                      (DL_FUNC) LibeRtAD_add_grad_sidecar);
}
