#include <Rcpp.h>
#include <unordered_map>
#include <functional>
#include <cmath>
#include <limits>
#include "ad_custom_reverse.h"
#include "ad_custom_forward.h"
using namespace Rcpp;

int g_ad_node_seq = 0;

std::unordered_map<int, NumericVector> g_grad_sidecar;
bool g_use_grad_sidecar = false;
std::unordered_map<std::string, Environment> g_const_pool;
std::string g_last_reverse_op;

void clear_const_pool_cpp() {
  g_const_pool.clear();
}

bool is_constant_node(SEXP myvar) {
  return Rf_inherits(myvar, "Constant");
}

void ad_add_grad_sidecar(Environment node, SEXP increment) {
  if (is_constant_node(node)) {
    return;
  }
  int id = as<int>(node["node_id"]);
  NumericVector inc = as<NumericVector>(increment);
  R_xlen_t n = as<NumericVector>(Environment(node)["value"]).size();
  if (n > 1 && inc.size() == 1) {
    NumericVector expanded(n);
    std::fill(expanded.begin(), expanded.end(), inc[0]);
    inc = expanded;
  }
  auto found = g_grad_sidecar.find(id);
  if (found == g_grad_sidecar.end()) {
    g_grad_sidecar[id] = clone(inc);
  } else {
    NumericVector acc = clone(found->second);
    if (acc.size() != inc.size()) {
      R_xlen_t m = std::max(acc.size(), inc.size());
      NumericVector tmp(m);
      for (R_xlen_t i = 0; i < m; ++i) {
        tmp[i] = (i < acc.size() ? acc[i] : 0.0) + (i < inc.size() ? inc[i] : 0.0);
      }
      g_grad_sidecar[id] = tmp;
    } else {
      for (R_xlen_t i = 0; i < inc.size(); ++i) {
        acc[i] += inc[i];
      }
      g_grad_sidecar[id] = acc;
    }
  }
}

NumericVector get_effective_grad(Environment node) {
  NumericVector base = as<NumericVector>(Environment(node)["grad"]);
  if (!g_use_grad_sidecar || is_constant_node(node)) {
    return base;
  }
  int id = as<int>(node["node_id"]);
  auto found = g_grad_sidecar.find(id);
  if (found == g_grad_sidecar.end()) {
    return base;
  }
  NumericVector extra = found->second;
  if (extra.size() == base.size()) {
    NumericVector out = clone(base);
    for (R_xlen_t i = 0; i < out.size(); ++i) {
      out[i] += extra[i];
    }
    return out;
  }
  if (extra.size() == 1 && base.size() > 1) {
    NumericVector out = clone(base);
    for (R_xlen_t i = 0; i < out.size(); ++i) {
      out[i] += extra[0];
    }
    return out;
  }
  if (base.size() == 1 && extra.size() > 1) {
    NumericVector out = clone(extra);
    for (R_xlen_t i = 0; i < out.size(); ++i) {
      out[i] += base[0];
    }
    return out;
  }
  if (base.size() == 1 && extra.size() == 1) {
    return NumericVector::create(base[0] + extra[0]);
  }
  return extra;
}

std::string next_ad_node_name() {
  return std::string("adn") + std::to_string(++g_ad_node_seq);
}

namespace {

bool is_constant_cpp(SEXP myvar) {
  return Rf_inherits(myvar, "Constant");
}

bool is_variable_cpp(SEXP myvar) {
  return Rf_inherits(myvar, "Variable");
}

std::string getvarname(Environment myvar) {
  return as<std::string>(myvar["name"]);
}

SEXP get_node_value(SEXP node) {
  return Environment(node)["value"];
}

SEXP get_node_grad(SEXP node) {
  return Environment(node)["grad"];
}

SEXP get_node_tangent(SEXP node) {
  return Environment(node)["tangent"];
}

NumericVector as_numeric(SEXP x) {
  return as<NumericVector>(x);
}

SEXP preserve_dims(NumericVector v, SEXP template_x) {
  if (!Rf_isNull(Rf_getAttrib(template_x, R_DimSymbol))) {
    v.attr("dim") = Rf_getAttrib(template_x, R_DimSymbol);
  }
  return v;
}

SEXP preserve_dims_binary(NumericVector v, SEXP a, SEXP b) {
  if (!Rf_isNull(Rf_getAttrib(a, R_DimSymbol))) {
    return preserve_dims(v, a);
  }
  if (!Rf_isNull(Rf_getAttrib(b, R_DimSymbol))) {
    return preserve_dims(v, b);
  }
  return v;
}

NumericVector zeros_like(SEXP x) {
  NumericVector z(as_numeric(x).size());
  std::fill(z.begin(), z.end(), 0.0);
  return as_numeric(preserve_dims(z, x));
}

Environment pkg_env() {
  return Environment::namespace_env("LibeRtAD");
}

Environment newconst_sexp(SEXP val) {
  Environment myconst = pkg_env()["Constant"];
  Function new_const = myconst["new"];
  return new_const(
    Named("name") = "const",
    Named("value") = val,
    Named("grad") = zeros_like(val)
  );
}

Environment newvar_sexp(std::string myname, SEXP myval, std::string myop,
                        List myparents, SEXP meta = R_NilValue) {
  Environment Var = pkg_env()["Variable"];
  Function new_var = Var["new"];
  Environment res = new_var(
    Named("name") = myname,
    Named("value") = myval,
    Named("grad") = zeros_like(myval),
    Named("op") = myop,
    Named("parents") = myparents
  );
  if (meta != R_NilValue) {
    res["meta"] = meta;
  }
  return res;
}

struct Operand {
  Environment var;
  std::string name;
  SEXP value;
};

Operand as_operand(SEXP x) {
  Operand out;
  if (is_variable_cpp(x) || is_constant_cpp(x)) {
    out.var = x;
    out.name = getvarname(x);
    out.value = get_node_value(x);
  } else {
    NumericVector v = as<NumericVector>(x);
    std::string key = std::to_string(v.size()) + ":" + std::to_string(v[0]);
    if (v.size() > 1) key += ":" + std::to_string(v[v.size() - 1]);
    auto found = g_const_pool.find(key);
    if (found != g_const_pool.end()) {
      out.var = found->second;
    } else {
      out.var = newconst_sexp(x);
      g_const_pool[key] = out.var;
    }
    out.name = "const";
    out.value = get_node_value(out.var);
  }
  return out;
}

bool is_unary_op(const std::string& op) {
  return op == "neg" || op == "sin" || op == "cos" || op == "exp" || op == "log" ||
    op == "abs" || op == "sqrt";
}

NumericVector ad_sign_vec(NumericVector x) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    if (x[i] > 0) out[i] = 1.0;
    else if (x[i] < 0) out[i] = -1.0;
    else out[i] = 0.0;
  }
  return out;
}

NumericVector vec_pow(NumericVector base, NumericVector exp) {
  NumericVector out(base.size());
  for (R_xlen_t i = 0; i < base.size(); ++i) {
    out[i] = std::pow(base[i], exp[i % exp.size()]);
  }
  return out;
}

NumericVector align_grad(NumericVector gv, R_xlen_t n) {
  if (gv.size() == n) {
    return gv;
  }
  if (gv.size() == 1) {
    NumericVector out(n);
    std::fill(out.begin(), out.end(), gv[0]);
    return out;
  }
  stop("Gradient size mismatch in op=" + g_last_reverse_op + " (grad len=" +
       std::to_string(gv.size()) + ", expected=" + std::to_string(n) + ")");
}

NumericVector align_tangent(NumericVector t, R_xlen_t n) {
  if (t.size() == n) {
    return t;
  }
  if (t.size() == 1) {
    NumericVector out(n);
    std::fill(out.begin(), out.end(), t[0]);
    return out;
  }
  if (n == 1) {
    double s = 0.0;
    for (R_xlen_t i = 0; i < t.size(); ++i) s += t[i];
    return NumericVector::create(s);
  }
  stop("Tangent size mismatch");
}

SEXP matmul_numeric(SEXP a, SEXP b) {
  if (Rf_isNull(Rf_getAttrib(a, R_DimSymbol)) || Rf_isNull(Rf_getAttrib(b, R_DimSymbol))) {
    stop("Matrix operands required for %*%");
  }
  NumericMatrix A = as<NumericMatrix>(a);
  NumericMatrix B = as<NumericMatrix>(b);
  const int n = A.nrow();
  const int k = A.ncol();
  const int m = B.ncol();
  NumericMatrix C(n, m);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      double s = 0.0;
      for (int t = 0; t < k; ++t) {
        s += A(i, t) * B(t, j);
      }
      C(i, j) = s;
    }
  }
  return C;
}

SEXP matmul_sexp(SEXP a, SEXP b) {
  return matmul_numeric(a, b);
}

SEXP transpose_sexp(SEXP a) {
  Environment base = Environment::base_env();
  Function t_fn = base["t"];
  return t_fn(a);
}

void add_grad(Environment node, SEXP increment) {
  if (g_use_grad_sidecar) {
    ad_add_grad_sidecar(node, increment);
    return;
  }
  Function setGrad = node["setGrad"];
  setGrad(increment);
}

int which_max_r(NumericVector x) {
  if (x.size() == 0) {
    return 1;
  }
  R_xlen_t idx = 0;
  double best = x[0];
  for (R_xlen_t i = 1; i < x.size(); ++i) {
    if (x[i] > best) {
      best = x[i];
      idx = i;
    }
  }
  return static_cast<int>(idx + 1);
}

}  // namespace

// [[Rcpp::export(name = "reset_ad_node_seq_cpp")]]
void reset_ad_node_seq_export() {
  g_ad_node_seq = 0;
}

// [[Rcpp::export(name = "is_constant_cpp")]]
bool is_constant_export(SEXP myvar) {
  return is_constant_cpp(myvar);
}

// [[Rcpp::export(name = "is_variable_cpp")]]
bool is_variable_export(SEXP myvar) {
  return is_variable_cpp(myvar);
}

// [[Rcpp::export]]
Environment add_var(SEXP e1, SEXP e2) {
  Operand left = as_operand(e1);
  Operand right = as_operand(e2);
  NumericVector out = as_numeric(left.value) + as_numeric(right.value);
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims_binary(out, left.value, right.value),
    "+",
    List::create(left.var, right.var)
  );
}

// [[Rcpp::export]]
Environment neg_var(SEXP e1) {
  Operand op = as_operand(e1);
  NumericVector out = -as_numeric(op.value);
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims(out, op.value),
    "neg",
    List::create(op.var)
  );
}

// [[Rcpp::export]]
Environment sub_var(SEXP e1, SEXP e2) {
  Operand left = as_operand(e1);
  Operand right = as_operand(e2);
  NumericVector out = as_numeric(left.value) - as_numeric(right.value);
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims_binary(out, left.value, right.value),
    "-",
    List::create(left.var, right.var)
  );
}

// [[Rcpp::export]]
Environment mul_var(SEXP e1, SEXP e2) {
  Operand left = as_operand(e1);
  Operand right = as_operand(e2);
  NumericVector out = as_numeric(left.value) * as_numeric(right.value);
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims_binary(out, left.value, right.value),
    "*",
    List::create(left.var, right.var)
  );
}

// [[Rcpp::export]]
Environment div_var(SEXP e1, SEXP e2) {
  Operand left = as_operand(e1);
  Operand right = as_operand(e2);
  NumericVector out = as_numeric(left.value) / as_numeric(right.value);
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims_binary(out, left.value, right.value),
    "/",
    List::create(left.var, right.var)
  );
}

// [[Rcpp::export]]
Environment pow_var(SEXP e1, SEXP e2) {
  Operand left = as_operand(e1);
  Operand right = as_operand(e2);
  NumericVector lv = as_numeric(left.value);
  NumericVector rv = as_numeric(right.value);
  NumericVector out = vec_pow(lv, rv);
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims_binary(out, left.value, right.value),
    "^",
    List::create(left.var, right.var)
  );
}

// [[Rcpp::export]]
Environment sin_var(SEXP e1) {
  Operand operand = as_operand(e1);
  NumericVector out = sin(as_numeric(operand.value));
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims(out, operand.value),
    "sin",
    List::create(operand.var)
  );
}

// [[Rcpp::export]]
Environment cos_var(SEXP e1) {
  Operand operand = as_operand(e1);
  NumericVector out = cos(as_numeric(operand.value));
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims(out, operand.value),
    "cos",
    List::create(operand.var)
  );
}

// [[Rcpp::export]]
Environment exp_var(SEXP e1) {
  Operand operand = as_operand(e1);
  NumericVector out = exp(as_numeric(operand.value));
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims(out, operand.value),
    "exp",
    List::create(operand.var)
  );
}

// [[Rcpp::export]]
Environment log_var(SEXP e1) {
  Operand operand = as_operand(e1);
  NumericVector vals = as_numeric(operand.value);
  for (R_xlen_t i = 0; i < vals.size(); ++i) {
    if (vals[i] <= 0) stop("Value must be positive for log function");
  }
  NumericVector out = log(vals);
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims(out, operand.value),
    "log",
    List::create(operand.var)
  );
}

// [[Rcpp::export]]
Environment abs_var(SEXP e1) {
  Operand operand = as_operand(e1);
  NumericVector out = abs(as_numeric(operand.value));
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims(out, operand.value),
    "abs",
    List::create(operand.var)
  );
}

// [[Rcpp::export]]
Environment sqrt_var(SEXP e1) {
  Operand operand = as_operand(e1);
  NumericVector vals = as_numeric(operand.value);
  for (R_xlen_t i = 0; i < vals.size(); ++i) {
    if (vals[i] < 0) stop("Value must be non-negative for sqrt function");
  }
  NumericVector out = sqrt(vals);
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims(out, operand.value),
    "sqrt",
    List::create(operand.var)
  );
}

// [[Rcpp::export]]
Environment pmax_var(SEXP e1, SEXP e2) {
  Operand left = as_operand(e1);
  Operand right = as_operand(e2);
  NumericVector lv = as_numeric(left.value);
  NumericVector rv = as_numeric(right.value);
  NumericVector out(lv.size());
  IntegerVector branch(lv.size());
  for (R_xlen_t i = 0; i < lv.size(); ++i) {
    double rvi = rv[i % rv.size()];
    if (lv[i] >= rvi) {
      out[i] = lv[i];
      branch[i] = 1;
    } else {
      out[i] = rvi;
      branch[i] = 2;
    }
  }
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims_binary(out, left.value, right.value),
    "pmax",
    List::create(left.var, right.var),
    List::create(Named("branch") = branch)
  );
}

// [[Rcpp::export]]
Environment pmin_var(SEXP e1, SEXP e2) {
  Operand left = as_operand(e1);
  Operand right = as_operand(e2);
  NumericVector lv = as_numeric(left.value);
  NumericVector rv = as_numeric(right.value);
  NumericVector out(lv.size());
  IntegerVector branch(lv.size());
  for (R_xlen_t i = 0; i < lv.size(); ++i) {
    double rvi = rv[i % rv.size()];
    if (lv[i] <= rvi) {
      out[i] = lv[i];
      branch[i] = 1;
    } else {
      out[i] = rvi;
      branch[i] = 2;
    }
  }
  return newvar_sexp(
    next_ad_node_name(),
    preserve_dims_binary(out, left.value, right.value),
    "pmin",
    List::create(left.var, right.var),
    List::create(Named("branch") = branch)
  );
}

// [[Rcpp::export]]
Environment sum_var(SEXP e1) {
  Operand operand = as_operand(e1);
  NumericVector vals = as_numeric(operand.value);
  double s = 0.0;
  for (R_xlen_t i = 0; i < vals.size(); ++i) s += vals[i];
  return newvar_sexp(
    next_ad_node_name(),
    NumericVector::create(s),
    "sum",
    List::create(operand.var)
  );
}

// [[Rcpp::export]]
Environment mean_var(SEXP e1) {
  Operand operand = as_operand(e1);
  NumericVector vals = as_numeric(operand.value);
  double s = 0.0;
  for (R_xlen_t i = 0; i < vals.size(); ++i) s += vals[i];
  double m = vals.size() > 0 ? s / static_cast<double>(vals.size()) : NA_REAL;
  return newvar_sexp(
    next_ad_node_name(),
    NumericVector::create(m),
    "mean",
    List::create(operand.var)
  );
}

// [[Rcpp::export]]
Environment max_var(SEXP e1) {
  Operand operand = as_operand(e1);
  NumericVector vals = as_numeric(operand.value);
  int idx = which_max_r(vals);
  return newvar_sexp(
    next_ad_node_name(),
    NumericVector::create(vals[idx - 1]),
    "max",
    List::create(operand.var),
    List::create(Named("index") = idx)
  );
}

// [[Rcpp::export]]
Environment subset_var(SEXP x, int idx) {
  if (idx < 1) stop("Index must be >= 1");
  Operand operand = as_operand(x);
  NumericVector vals = as_numeric(operand.value);
  if (idx > vals.size()) stop("Index out of bounds");
  NumericVector out = NumericVector::create(vals[idx - 1]);
  return newvar_sexp(
    next_ad_node_name(),
    out,
    "[",
    List::create(operand.var),
    List::create(Named("index") = idx)
  );
}

// [[Rcpp::export]]
Environment matmul_var(SEXP e1, SEXP e2) {
  Operand left = as_operand(e1);
  Operand right = as_operand(e2);
  SEXP result = matmul_numeric(left.value, right.value);
  return newvar_sexp(
    next_ad_node_name(),
    result,
    "%*%",
    List::create(left.var, right.var)
  );
}

// [[Rcpp::export]]
void reset_grad_sidecar_cpp() {
  g_grad_sidecar.clear();
}

// [[Rcpp::export]]
void clear_const_pool_export() {
  clear_const_pool_cpp();
}

// [[Rcpp::export]]
void apply_grad_sidecar_cpp(List tape) {
  for (int i = 0; i < tape.size(); ++i) {
    Environment node = tape[i];
    if (is_constant_node(node)) {
      continue;
    }
    NumericVector g = get_effective_grad(node);
    node["grad"] = g;
  }
  g_grad_sidecar.clear();
}

// [[Rcpp::export]]
Environment logsumexp_scalars_var(List terms) {
  if (terms.size() == 0) {
    return newvar_sexp(
      next_ad_node_name(),
      NumericVector::create(-std::numeric_limits<double>::infinity()),
      "logsumexp",
      List::create()
    );
  }
  NumericVector vals(terms.size());
  List parents = List::create();
  for (int i = 0; i < terms.size(); ++i) {
    Operand op = as_operand(terms[i]);
    vals[i] = as<NumericVector>(op.value)[0];
    parents.push_back(op.var);
  }
  double m = vals[0];
  for (int i = 1; i < vals.size(); ++i) {
    if (vals[i] > m) m = vals[i];
  }
  double s = 0.0;
  for (int i = 0; i < vals.size(); ++i) {
    s += std::exp(vals[i] - m);
  }
  double out = m + std::log(s);
  return newvar_sexp(
    next_ad_node_name(),
    NumericVector::create(out),
    "logsumexp",
    parents,
    List::create(Named("max") = m)
  );
}

// [[Rcpp::export]]
void reverse_diff_node(Environment myvar) {
  if (is_constant_cpp(myvar)) {
    return;
  }

  NumericVector mygrad = get_effective_grad(myvar);

  std::string myop = myvar["op"];
  g_last_reverse_op = myop;
  if (myop == "") {
    return;
  }

  List parents = myvar["parents"];
  Environment par1 = parents[0];
  bool par1const = is_constant_cpp(par1);
  SEXP par1val = get_node_value(par1);

  if (myop == "sum") {
    if (!par1const) {
      NumericVector gv = as_numeric(mygrad);
      double g = gv[0];
      NumericVector inc = as_numeric(par1val);
      for (R_xlen_t i = 0; i < inc.size(); ++i) inc[i] = g;
      add_grad(par1, preserve_dims(inc, par1val));
    }
    return;
  }

  if (myop == "mean") {
    if (!par1const) {
      NumericVector gv = as_numeric(mygrad);
      double g = gv[0];
      NumericVector inc = as_numeric(par1val);
      double scale = inc.size() > 0 ? g / static_cast<double>(inc.size()) : 0.0;
      for (R_xlen_t i = 0; i < inc.size(); ++i) inc[i] = scale;
      add_grad(par1, preserve_dims(inc, par1val));
    }
    return;
  }

  if (myop == "max") {
    if (!par1const) {
      List meta = myvar["meta"];
      int idx = as<int>(meta["index"]);
      NumericVector gv = as_numeric(mygrad);
      NumericVector inc = zeros_like(par1val);
      inc[idx - 1] = gv[0];
      add_grad(par1, inc);
    }
    return;
  }

  if (myop == "[") {
    if (!par1const) {
      List meta = myvar["meta"];
      int idx = as<int>(meta["index"]);
      NumericVector gv = as_numeric(mygrad);
      NumericVector inc = zeros_like(par1val);
      inc[idx - 1] = gv[0];
      add_grad(par1, inc);
    }
    return;
  }

  if (is_unary_op(myop)) {
    NumericVector p1v = as_numeric(par1val);
    NumericVector gv = align_grad(as_numeric(mygrad), p1v.size());
    NumericVector inc(p1v.size());
    if (myop == "neg") {
      inc = -gv;
    } else if (myop == "sin") {
      inc = gv * cos(p1v);
    } else if (myop == "cos") {
      inc = gv * (-sin(p1v));
    } else if (myop == "exp") {
      inc = gv * exp(p1v);
    } else if (myop == "log") {
      inc = gv / p1v;
    } else if (myop == "abs") {
      inc = gv * ad_sign_vec(p1v);
    } else if (myop == "sqrt") {
      inc = gv / (2 * sqrt(p1v));
    } else {
      stop("Unsupported operation: " + myop);
    }
    if (!par1const) add_grad(par1, preserve_dims(inc, par1val));
    return;
  }

  if (ad_custom::dispatch(myop, myvar, mygrad)) {
    return;
  }

  if (myop == "logsumexp") {
    List meta = myvar["meta"];
    double m = meta["max"];
    NumericVector gv = as_numeric(mygrad);
    double g = gv[0];
    double denom = 0.0;
    for (R_xlen_t i = 0; i < parents.size(); ++i) {
      Environment child = parents[i];
      double vi = as<NumericVector>(get_node_value(child))[0];
      denom += std::exp(vi - m);
    }
    for (R_xlen_t i = 0; i < parents.size(); ++i) {
      Environment child = parents[i];
      if (is_constant_cpp(child)) continue;
      double vi = as<NumericVector>(get_node_value(child))[0];
      double w = std::exp(vi - m) / denom;
      add_grad(child, NumericVector::create(g * w));
    }
    return;
  }

  Environment par2 = parents[1];
  bool par2const = is_constant_cpp(par2);
  SEXP par2val = get_node_value(par2);
  NumericVector p1v = as_numeric(par1val);
  NumericVector p2v = as_numeric(par2val);
  NumericVector gv = align_grad(as_numeric(mygrad),
    std::max(p1v.size(), p2v.size()));

  if (myop == "pmax" || myop == "pmin") {
    List meta = myvar["meta"];
    IntegerVector branch = meta["branch"];
    NumericVector gvp = align_grad(as_numeric(mygrad), branch.size());
    NumericVector inc1 = zeros_like(par1val);
    NumericVector inc2 = zeros_like(par2val);
    if (branch.size() == 1) {
      if (branch[0] == 1) {
        inc1 = gvp;
      } else {
        inc2 = gvp;
      }
    } else {
      for (R_xlen_t i = 0; i < branch.size(); ++i) {
        if (branch[i] == 1) inc1[i] = gvp[i];
        else inc2[i] = gvp[i];
      }
    }
    if (!par1const) add_grad(par1, inc1);
    if (!par2const) add_grad(par2, inc2);
    return;
  }

  if (myop == "%*%") {
    if (!par1const) {
      add_grad(par1, matmul_numeric(mygrad, transpose_sexp(par2val)));
    }
    if (!par2const) {
      add_grad(par2, matmul_numeric(transpose_sexp(par1val), mygrad));
    }
    return;
  }

  NumericVector inc1(p1v.size());
  NumericVector inc2(p2v.size());
  if (myop == "+") {
    NumericVector g1 = align_grad(gv, p1v.size());
    NumericVector g2 = align_grad(gv, p2v.size());
    inc1 = g1;
    inc2 = g2;
  } else if (myop == "-") {
    inc1 = align_grad(gv, p1v.size());
    inc2 = -align_grad(gv, p2v.size());
  } else if (myop == "*") {
    NumericVector g = align_grad(gv, p1v.size());
    inc1 = g * p2v;
    inc2 = align_grad(gv, p2v.size()) * p1v;
  } else if (myop == "/") {
    NumericVector g = align_grad(gv, p1v.size());
    inc1 = g / p2v;
    inc2 = -align_grad(gv, p2v.size()) * p1v / (p2v * p2v);
  } else if (myop == "^") {
    NumericVector g = align_grad(gv, p1v.size());
    NumericVector exp_m1 = clone(p2v);
    for (R_xlen_t i = 0; i < exp_m1.size(); ++i) exp_m1[i] -= 1.0;
    for (R_xlen_t i = 0; i < p1v.size(); ++i) {
      double p2i = p2v[i % p2v.size()];
      double em1 = exp_m1[i % exp_m1.size()];
      inc1[i] = g[i] * p2i * std::pow(p1v[i], em1);
    }
    if (!par2const) {
      NumericVector g2 = align_grad(gv, p2v.size());
      for (R_xlen_t i = 0; i < p2v.size(); ++i) {
        double p1i = p1v[i % p1v.size()];
        inc2[i] = g2[i] * std::pow(p1i, p2v[i]) * std::log(p1i);
      }
    }
  } else {
    stop("Unsupported operation: " + myop);
  }

  if (!par1const) add_grad(par1, preserve_dims_binary(inc1, par1val, mygrad));
  if (!par2const) add_grad(par2, preserve_dims_binary(inc2, par2val, mygrad));
}

// [[Rcpp::export]]
void reverse_tape_cpp(List tape) {
  g_use_grad_sidecar = true;
  reset_grad_sidecar_cpp();
  if (tape.size() > 0) {
    Environment root = tape[tape.size() - 1];
    root["grad"] = NumericVector::create(1.0);
  }
  for (int i = tape.size() - 1; i >= 0; --i) {
    reverse_diff_node(tape[i]);
  }
  apply_grad_sidecar_cpp(tape);
  g_use_grad_sidecar = false;
}

// [[Rcpp::export]]
void reverseDiff(Environment myvar) {
  std::function<void(Environment)> walk;
  walk = [&](Environment node) {
    if (is_constant_cpp(node)) return;
    std::string op = as<std::string>(node["op"]);
    if (op == "") return;
    reverse_diff_node(node);
    List parents = node["parents"];
    walk(parents[0]);
    if (parents.size() > 1) walk(parents[1]);
  };
  walk(myvar);
}

// [[Rcpp::export]]
double forwardDiff(Environment myvar) {
  std::unordered_map<int, SEXP> cache;

  std::function<SEXP(Environment)> propagate;
  propagate = [&](Environment node) -> SEXP {
    if (is_constant_cpp(node)) {
      int id = as<int>(node["node_id"]);
      auto found = cache.find(id);
      if (found != cache.end()) {
        return found->second;
      }
      SEXP z = zeros_like(get_node_value(node));
      cache[id] = z;
      return z;
    }

    std::string op = as<std::string>(node["op"]);
    if (op == "") {
      return get_node_tangent(node);
    }

    int id = as<int>(node["node_id"]);
    auto found = cache.find(id);
    if (found != cache.end()) {
      return found->second;
    }

    List parents = node["parents"];
    Environment par1 = parents[0];
    SEXP t1 = propagate(par1);
    SEXP par1val = get_node_value(par1);

    if (op == "sum") {
      NumericVector tv = as_numeric(t1);
      double s = 0.0;
      for (R_xlen_t i = 0; i < tv.size(); ++i) s += tv[i];
      SEXP tan = NumericVector::create(s);
      node["tangent"] = tan;
      cache[id] = tan;
      return tan;
    }

    if (op == "mean") {
      NumericVector tv = as_numeric(t1);
      double s = 0.0;
      for (R_xlen_t i = 0; i < tv.size(); ++i) s += tv[i];
      double m = tv.size() > 0 ? s / static_cast<double>(tv.size()) : 0.0;
      SEXP tan = NumericVector::create(m);
      node["tangent"] = tan;
      cache[id] = tan;
      return tan;
    }

    if (op == "max") {
      List meta = node["meta"];
      int idx = as<int>(meta["index"]);
      NumericVector tv = as_numeric(t1);
      SEXP tan = NumericVector::create(tv[idx - 1]);
      node["tangent"] = tan;
      cache[id] = tan;
      return tan;
    }

    if (op == "[") {
      List meta = node["meta"];
      int idx = as<int>(meta["index"]);
      NumericVector tv = as_numeric(t1);
      SEXP tan = NumericVector::create(tv[idx - 1]);
      node["tangent"] = tan;
      cache[id] = tan;
      return tan;
    }

    if (is_unary_op(op)) {
      NumericVector p1v = as_numeric(par1val);
      NumericVector tv = align_tangent(as_numeric(t1), p1v.size());
      NumericVector tan(p1v.size());
      if (op == "neg") {
        for (R_xlen_t i = 0; i < p1v.size(); ++i) tan[i] = -tv[i];
      } else if (op == "sin") {
        for (R_xlen_t i = 0; i < p1v.size(); ++i) tan[i] = tv[i] * std::cos(p1v[i]);
      } else if (op == "cos") {
        for (R_xlen_t i = 0; i < p1v.size(); ++i) tan[i] = -tv[i] * std::sin(p1v[i]);
      } else if (op == "exp") {
        for (R_xlen_t i = 0; i < p1v.size(); ++i) tan[i] = tv[i] * std::exp(p1v[i]);
      } else if (op == "log") {
        for (R_xlen_t i = 0; i < p1v.size(); ++i) tan[i] = tv[i] / p1v[i];
      } else if (op == "abs") {
        NumericVector sgn = ad_sign_vec(p1v);
        for (R_xlen_t i = 0; i < p1v.size(); ++i) tan[i] = tv[i] * sgn[i];
      } else if (op == "sqrt") {
        for (R_xlen_t i = 0; i < p1v.size(); ++i) tan[i] = tv[i] / (2 * std::sqrt(p1v[i]));
      } else {
        stop("Unsupported operation: " + op);
      }
      SEXP out = preserve_dims(tan, par1val);
      node["tangent"] = out;
      cache[id] = out;
      return out;
    }

    Environment par2 = parents[1];
    SEXP t2 = propagate(par2);
    SEXP par2val = get_node_value(par2);
    bool par2const = is_constant_cpp(par2);
    NumericVector tv1 = as_numeric(t1);
    NumericVector tv2 = as_numeric(t2);
    NumericVector p1v = as_numeric(par1val);
    NumericVector p2v = as_numeric(par2val);

    if (op == "pmax" || op == "pmin") {
      List meta = node["meta"];
      IntegerVector branch = meta["branch"];
      NumericVector tv1a = align_tangent(tv1, branch.size());
      NumericVector tv2a = align_tangent(tv2, branch.size());
      NumericVector tan = zeros_like(get_node_value(node));
      if (branch.size() == 1) {
        tan = branch[0] == 1 ? tv1a : tv2a;
      } else {
        for (R_xlen_t i = 0; i < branch.size(); ++i) {
          tan[i] = branch[i] == 1 ? tv1a[i] : tv2a[i];
        }
      }
      SEXP out = preserve_dims(tan, get_node_value(node));
      node["tangent"] = out;
      cache[id] = out;
      return out;
    }

    if (op == "%*%") {
      Environment base = Environment::base_env();
      Function plus = base["+"];
      SEXP tan = plus(matmul_sexp(t1, par2val), matmul_sexp(par1val, t2));
      node["tangent"] = tan;
      cache[id] = tan;
      return tan;
    }

    NumericVector tan(p1v.size());
    if (op == "+") {
      R_xlen_t out_len = std::max(p1v.size(), p2v.size());
      tan = NumericVector(out_len);
      NumericVector a1 = align_tangent(tv1, out_len);
      NumericVector a2 = align_tangent(tv2, out_len);
      for (R_xlen_t i = 0; i < out_len; ++i) tan[i] = a1[i] + a2[i];
    } else if (op == "-") {
      NumericVector a1 = align_tangent(tv1, p1v.size());
      NumericVector a2 = align_tangent(tv2, p2v.size());
      for (R_xlen_t i = 0; i < p1v.size(); ++i) tan[i] = a1[i] - a2[i % a2.size()];
    } else if (op == "*") {
      NumericVector a1 = align_tangent(tv1, p1v.size());
      NumericVector a2 = align_tangent(tv2, p2v.size());
      for (R_xlen_t i = 0; i < p1v.size(); ++i) {
        tan[i] = a1[i] * p2v[i % p2v.size()] + a2[i % a2.size()] * p1v[i];
      }
    } else if (op == "/") {
      NumericVector a1 = align_tangent(tv1, p1v.size());
      NumericVector a2 = align_tangent(tv2, p2v.size());
      for (R_xlen_t i = 0; i < p1v.size(); ++i) {
        double d = p2v[i % p2v.size()];
        tan[i] = a1[i] / d - a2[i % a2.size()] * p1v[i] / (d * d);
      }
    } else if (op == "^") {
      NumericVector a1 = align_tangent(tv1, p1v.size());
      NumericVector a2 = align_tangent(tv2, p1v.size());
      for (R_xlen_t i = 0; i < p1v.size(); ++i) {
        double p2i = p2v[i % p2v.size()];
        tan[i] = a1[i] * p2i * std::pow(p1v[i], p2i - 1.0);
      }
      if (!par2const) {
        for (R_xlen_t i = 0; i < p1v.size(); ++i) {
          double p2i = p2v[i % p2v.size()];
          tan[i] += a2[i] * std::pow(p1v[i], p2i) * std::log(p1v[i]);
        }
      }
    } else {
      stop("Unsupported operation: " + op);
    }

    SEXP out = preserve_dims_binary(tan, par1val, par2val);
    node["tangent"] = out;
    cache[id] = out;
    return out;
  };

  SEXP tan = propagate(myvar);
  NumericVector tv = as_numeric(tan);
  return tv[0];
}

namespace {

bool op_is_empty(Environment node) {
  if (!node.exists("op")) {
    return true;
  }
  SEXP op_sexp = node["op"];
  if (Rf_isNull(op_sexp)) {
    return true;
  }
  CharacterVector cv = as<CharacterVector>(op_sexp);
  if (cv.size() == 0) {
    return true;
  }
  if (CharacterVector::is_na(cv[0])) {
    return true;
  }
  return as<std::string>(cv[0]) == "";
}

bool is_param_leaf(Environment node) {
  if (is_constant_cpp(node)) {
    return false;
  }
  if (!node.exists("par")) {
    return false;
  }
  if (!as<bool>(node["par"])) {
    return false;
  }
  return op_is_empty(node);
}

void set_node_value(Environment node, SEXP val) {
  node["value"] = val;
}

void replay_unary(Environment node, const std::string& op) {
  List parents = node["parents"];
  NumericVector p1v = as_numeric(get_node_value(parents[0]));
  NumericVector out(p1v.size());
  if (op == "neg") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) out[i] = -p1v[i];
  } else if (op == "sin") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) out[i] = std::sin(p1v[i]);
  } else if (op == "cos") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) out[i] = std::cos(p1v[i]);
  } else if (op == "exp") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) out[i] = std::exp(p1v[i]);
  } else if (op == "log") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) out[i] = std::log(p1v[i]);
  } else if (op == "abs") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) out[i] = std::abs(p1v[i]);
  } else if (op == "sqrt") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) out[i] = std::sqrt(p1v[i]);
  } else {
    stop("Unsupported unary replay op: " + op);
  }
  set_node_value(node, preserve_dims(out, get_node_value(parents[0])));
}

void replay_binary(Environment node, const std::string& op) {
  List parents = node["parents"];
  NumericVector p1v = as_numeric(get_node_value(parents[0]));
  NumericVector p2v = as_numeric(get_node_value(parents[1]));
  NumericVector out(p1v.size());
  if (op == "+") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) {
      out[i] = p1v[i] + p2v[i % p2v.size()];
    }
  } else if (op == "-") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) {
      out[i] = p1v[i] - p2v[i % p2v.size()];
    }
  } else if (op == "*") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) {
      out[i] = p1v[i] * p2v[i % p2v.size()];
    }
  } else if (op == "/") {
    for (R_xlen_t i = 0; i < p1v.size(); ++i) {
      out[i] = p1v[i] / p2v[i % p2v.size()];
    }
  } else if (op == "^") {
    out = vec_pow(p1v, p2v);
  } else {
    stop("Unsupported binary replay op: " + op);
  }
  set_node_value(node, preserve_dims_binary(out, get_node_value(parents[0]),
                                            get_node_value(parents[1])));
}

void replay_logsumexp(Environment node) {
  List parents = node["parents"];
  double m = R_NegInf;
  for (R_xlen_t i = 0; i < parents.size(); ++i) {
    double vi = as<NumericVector>(get_node_value(parents[i]))[0];
    if (vi > m) m = vi;
  }
  double s = 0.0;
  for (R_xlen_t i = 0; i < parents.size(); ++i) {
    double vi = as<NumericVector>(get_node_value(parents[i]))[0];
    s += std::exp(vi - m);
  }
  double out = m + std::log(s);
  set_node_value(node, NumericVector::create(out));
  if (node.exists("meta")) {
    node["meta"] = List::create(Named("max") = m);
  }
}

void replay_node_value(Environment node) {
  if (op_is_empty(node)) {
    return;
  }
  std::string op = as<std::string>(node["op"]);
  if (ad_custom_fwd::dispatch(op, node)) {
    return;
  }
  List parents = node["parents"];
  if (op == "sum") {
    NumericVector p1v = as_numeric(get_node_value(parents[0]));
    double s = 0.0;
    for (R_xlen_t i = 0; i < p1v.size(); ++i) s += p1v[i];
    set_node_value(node, NumericVector::create(s));
    return;
  }
  if (op == "mean") {
    NumericVector p1v = as_numeric(get_node_value(parents[0]));
    double s = 0.0;
    for (R_xlen_t i = 0; i < p1v.size(); ++i) s += p1v[i];
    double m = p1v.size() > 0 ? s / static_cast<double>(p1v.size()) : 0.0;
    set_node_value(node, NumericVector::create(m));
    return;
  }
  if (op == "max") {
    NumericVector p1v = as_numeric(get_node_value(parents[0]));
    int idx = which_max_r(p1v);
    set_node_value(node, NumericVector::create(p1v[idx - 1]));
    if (node.exists("meta")) {
      node["meta"] = List::create(Named("index") = idx);
    }
    return;
  }
  if (op == "[") {
    List meta = node["meta"];
    int idx = as<int>(meta["index"]);
    NumericVector p1v = as_numeric(get_node_value(parents[0]));
    set_node_value(node, NumericVector::create(p1v[idx - 1]));
    return;
  }
  if (op == "logsumexp") {
    replay_logsumexp(node);
    return;
  }
  if (op == "pmax" || op == "pmin") {
    NumericVector p1v = as_numeric(get_node_value(parents[0]));
    NumericVector p2v = as_numeric(get_node_value(parents[1]));
    NumericVector out(p1v.size());
    IntegerVector branch(out.size());
    for (R_xlen_t i = 0; i < out.size(); ++i) {
      double a = p1v[i];
      double b = p2v[i % p2v.size()];
      if (op == "pmax") {
        out[i] = std::max(a, b);
        branch[i] = (a >= b) ? 1 : 2;
      } else {
        out[i] = std::min(a, b);
        branch[i] = (a <= b) ? 1 : 2;
      }
    }
    set_node_value(node, out);
    node["meta"] = List::create(Named("branch") = branch);
    return;
  }
  if (op == "%*%") {
    set_node_value(node, matmul_numeric(get_node_value(parents[0]),
                                        get_node_value(parents[1])));
    return;
  }
  if (is_unary_op(op)) {
    replay_unary(node, op);
    return;
  }
  replay_binary(node, op);
}

}  // namespace

// [[Rcpp::export]]
void reset_tape_grads_cpp(List tape) {
  for (int i = 0; i < tape.size(); ++i) {
    Environment node = tape[i];
    node["grad"] = zeros_like(get_node_value(node));
  }
}

// [[Rcpp::export]]
void replay_tape_values_cpp(List tape, List at) {
  CharacterVector at_names = at.names();
  if (at_names.size() == 0) {
    stop("Parameter list `at` must be named.");
  }
  for (int i = 0; i < tape.size(); ++i) {
    Environment node = tape[i];
    if (!is_param_leaf(node)) {
      continue;
    }
    std::string nm = getvarname(node);
    for (R_xlen_t j = 0; j < at_names.size(); ++j) {
      if (as<std::string>(at_names[j]) == nm) {
        set_node_value(node, at[j]);
        break;
      }
    }
  }
  for (int i = 0; i < tape.size(); ++i) {
    Environment node = tape[i];
    if (is_constant_cpp(node) || is_param_leaf(node)) {
      continue;
    }
    replay_node_value(node);
  }
}

// [[Rcpp::export]]
double tape_scalar_value_cpp(List tape) {
  if (tape.size() == 0) {
    return NA_REAL;
  }
  Environment root = tape[tape.size() - 1];
  NumericVector v = as<NumericVector>(get_node_value(root));
  return v[0];
}

