// [[Rcpp::depends(RcppEigen, RcppEigenAD, BH)]]
// [[Rcpp::plugins(cpp17)]]

#include <RcppEigen.h>
#include <LibeRtAD/program.hpp>

#include <algorithm>
#include <sstream>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

namespace {

using libertad::Program;
using libertad::ProgramHandle;
using libertad::TapeHandle;

std::vector<std::string> character_vector(const Rcpp::CharacterVector& x) {
  return Rcpp::as<std::vector<std::string>>(x);
}

std::vector<double> numeric_vector(const Rcpp::NumericVector& x) {
  return Rcpp::as<std::vector<double>>(x);
}

void ensure_unique(const std::vector<std::string>& values, const char* what) {
  std::unordered_set<std::string> seen;
  for (const std::string& value : values) {
    if (!seen.insert(value).second) {
      Rcpp::stop("Duplicate %s name: %s", what, value.c_str());
    }
  }
}

Rcpp::NumericVector named_vector(const std::vector<double>& values,
                                 const std::vector<std::string>& names) {
  Rcpp::NumericVector result(values.begin(), values.end());
  result.attr("names") = Rcpp::wrap(names);
  return result;
}

std::vector<double> tape_point(const TapeHandle& tape, const Rcpp::NumericVector& x) {
  if (x.size() != static_cast<R_xlen_t>(tape.domain.size())) {
    Rcpp::stop("Tape point has length %lld; expected %lld.",
               static_cast<long long>(x.size()),
               static_cast<long long>(tape.domain.size()));
  }
  return numeric_vector(x);
}

std::vector<double> tape_forward_zero(TapeHandle& tape,
                                      const std::vector<double>& point) {
  std::ostringstream messages;
  return tape.fun.Forward(0, point, messages);
}

std::vector<double> tape_jacobian(TapeHandle& tape,
                                  const std::vector<double>& point) {
  const std::size_t n = tape.domain.size();
  const std::size_t m = tape.range.size();
  tape_forward_zero(tape, point);
  std::vector<double> jacobian(m * n, 0.0);
  if (n <= m) {
    std::vector<double> direction(n, 0.0);
    std::ostringstream messages;
    for (std::size_t j = 0; j < n; ++j) {
      direction[j] = 1.0;
      std::vector<double> derivative = tape.fun.Forward(1, direction, messages);
      direction[j] = 0.0;
      for (std::size_t i = 0; i < m; ++i) jacobian[i * n + j] = derivative[i];
    }
  } else {
    std::vector<double> weight(m, 0.0);
    for (std::size_t i = 0; i < m; ++i) {
      weight[i] = 1.0;
      std::vector<double> derivative = tape.fun.Reverse(1, weight);
      weight[i] = 0.0;
      for (std::size_t j = 0; j < n; ++j) jacobian[i * n + j] = derivative[j];
    }
  }
  return jacobian;
}

std::vector<double> tape_hessian(TapeHandle& tape,
                                 const std::vector<double>& point) {
  const std::size_t n = tape.domain.size();
  tape_forward_zero(tape, point);
  std::vector<double> hessian(n * n, 0.0);
  std::vector<double> direction(n, 0.0);
  std::vector<double> weight(1, 1.0);
  std::ostringstream messages;
  for (std::size_t j = 0; j < n; ++j) {
    direction[j] = 1.0;
    tape.fun.Forward(1, direction, messages);
    direction[j] = 0.0;
    std::vector<double> reverse = tape.fun.Reverse(2, weight);
    for (std::size_t k = 0; k < n; ++k) hessian[k * n + j] = reverse[k * 2 + 1];
  }
  return hessian;
}

}  // namespace

// [[Rcpp::export(name = ".libertad_program_create")]]
SEXP libertad_program_create(const Rcpp::List& ir) {
  Rcpp::XPtr<ProgramHandle> pointer(new ProgramHandle(ir), true);
  pointer.attr("class") = Rcpp::CharacterVector::create("libertad_program_ptr", "externalptr");
  return pointer;
}

// [[Rcpp::export(name = ".libertad_program_value")]]
Rcpp::NumericVector libertad_program_value(
    SEXP program_pointer,
    const Rcpp::NumericVector& at,
    const Rcpp::CharacterVector& outputs) {
  Rcpp::XPtr<ProgramHandle> handle(program_pointer);
  std::vector<std::string> output_names = character_vector(outputs);
  std::vector<std::size_t> selected = handle->program->select_outputs(output_names);
  std::vector<double> values = handle->program->eval_outputs(numeric_vector(at), selected);
  return named_vector(values, output_names);
}

// [[Rcpp::export(name = ".libertad_tape_create")]]
SEXP libertad_tape_create(
    SEXP program_pointer,
    const Rcpp::NumericVector& at,
    const Rcpp::CharacterVector& wrt,
    const Rcpp::CharacterVector& outputs,
    bool optimize = true) {
  Rcpp::XPtr<ProgramHandle> handle(program_pointer);
  const Program& program = *handle->program;
  std::vector<std::string> domain = character_vector(wrt);
  std::vector<std::string> range = character_vector(outputs);
  if (domain.empty()) Rcpp::stop("A tape requires at least one differentiation input.");
  if (range.empty()) Rcpp::stop("A tape requires at least one output.");
  ensure_unique(domain, "tape input");
  ensure_unique(range, "tape output");
  if (at.size() != static_cast<R_xlen_t>(program.input_names.size())) {
    Rcpp::stop("Recording point has the wrong length.");
  }

  std::vector<int> wrt_positions;
  wrt_positions.reserve(domain.size());
  for (const std::string& name : domain) {
    auto it = program.input_positions.find(name);
    if (it == program.input_positions.end()) {
      Rcpp::stop("Unknown tape input: %s", name.c_str());
    }
    wrt_positions.push_back(static_cast<int>(it->second));
  }
  std::vector<std::size_t> selected = program.select_outputs(range);

  using AD = CppAD::AD<double>;
  std::vector<AD> independent(domain.size());
  for (std::size_t i = 0; i < domain.size(); ++i) {
    independent[i] = at[wrt_positions[i]];
  }
  CppAD::Independent(independent);

  std::vector<AD> full_inputs(program.input_names.size());
  for (std::size_t i = 0; i < full_inputs.size(); ++i) full_inputs[i] = at[i];
  for (std::size_t i = 0; i < domain.size(); ++i) {
    full_inputs[static_cast<std::size_t>(wrt_positions[i])] = independent[i];
  }
  std::vector<AD> dependent = program.eval_outputs(full_inputs, selected);
  CppAD::ADFun<double> fun;
  fun.Dependent(independent, dependent);
  if (optimize) fun.optimize();

  Rcpp::XPtr<TapeHandle> pointer(
    new TapeHandle(handle->program, std::move(fun), domain, range), true
  );
  pointer.attr("class") = Rcpp::CharacterVector::create("libertad_tape_ptr", "externalptr");
  return pointer;
}

// [[Rcpp::export(name = ".libertad_tape_value")]]
Rcpp::NumericVector libertad_tape_value(SEXP tape_pointer,
                                         const Rcpp::NumericVector& x) {
  Rcpp::XPtr<TapeHandle> tape(tape_pointer);
  std::vector<double> value = tape_forward_zero(*tape, tape_point(*tape, x));
  return named_vector(value, tape->range);
}

// [[Rcpp::export(name = ".libertad_tape_jacobian")]]
Rcpp::NumericMatrix libertad_tape_jacobian(SEXP tape_pointer,
                                            const Rcpp::NumericVector& x) {
  Rcpp::XPtr<TapeHandle> tape(tape_pointer);
  const std::size_t nr = tape->range.size();
  const std::size_t nc = tape->domain.size();
  std::vector<double> jac = tape_jacobian(*tape, tape_point(*tape, x));
  Rcpp::NumericMatrix result(nr, nc);
  for (std::size_t i = 0; i < nr; ++i) {
    for (std::size_t j = 0; j < nc; ++j) result(i, j) = jac[i * nc + j];
  }
  result.attr("dimnames") = Rcpp::List::create(Rcpp::wrap(tape->range), Rcpp::wrap(tape->domain));
  return result;
}

// [[Rcpp::export(name = ".libertad_tape_hessian")]]
Rcpp::NumericMatrix libertad_tape_hessian(SEXP tape_pointer,
                                           const Rcpp::NumericVector& x) {
  Rcpp::XPtr<TapeHandle> tape(tape_pointer);
  if (tape->range.size() != 1U) Rcpp::stop("Hessian requires a scalar tape output.");
  const std::size_t n = tape->domain.size();
  std::vector<double> hessian = tape_hessian(*tape, tape_point(*tape, x));
  Rcpp::NumericMatrix result(n, n);
  for (std::size_t i = 0; i < n; ++i) {
    for (std::size_t j = 0; j < n; ++j) result(i, j) = hessian[i * n + j];
  }
  result.attr("dimnames") = Rcpp::List::create(Rcpp::wrap(tape->domain), Rcpp::wrap(tape->domain));
  return result;
}

// [[Rcpp::export(name = ".libertad_tape_value_gradient")]]
Rcpp::List libertad_tape_value_gradient(SEXP tape_pointer,
                                         const Rcpp::NumericVector& x) {
  Rcpp::XPtr<TapeHandle> tape(tape_pointer);
  if (tape->range.size() != 1U) Rcpp::stop("Gradient requires a scalar tape output.");
  std::vector<double> point = tape_point(*tape, x);
  std::vector<double> value = tape_forward_zero(*tape, point);
  std::vector<double> jac = tape_jacobian(*tape, point);
  return Rcpp::List::create(
    Rcpp::Named("value") = named_vector(value, tape->range),
    Rcpp::Named("gradient") = named_vector(jac, tape->domain)
  );
}

// [[Rcpp::export(name = ".libertad_engine_info")]]
Rcpp::List libertad_engine_info() {
  return Rcpp::List::create(
    Rcpp::Named("backend") = "CppAD via RcppEigenAD",
    Rcpp::Named("scalar") = "double",
    Rcpp::Named("persistent_tape") = true,
    Rcpp::Named("cpp_standard") = 17,
    Rcpp::Named("thread_state") = "one independent tape per ADModel"
  );
}
