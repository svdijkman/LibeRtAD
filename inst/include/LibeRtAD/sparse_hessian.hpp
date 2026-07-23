#ifndef LIBERTAD_SPARSE_HESSIAN_HPP
#define LIBERTAD_SPARSE_HESSIAN_HPP

#include <cppad/cppad.hpp>

#include <algorithm>
#include <cstddef>
#include <string>
#include <vector>

namespace libertad {

using SparseSizeVector = CppAD::vector<std::size_t>;
using SparseValueVector = CppAD::vector<double>;

struct SparseHessianCache {
  CppAD::sparse_rc<SparseSizeVector> pattern;
  CppAD::sparse_hes_work work;
  bool analysed = false;
  bool use_sparse = false;
  std::size_t nonzeros = 0U;
  std::size_t sweeps = 0U;
  double density = 1.0;
  std::string strategy = "not-evaluated";
};

inline void analyse_hessian_sparsity(
    CppAD::ADFun<double>& fun, SparseHessianCache& cache,
    std::size_t minimum_dimension = 32U, double maximum_density = 0.35) {
  if (cache.analysed) return;
  cache.analysed = true;
  const std::size_t n = fun.Domain();
  if (n < minimum_dimension || fun.Range() != 1U) {
    cache.strategy = "dense-directional";
    return;
  }
  CppAD::vectorBool select_domain(n), select_range(1U);
  for (std::size_t index = 0; index < n; ++index) {
    select_domain[index] = true;
  }
  select_range[0] = true;
  fun.for_hes_sparsity(
    select_domain, select_range, false, cache.pattern);
  cache.nonzeros = cache.pattern.nnz();
  cache.density = static_cast<double>(cache.nonzeros) /
    static_cast<double>(std::max<std::size_t>(n * n, 1U));
  cache.use_sparse = cache.nonzeros > 0U &&
    cache.density <= maximum_density;
  cache.strategy = cache.use_sparse ?
    "sparse-colored" : "dense-directional";
}

inline std::vector<double> sparse_hessian(
    CppAD::ADFun<double>& fun, const std::vector<double>& point,
    SparseHessianCache& cache) {
  const std::size_t n = fun.Domain();
  SparseValueVector x(n), weight(1U);
  for (std::size_t index = 0; index < n; ++index) x[index] = point[index];
  weight[0] = 1.0;
  CppAD::sparse_rcv<SparseSizeVector, SparseValueVector> subset(
    cache.pattern);
  cache.sweeps = fun.sparse_hes(
    x, weight, subset, cache.pattern, "cppad.symmetric", cache.work);
  std::vector<double> result(n * n, 0.0);
  std::vector<bool> present(n * n, false);
  for (std::size_t index = 0; index < subset.nnz(); ++index) {
    const std::size_t row = subset.row()[index];
    const std::size_t column = subset.col()[index];
    result[row * n + column] = subset.val()[index];
    present[row * n + column] = true;
  }
  for (std::size_t row = 0; row < n; ++row) {
    for (std::size_t column = row + 1U; column < n; ++column) {
      const bool upper = present[row * n + column];
      const bool lower = present[column * n + row];
      if (upper && !lower) {
        result[column * n + row] = result[row * n + column];
      } else if (lower && !upper) {
        result[row * n + column] = result[column * n + row];
      }
    }
  }
  return result;
}

}  // namespace libertad

#endif
