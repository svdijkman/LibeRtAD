#ifndef LIBERTAD_EIGEN_R_HPP
#define LIBERTAD_EIGEN_R_HPP

#include <Rcpp.h>
#include <LibeRtAD/eigen.hpp>

#include <type_traits>

namespace libertad {

// The LibeR native API exports R vectors and matrices, not Eigen objects. Keep
// this deliberately small bridge explicit so Eigen can be versioned without a
// dependency on the much broader RcppEigen conversion layer.
template <typename Derived>
Rcpp::NumericVector eigen_vector_to_r(
    const Eigen::MatrixBase<Derived>& value) {
  using Scalar = typename Derived::Scalar;
  static_assert(std::is_convertible<Scalar, double>::value,
                "Only real-valued Eigen vectors can be returned to R.");
  Rcpp::NumericVector result(static_cast<R_xlen_t>(value.size()));
  for (Eigen::Index index = 0; index < value.size(); ++index) {
    result[static_cast<R_xlen_t>(index)] = static_cast<double>(value(index));
  }
  return result;
}

template <typename Derived>
Rcpp::NumericMatrix eigen_matrix_to_r(
    const Eigen::MatrixBase<Derived>& value) {
  using Scalar = typename Derived::Scalar;
  static_assert(std::is_convertible<Scalar, double>::value,
                "Only real-valued Eigen matrices can be returned to R.");
  Rcpp::NumericMatrix result(static_cast<int>(value.rows()),
                             static_cast<int>(value.cols()));
  for (Eigen::Index column = 0; column < value.cols(); ++column) {
    for (Eigen::Index row = 0; row < value.rows(); ++row) {
      result(static_cast<int>(row), static_cast<int>(column)) =
        static_cast<double>(value(row, column));
    }
  }
  return result;
}

inline Eigen::Map<const Eigen::VectorXd> r_vector_map(
    const Rcpp::NumericVector& value) {
  return Eigen::Map<const Eigen::VectorXd>(
    value.begin(), static_cast<Eigen::Index>(value.size()));
}

inline Eigen::Map<const Eigen::MatrixXd> r_matrix_map(
    const Rcpp::NumericMatrix& value) {
  return Eigen::Map<const Eigen::MatrixXd>(
    value.begin(), static_cast<Eigen::Index>(value.nrow()),
    static_cast<Eigen::Index>(value.ncol()));
}

}  // namespace libertad

#endif
