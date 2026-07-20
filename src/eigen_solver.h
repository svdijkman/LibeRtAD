#ifndef LIBERTAD_NATIVE_EIGEN_SOLVER_HPP
#define LIBERTAD_NATIVE_EIGEN_SOLVER_HPP

#include <LibeRtAD/eigen.hpp>

namespace libertad {
namespace detail {

struct SelfAdjointEigenResult {
  Eigen::ComputationInfo info = Eigen::InvalidInput;
  Eigen::VectorXd values;
  Eigen::MatrixXd vectors;
};

SelfAdjointEigenResult self_adjoint_eigen(
  const Eigen::MatrixXd& matrix, bool compute_vectors = true);

}  // namespace detail
}  // namespace libertad

#endif
