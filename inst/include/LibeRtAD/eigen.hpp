#ifndef LIBERTAD_EIGEN_HPP
#define LIBERTAD_EIGEN_HPP

// Keep every Eigen module reachable through the LibeRtAD public interface on
// Eigen's MPL-2.0 or more permissive code path.
#ifndef EIGEN_MPL2_ONLY
#define EIGEN_MPL2_ONLY
#endif

// LibeRtAD owns both dependency snapshots. Include CppAD through the R console
// adapter first, then install CppAD's upstream Eigen scalar traits before any
// broader Eigen module is parsed. This keeps AD/Eigen interoperability and
// include ordering identical for LibeRtAD and downstream packages.
#include <LibeRtAD/cppad_r_output.hpp>
#include <cppad/example/cppad_eigen.hpp>

#include <Eigen/Dense>
#include <Eigen/Eigenvalues>

namespace libertad {

inline constexpr const char* eigen_version = "5.0.1";
inline constexpr const char* eigen_source_commit =
  "bc3b39870ecb690a623a3f49149a358b95c5781d";

static_assert(EIGEN_WORLD_VERSION == 3 && EIGEN_MAJOR_VERSION == 5 &&
                EIGEN_MINOR_VERSION == 0 && EIGEN_PATCH_VERSION == 1,
              "LibeRtAD must be compiled against its bundled Eigen 5.0.1 headers.");

}  // namespace libertad

#endif
