#' LibeRtAD: the compiled automatic-differentiation layer for LibeR
#'
#' Compiles a restricted R-like mathematical model language to a serializable
#' intermediate representation and evaluates persistent CppAD tapes using the
#' bundled CppAD 20260000.0 and Eigen 5.0.1 headers. Dynamic parameters allow
#' non-differentiated data to change without retaping, and optimized graph
#' caches reconstruct tapes with exact source-provenance checks. A small R6
#' wrapper owns external pointers while numerical evaluation, gradients, dense
#' or sparse Jacobians, Hessians, matrix operations, and guarded tensor and
#' Smolyak sparse Gauss-Hermite quadrature grids execute in C++.
#'
#' @keywords internal
"_PACKAGE"
