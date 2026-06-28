#' LibeRtAD: Reverse-Mode Automatic Differentiation in R
#'
#' LibeRtAD implements reverse- and forward-mode automatic differentiation
#' for scalar objectives using R6 computational graphs. An optional C++
#' backend accelerates the backward pass; tape caching, Hessians, and sparse
#' Jacobians are supported. Linked packages can register custom reverse rules
#' via registered C callables.
#'
#' @section Main entry points:
#' * [autodiff()] — unified AD (reverse or forward, R or C++ backend)
#' * [backdiff()] — reverse-mode convenience wrapper
#' * [forwarddiff()] — forward-mode convenience wrapper
#' * [autodiff_hessian()] — Hessian via AD or numeric fallback
#' * [sparse_jacobian()] — sparse Jacobian structure and values
#'
#' @section Tape utilities:
#' * [reset_tape()], [ad_tape_save()], [ad_tape_load()], [ad_tape_reuse()],
#'   [ad_tape_stats()]
#'
#' @section Low-level graph API:
#' * [Variable], [Constant], [newVariable()], [newConstant()], [ad_var()]
#' * Arithmetic and math overloads for `Variable` objects (see [Variable])
#'
#' @section Options:
#' * `options(LibeRtAD.n_cores = ...)` — parallel worker count for tape ops
#' * `options(LibeRtAD.n_cores_auto = TRUE)` — auto-detect cores minus one
#'
#' @seealso The example script at `inst/examples/basic-example.R`.
"_PACKAGE"
