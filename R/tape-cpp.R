#' Replay parameter values on a cached AD tape (C++ backend)
#'
#' Low-level helper used by [ad_tape_reuse()] to bind new parameter values into
#' an existing computational graph without rebuilding the tape.
#'
#' @param tape List of AD graph nodes from a recorded tape.
#' @param at Named list of parameter values (same names as when the tape was built).
#' @return Invisibly \code{NULL}.
#' @seealso [ad_tape_save()], [ad_tape_reuse()], [reset_tape_grads_cpp()]
#' @keywords internal
#' @export
#' @name replay_tape_values_cpp
NULL

#' Reset gradient sidecars on a tape (C++ backend)
#'
#' Clears accumulated partial derivatives on each node before a reverse pass.
#' Called internally when reusing a cached tape with [ad_tape_reuse()].
#'
#' @param tape List of AD graph nodes.
#' @return Invisibly \code{NULL}.
#' @seealso [replay_tape_values_cpp()], [ad_tape_reuse()]
#' @keywords internal
#' @export
#' @name reset_tape_grads_cpp
NULL

#' Scalar objective value from a tape root (C++ backend)
#'
#' Returns the numeric value at the root node after values have been replayed.
#'
#' @param tape List of AD graph nodes.
#' @return Numeric scalar objective value.
#' @seealso [replay_tape_values_cpp()], [ad_tape_reuse()]
#' @keywords internal
#' @export
#' @name tape_scalar_value_cpp
NULL
