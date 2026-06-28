#' Unified automatic differentiation
#'
#' Differentiates a scalar function using reverse or forward mode. This is the
#' main entry point; \code{\link{backdiff}} and \code{\link{forwarddiff}} are
#' convenience wrappers.
#'
#' @param f A function of one or more numeric arguments.
#' @param ... Named numeric values at which to evaluate and differentiate.
#'   Values may be scalars, vectors, or matrices.
#' @param at Optional named list of parameter values. Use either \code{...} or
#'   \code{at}, not both.
#' @param mode Either \code{"reverse"} or \code{"forward"}.
#' @param backend Either \code{"R"} or \code{"cpp"} for the differentiation pass.
#' @param record_tape Logical; record intermediate nodes on the tape.
#' @param print_result Logical; print a summary of the result.
#' @return A list with components \code{value}, \code{gradient}, \code{partials},
#'   \code{partials_flat}, and \code{node}.
#' @export
#' @examples
#' f <- function(x) sum(x^2)
#' autodiff(f, x = c(1, 2, 3), mode = "reverse")
#' autodiff(f, x = c(1, 2, 3), mode = "forward", backend = "cpp")
autodiff <- function(f, ..., at = NULL, mode = c("reverse", "forward"),
                     backend = c("R", "cpp"), record_tape = TRUE,
                     print_result = FALSE) {
  at <- .ad_parse_at(at, ...)
  .ad_autodiff(
    f, at,
    mode = match.arg(mode),
    backend = match.arg(backend),
    record_tape = record_tape,
    print_result = print_result
  )
}

#' Reverse-mode automatic differentiation
#'
#' Differentiates a scalar function with respect to the supplied parameter
#' values. Parameters are automatically converted to internal AD variables.
#' Vector parameters are supported; each element receives its own partial
#' derivative in \code{partials}.
#'
#' @param f A function of one or more numeric arguments.
#' @param ... Named numeric values at which to evaluate and differentiate.
#'   Values may be scalars or numeric vectors.
#' @param at Optional named list of parameter values. Use either \code{...} or
#'   \code{at}, not both.
#' @param backend Either \code{"R"} or \code{"cpp"} for the backward pass.
#' @param record_tape Logical; record intermediate nodes on the tape.
#' @param print_result Logical; print a summary of the result.
#' @return A list with components \code{value}, \code{gradient}, \code{partials},
#'   \code{partials_flat}, and \code{node}.
#' @export
#' @examples
#' f <- function(x, y, z) {
#'   3 * x^4 + y / z / x + z * 3
#' }
#' backdiff(f, x = 2, y = 2, z = 3)
backdiff <- function(f, ..., at = NULL, backend = c("R", "cpp"),
                     record_tape = TRUE, print_result = FALSE) {
  at <- .ad_parse_at(at, ...)
  autodiff(
    f, at = at, mode = "reverse", backend = match.arg(backend),
    record_tape = record_tape, print_result = print_result
  )
}

#' Forward-mode automatic differentiation
#'
#' Computes partial derivatives by propagating tangent vectors from each
#' parameter degree of freedom. Useful when there are few parameters and
#' many operations.
#'
#' @inheritParams autodiff
#' @export
#' @examples
#' f <- function(x) {
#'   sum(x^2)
#' }
#' forwarddiff(f, x = c(1, 2, 3))
forwarddiff <- function(f, ..., at = NULL, backend = c("R", "cpp"),
                        record_tape = TRUE, print_result = FALSE) {
  at <- .ad_parse_at(at, ...)
  .ad_autodiff(
    f, at, mode = "forward", backend = match.arg(backend),
    record_tape = record_tape, print_result = print_result
  )
}

#' @rdname backdiff
#' @export
#' @examples
#' f <- function(x) x^2
#' backdiffR(f, x = 2)
backdiffR <- function(f, ..., at = NULL, print_result = FALSE) {
  backdiff(
    f, ..., at = at, backend = "R",
    record_tape = TRUE, print_result = print_result
  )
}

#' @rdname backdiff
#' @export
#' @examples
#' f <- function(x) x^2
#' backdiffCPP(f, x = 2)
backdiffCPP <- function(f, ..., at = NULL, print_result = FALSE) {
  backdiff(
    f, ..., at = at, backend = "cpp",
    record_tape = TRUE, print_result = print_result
  )
}

#' @rdname forwarddiff
#' @export
#' @examples
#' f <- function(x) x^2
#' forwarddiffCPP(f, x = 2)
forwarddiffCPP <- function(f, ..., at = NULL, print_result = FALSE) {
  forwarddiff(
    f, ..., at = at, backend = "cpp",
    record_tape = TRUE, print_result = print_result
  )
}

#' Symbolic differentiation baseline
#'
#' Uses R's built-in \code{\link[stats]{deriv}} (\code{D()}) for comparison.
#' Supports scalar parameters only.
#'
#' @inheritParams backdiff
#' @export
#' @examples
#' f <- function(x, y, z) {
#'   3 * x^4 + y / z / x + z * 3
#' }
#' symbolicD(f, x = 2, y = 2, z = 3)
symbolicD <- function(f, ..., at = NULL, print_result = FALSE) {
  at <- .ad_parse_at(at, ...)
  .ad_check_formals(f, at)
  if (any(vapply(at, function(x) length(x) != 1L, logical(1)))) {
    stop("symbolicD supports scalar parameters only.", call. = FALSE)
  }
  parameters <- .ad_values_to_parameters(at)

  reset_tape()
  .ad_state$active <- FALSE
  set_ops("R")

  .ad_reset_gradients(parameters)
  result <- .ad_eval_function(f, parameters)

  env <- .ad_make_value_env(parameters)
  body_expr <- .ad_function_expr(f)

  partials <- vapply(parameters, function(p) {
    eval(D(body_expr, p$name), envir = env)
  }, numeric(1))
  names(partials) <- vapply(parameters, function(p) p$name, character(1))

  out <- list(
    value = result$value,
    gradient = sum(partials),
    partials = as.list(partials),
    partials_flat = partials,
    node = result
  )

  if (print_result) {
    print(out)
  }
  out
}
