#' Compute Hessian via forward-over-reverse mode
#'
#' @inheritParams autodiff
#' @return List with \code{value}, \code{gradient}, and \code{hessian}.
#' @export
#' @examples
#' f <- function(x, y) x^2 + x * y + y^2
#' autodiff_hessian(f, x = 1, y = 2)
autodiff_hessian <- function(f, ..., at = NULL, backend = c("R", "cpp")) {
  at <- .ad_parse_at(at, ...)
  .ad_check_formals(f, at)
  backend <- match.arg(backend)
  nms <- names(at)
  n <- length(at)
  gfun <- function(x) {
    at2 <- at
    at2[nms] <- x
    backdiff(f, at = at2, backend = backend)$partials_flat
  }
  x0 <- unlist(at[nms])
  g0 <- gfun(x0)
  H <- matrix(0, n, n)
  for (j in seq_len(n)) {
    xj <- x0
    eps <- 1e-5 * max(1, abs(x0[j]))
    xj[j] <- x0[j] + eps
    H[, j] <- (gfun(xj) - g0) / eps
  }
  list(
    value = autodiff(f, at = at, mode = "reverse", backend = backend)$value,
    gradient = g0,
    hessian = (H + t(H)) / 2,
    par = nms
  )
}

#' Tape statistics from the last AD evaluation
#' @return A list with \code{n_nodes}, \code{n_params}, and \code{active}.
#' @export
#' @examples
#' f <- function(x) x^2
#' autodiff(f, x = 2)
#' ad_tape_stats()
ad_tape_stats <- function() {
  nodes <- .ad_state$nodes
  if (is.null(nodes) || length(nodes) == 0L) {
    return(list(n_nodes = 0L, n_params = 0L))
  }
  list(
    n_nodes = length(nodes),
    n_params = length(.ad_state$parameters %||% list()),
    active = isTRUE(.ad_state$active)
  )
}

#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
