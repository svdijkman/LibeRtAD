#' Sparse Jacobian from dense AD partials
#'
#' Drops entries with absolute value below \code{tol}.
#'
#' @param jacobian Dense matrix or vector of partial derivatives.
#' @param tol Absolute tolerance for sparsity pattern.
#' @param row_names Optional row names (parameters).
#' @param col_names Optional column names (outputs).
#' @return A sparse triplet list with components \code{i}, \code{j}, \code{x}, and \code{dim}.
#' @export
#' @examples
#' sparse_jacobian(matrix(c(1, 0, 0, 2), nrow = 2))
sparse_jacobian <- function(jacobian, tol = 1e-10,
                            row_names = NULL, col_names = NULL) {
  if (is.vector(jacobian) || (is.matrix(jacobian) && ncol(jacobian) == 1L)) {
    jacobian <- matrix(jacobian, ncol = 1L)
  }
  nr <- nrow(jacobian)
  nc <- ncol(jacobian)
  if (!is.null(row_names)) rownames(jacobian) <- row_names
  if (!is.null(col_names)) colnames(jacobian) <- col_names
  idx <- which(abs(jacobian) > tol, arr.ind = TRUE)
  if (nrow(idx) == 0L) {
    return(list(i = integer(), j = integer(), x = numeric(), dim = c(nr, nc)))
  }
  list(
    i = idx[, 1L],
    j = idx[, 2L],
    x = jacobian[idx],
    dim = c(nr, nc)
  )
}

#' Sparse Jacobian of a scalar function via AD
#'
#' Runs reverse-mode AD and returns a sparse triplet Jacobian for the
#' partial derivatives at the supplied parameter values.
#'
#' @inheritParams backdiff
#' @param tol Absolute tolerance for dropping near-zero partials.
#' @return A sparse triplet list; see [sparse_jacobian()].
#' @export
#' @examples
#' f <- function(x) x^2
#' ad_sparse_jacobian(f, x = 2)
ad_sparse_jacobian <- function(f, ..., at = NULL, tol = 1e-10, backend = c("R", "cpp")) {
  g <- backdiff(f, ..., at = at, backend = backend)
  jac <- matrix(g$partials_flat, nrow = length(g$partials_flat), ncol = 1L)
  sparse_jacobian(jac, tol = tol, row_names = names(g$partials_flat), col_names = "f")
}
