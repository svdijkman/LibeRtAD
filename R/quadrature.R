#' Standard-normal tensor Gauss--Hermite quadrature grid
#'
#' Constructs a tensor-product Gaussian quadrature rule for expectations under
#' a multivariate standard-normal distribution. The one-dimensional rule uses
#' the Golub--Welsch eigensystem in C++; tensor nodes and log weights are also
#' assembled in C++ to avoid repeated R-side grid construction.
#'
#' @param order Number of nodes per dimension.
#' @param dimension Dimension of the normal integral. Zero returns the single
#'   empty node with unit mass.
#' @param max_points Maximum permitted tensor-grid size. This guard prevents an
#'   accidental exponential allocation as the dimension grows.
#' @return A list containing `nodes`, normalized `weights`, `log_weights`, and
#'   grid metadata.
#' @export
ad_gauss_hermite <- function(order = 5L, dimension = 1L,
                             max_points = 100000L) {
  order <- as.integer(order)
  dimension <- as.integer(dimension)
  max_points <- as.numeric(max_points)
  if (length(order) != 1L || is.na(order)) {
    .ad_stop("`order` must be one integer.")
  }
  if (length(dimension) != 1L || is.na(dimension)) {
    .ad_stop("`dimension` must be one integer.")
  }
  if (length(max_points) != 1L || is.na(max_points)) {
    .ad_stop("`max_points` must be one number.")
  }
  .libertad_gauss_hermite_grid(order, dimension, max_points)
}

#' Standard-normal Smolyak Gauss--Hermite quadrature grid
#'
#' Constructs an isotropic Smolyak sparse grid for multivariate
#' standard-normal expectations. Level `i` uses the odd-order
#' one-dimensional Gauss--Hermite rule with `2 * i - 1` nodes. Duplicate nodes
#' are consolidated in C++ and negligible cancellation residues are removed.
#'
#' Smolyak combination weights are signed. `weights` therefore contains the
#' signed masses, while `signs` and `log_abs_weights` provide a numerically
#' stable representation for likelihood integration.
#'
#' @param level Sparse-grid accuracy level. Level one is the single origin
#'   node; increasing the level adds higher-order interactions.
#' @param dimension Dimension of the normal integral. Zero returns the single
#'   empty node with unit mass.
#' @param max_points Maximum permitted number of retained sparse-grid nodes.
#' @return A list containing `nodes`, signed `weights`, `signs`,
#'   `log_abs_weights`, and sparse-grid metadata.
#' @export
ad_smolyak_gauss_hermite <- function(level = 3L, dimension = 4L,
                                     max_points = 100000L) {
  level <- as.integer(level)
  dimension <- as.integer(dimension)
  max_points <- as.numeric(max_points)
  if (length(level) != 1L || is.na(level)) {
    .ad_stop("`level` must be one integer.")
  }
  if (length(dimension) != 1L || is.na(dimension)) {
    .ad_stop("`dimension` must be one integer.")
  }
  if (length(max_points) != 1L || is.na(max_points)) {
    .ad_stop("`max_points` must be one number.")
  }
  .libertad_smolyak_gauss_hermite_grid(level, dimension, max_points)
}
