library(LibeRtAD)

f <- function(x, y, z) {
  3 * x^4 + y / z / x + z * 3
}

backdiff(f, x = 2, y = 2, z = 3)

f_vec <- function(x) {
  sum(sqrt(abs(x)))
}

autodiff(f_vec, x = c(1, 4, 9), mode = "forward", backend = "cpp")

f_mat <- function(A, B) {
  sum(A %*% B)
}

autodiff(
  f_mat,
  A = matrix(c(1, 2, 3, 4), nrow = 2),
  B = matrix(c(5, 6, 7, 8), nrow = 2),
  mode = "reverse"
)

f_piecewise <- function(x) {
  pmax(x^2, 2 * x)
}

backdiff(f_piecewise, x = 3)