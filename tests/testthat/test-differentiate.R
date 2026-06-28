library(testthat)
library(LibeRtAD)

f_example <- function(x, y, z) {
  3 * x^4 + y / z / x + z * 3
}

f_unary <- function(x) {
  sin(x^2) + exp(x)
}

test_that("reverse-mode AD matches symbolic differentiation", {
  sym <- symbolicD(f_example, x = 2, y = 2, z = 3)
  r <- backdiffR(f_example, x = 2, y = 2, z = 3)
  cpp <- backdiffCPP(f_example, x = 2, y = 2, z = 3)

  expect_equal(r$value, sym$value, tolerance = 1e-10)
  expect_equal(cpp$value, sym$value, tolerance = 1e-10)
  expect_equal(r$gradient, sym$gradient, tolerance = 1e-10)
  expect_equal(cpp$gradient, sym$gradient, tolerance = 1e-10)
})

test_that("unary operations differentiate correctly", {
  sym <- symbolicD(f_unary, x = 1.5)
  r <- backdiff(f_unary, x = 1.5, backend = "R")
  cpp <- backdiff(f_unary, x = 1.5, backend = "cpp")

  expect_equal(r$gradient, sym$gradient, tolerance = 1e-10)
  expect_equal(cpp$gradient, sym$gradient, tolerance = 1e-10)
})

test_that("at argument works as an alternative to dots", {
  r1 <- backdiff(f_example, x = 2, y = 2, z = 3)
  r2 <- backdiff(f_example, at = list(x = 2, y = 2, z = 3))
  expect_equal(r1$value, r2$value)
  expect_equal(r1$gradient, r2$gradient)
  expect_equal(r1$partials, r2$partials)
})

test_that("unknown parameters are rejected", {
  expect_error(
    backdiff(f_example, x = 2, w = 1),
    "Unknown parameters: w"
  )
})

test_that("log rejects non-positive values", {
  f_log <- function(x) log(x)
  expect_error(
    backdiff(f_log, x = -1),
    "Value must be positive for log function"
  )
})

test_that("if statements differentiate through the active branch only", {
  f_pos <- function(x) {
    if (x > 0) {
      x^2
    } else {
      0
    }
  }

  r_true <- backdiff(f_pos, x = 2)
  r_false <- backdiff(f_pos, x = -1)

  expect_equal(r_true$value, 4)
  expect_equal(r_true$partials[["x"]], 4, tolerance = 1e-10)
  expect_equal(r_false$value, 0)
  expect_equal(r_false$partials[["x"]], 0, tolerance = 1e-10)
})

test_that("compound logical conditions work in if statements", {
  f <- function(x, y) {
    if (x > 0 && y > 0) {
      x * y
    } else {
      x + y
    }
  }

  r_both_pos <- backdiff(f, x = 2, y = 3)
  r_mixed <- backdiff(f, x = -2, y = 3)

  expect_equal(r_both_pos$partials[["x"]], 3, tolerance = 1e-10)
  expect_equal(r_both_pos$partials[["y"]], 2, tolerance = 1e-10)
  expect_equal(r_mixed$partials[["x"]], 1, tolerance = 1e-10)
  expect_equal(r_mixed$partials[["y"]], 1, tolerance = 1e-10)
})

test_that("ifelse differentiates through the selected branch", {
  f <- function(x) {
    ifelse(x > 0, x^2, 2 * x)
  }

  r_pos <- backdiff(f, x = 3)
  r_neg <- backdiff(f, x = -2)

  expect_equal(r_pos$partials[["x"]], 6, tolerance = 1e-10)
  expect_equal(r_neg$partials[["x"]], 2, tolerance = 1e-10)
})

test_that("comparisons work when a variable is on the right-hand side", {
  f <- function(x) {
    if (0 < x) {
      x^3
    } else {
      0
    }
  }

  r <- backdiff(f, x = 2)
  expect_equal(r$value, 8)
  expect_equal(r$partials[["x"]], 12, tolerance = 1e-10)
})

test_that("abs, sqrt, pmax, and pmin differentiate correctly", {
  f <- function(x) {
    pmax(sqrt(abs(x)), 0.5)
  }

  r <- backdiff(f, x = 4)
  fwd <- forwarddiff(f, x = 4)
  expect_equal(r$partials[["x"]], 0.25, tolerance = 1e-10)
  expect_equal(fwd$partials[["x"]], 0.25, tolerance = 1e-10)

  r_neg <- backdiff(f, x = -4)
  expect_equal(r_neg$partials[["x"]], -0.25, tolerance = 1e-10)
})

test_that("vector parameters work in reverse mode", {
  f <- function(x) {
    sum(x^2)
  }

  r <- backdiff(f, x = c(1, 2, 3))
  expect_equal(r$value, 14)
  expect_equal(unname(r$partials[["x"]]), c(2, 4, 6), tolerance = 1e-10)
})

test_that("vector parameters work in forward mode", {
  f <- function(x) {
    sum(x^2)
  }

  r <- forwarddiff(f, x = c(1, 2, 3))
  expect_equal(r$value, 14)
  expect_equal(unname(r$partials[["x"]]), c(2, 4, 6), tolerance = 1e-10)
})

test_that("vector indexing works in AD functions", {
  f <- function(x) {
    x[1]^2 + x[2]
  }

  r <- backdiff(f, x = c(3, 4))
  expect_equal(r$value, 13)
  expect_equal(unname(r$partials[["x"]]), c(6, 1), tolerance = 1e-10)
})

test_that("forward and reverse mode agree for mixed functions", {
  f <- function(x, y) {
    sum(pmax(x, y)) + sqrt(abs(x[1]))
  }

  rev <- backdiff(f, x = c(1, -2), y = c(0, 3))
  fwd <- forwarddiff(f, x = c(1, -2), y = c(0, 3))
  expect_equal(rev$partials_flat, fwd$partials_flat, tolerance = 1e-10)
})

test_that("forward mode C++ backend matches reverse mode for scalars", {
  f <- function(x, y, z) {
    3 * x^4 + y / z / x + z * 3
  }

  rev <- backdiffCPP(f, x = 2, y = 2, z = 3)
  fwd <- forwarddiffCPP(f, x = 2, y = 2, z = 3)
  expect_equal(fwd$partials_flat, rev$partials_flat, tolerance = 1e-10)
})

test_that("forward mode R and C++ backends agree for scalars", {
  f <- function(x, y) {
    sin(x * y) + sqrt(abs(x))
  }

  r <- forwarddiff(f, x = 1.5, y = 2, backend = "R")
  cpp <- forwarddiffCPP(f, x = 1.5, y = 2)
  expect_equal(cpp$partials_flat, r$partials_flat, tolerance = 1e-10)
})

test_that("mean and max reductions differentiate correctly", {
  f_mean <- function(x) mean(x^2)
  r <- backdiff(f_mean, x = c(1, 2, 3))
  expect_equal(r$value, 14 / 3, tolerance = 1e-10)
  expect_equal(unname(r$partials[["x"]]), c(2, 4, 6) / 3, tolerance = 1e-10)

  f_max <- function(x) max(x^2)
  r_max <- backdiff(f_max, x = c(1, 2, 3))
  expect_equal(r_max$value, 9)
  expect_equal(unname(r_max$partials[["x"]]), c(0, 0, 6), tolerance = 1e-10)
})

test_that("matrix multiplication differentiates correctly", {
  f <- function(A, B) {
    sum(A %*% B)
  }
  A <- matrix(c(1, 2, 3, 4), nrow = 2)
  B <- matrix(c(5, 6, 7, 8), nrow = 2)

  r <- backdiff(f, A = A, B = B)
  fwd <- forwarddiff(f, A = A, B = B)
  expect_equal(r$value, sum(A %*% B))
  expect_equal(r$partials_flat, fwd$partials_flat, tolerance = 1e-10)
})

test_that("autodiff wrapper matches backdiff and forwarddiff", {
  f <- function(x) sum(x^2)
  rev <- autodiff(f, x = c(1, 2, 3), mode = "reverse")
  fwd <- autodiff(f, x = c(1, 2, 3), mode = "forward")
  expect_equal(rev$partials_flat, backdiff(f, x = c(1, 2, 3))$partials_flat)
  expect_equal(fwd$partials_flat, forwarddiff(f, x = c(1, 2, 3))$partials_flat)
})

test_that("C++ backend supports vector parameters", {
  f <- function(x) sum(x^2) + mean(x)
  x <- c(1, 2, 3)
  rev_r <- backdiff(f, x = x, backend = "R")
  fwd_cpp <- forwarddiff(f, x = x, backend = "cpp")
  rev_cpp <- backdiff(f, x = x, backend = "cpp")
  expect_equal(rev_cpp$partials_flat, rev_r$partials_flat, tolerance = 1e-10)
  expect_equal(fwd_cpp$partials_flat, rev_r$partials_flat, tolerance = 1e-10)
})
