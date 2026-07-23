test_that("random cubic tapes preserve analytic values and derivatives", {
  model <- ad_compile(
    "Y=A*X^3+B*X^2+C*X+D",
    inputs = c("X", "A", "B", "C", "D"), outputs = "Y",
    at = c(X = 0, A = 1, B = 1, C = 1, D = 1), wrt = "X"
  )
  set.seed(20260723)
  for (index in seq_len(100L)) {
    coefficient <- stats::runif(4L, -5, 5)
    names(coefficient) <- c("A", "B", "C", "D")
    x <- stats::runif(1L, -3, 3)
    model$set_dynamic(coefficient)
    expected <- coefficient[["A"]] * x^3 + coefficient[["B"]] * x^2 +
      coefficient[["C"]] * x + coefficient[["D"]]
    derivative <- 3 * coefficient[["A"]] * x^2 +
      2 * coefficient[["B"]] * x + coefficient[["C"]]
    expect_equal(unname(model$value(c(X = x))), expected, tolerance = 1e-11)
    expect_equal(unname(model$gradient(c(X = x))), derivative, tolerance = 1e-10)
  }
})

test_that("conditional-expression tapes remain valid across random branches", {
  model <- ad_compile(
    "Y=ifelse(X<SWITCH,A*X,B*X)",
    inputs = c("X", "SWITCH", "A", "B"), outputs = "Y",
    at = c(X = -1, SWITCH = 0, A = 2, B = 3), wrt = "X"
  )
  set.seed(20260724)
  for (index in seq_len(50L)) {
    dynamic <- c(SWITCH = stats::runif(1L, -1, 1),
                 A = stats::runif(1L, 0.1, 4), B = stats::runif(1L, 0.1, 4))
    x <- stats::runif(1L, -2, 2)
    model$set_dynamic(dynamic)
    slope <- if (x < dynamic[["SWITCH"]]) dynamic[["A"]] else dynamic[["B"]]
    expect_equal(unname(model$value(c(X = x))), slope * x, tolerance = 1e-12)
    expect_equal(unname(model$gradient(c(X = x))), slope, tolerance = 1e-12)
  }
})
