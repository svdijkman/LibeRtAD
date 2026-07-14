test_that("persistent tape returns exact gradient and Hessian", {
  model <- ad_compile(
    "Y = (X - 1)^2 + 3 * Z^2 + X * Z",
    inputs = c("X", "Z"), outputs = "Y",
    at = c(X = 2, Z = -1), wrt = c("X", "Z")
  )
  expect_equal(unname(model$value(c(X = 2, Z = -1))), 2)
  expect_equal(unname(model$gradient(c(X = 2, Z = -1))), c(1, -4), tolerance = 1e-12)
  expect_equal(
    unname(model$hessian(c(X = 2, Z = -1))),
    matrix(c(2, 1, 1, 6), 2, 2), tolerance = 1e-12
  )
})

test_that("program evaluation can use non-taped fixed inputs", {
  model <- ad_compile("Y = A * X + B", inputs = c("A", "X", "B"), outputs = "Y")
  expect_equal(unname(model$value(c(A = 2, X = 4, B = 1), taped = FALSE)), 9)
  model$record(c(A = 2, X = 4, B = 1), wrt = "X", outputs = "Y")
  expect_equal(unname(model$gradient(c(X = 10))), 2)
})
