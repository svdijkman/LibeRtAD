test_that("Gauss-Hermite grids integrate standard-normal moments", {
  rule <- ad_gauss_hermite(order = 5L, dimension = 1L)
  expect_equal(sum(rule$weights), 1, tolerance = 1e-14)
  expect_equal(sum(rule$weights * rule$nodes[, 1L]), 0, tolerance = 1e-14)
  expect_equal(sum(rule$weights * rule$nodes[, 1L]^2), 1, tolerance = 1e-13)
  expect_equal(sum(rule$weights * rule$nodes[, 1L]^4), 3, tolerance = 1e-12)

  grid <- ad_gauss_hermite(order = 3L, dimension = 2L)
  expect_equal(dim(grid$nodes), c(9L, 2L))
  expect_equal(sum(exp(grid$log_weights)), 1, tolerance = 1e-14)
  expect_equal(
    colSums(grid$nodes^2 * grid$weights), c(1, 1), tolerance = 1e-13
  )
})

test_that("Gauss-Hermite grids guard exponential allocations", {
  expect_error(
    ad_gauss_hermite(order = 7L, dimension = 6L, max_points = 100000L),
    "exceeds `max_points`"
  )
  empty <- ad_gauss_hermite(order = 5L, dimension = 0L)
  expect_equal(dim(empty$nodes), c(1L, 0L))
  expect_equal(empty$weights, 1)
})

test_that("Smolyak Gauss-Hermite grids integrate multivariate moments", {
  sparse <- ad_smolyak_gauss_hermite(
    level = 3L, dimension = 4L, max_points = 10000L
  )
  expect_identical(sparse$grid, "smolyak")
  expect_lt(sparse$points, 5^4)
  expect_equal(sum(sparse$weights), 1, tolerance = 1e-13)
  expect_equal(
    colSums(sparse$nodes^2 * sparse$weights), rep(1, 4L),
    tolerance = 1e-12
  )
  expect_equal(
    sum(sparse$weights * sparse$nodes[, 1L]^2 * sparse$nodes[, 2L]^2),
    1, tolerance = 1e-12
  )
  expect_true(any(sparse$weights < 0))
  expect_equal(
    sparse$weights,
    sparse$signs * exp(sparse$log_abs_weights),
    tolerance = 1e-15
  )
})

test_that("one-dimensional Smolyak rules equal their tensor rules", {
  sparse <- ad_smolyak_gauss_hermite(level = 4L, dimension = 1L)
  tensor <- ad_gauss_hermite(order = 7L, dimension = 1L)
  expect_equal(sparse$nodes, tensor$nodes, tolerance = 1e-14)
  expect_equal(sparse$weights, tensor$weights, tolerance = 1e-14)

  origin <- ad_smolyak_gauss_hermite(level = 1L, dimension = 8L)
  expect_equal(dim(origin$nodes), c(1L, 8L))
  expect_equal(origin$nodes, matrix(0, 1L, 8L))
  expect_equal(origin$weights, 1)
  expect_error(
    ad_smolyak_gauss_hermite(level = 5L, dimension = 8L, max_points = 10L),
    "max_points|intermediate work limit"
  )
})
