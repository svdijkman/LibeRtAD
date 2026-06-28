test_that("autodiff_hessian matches numeric for quadratic", {
  f <- function(x, y) x^2 + 2 * x * y + 3 * y^2
  h <- autodiff_hessian(f, x = 1, y = 1)
  expect_equal(h$hessian[1, 2], 2, tolerance = 0.05)
  expect_equal(h$hessian[2, 2], 6, tolerance = 0.05)
})

test_that("ad_tape_stats returns list", {
  reset_tape()
  autodiff(function(x) x^2, x = 2)
  st <- ad_tape_stats()
  expect_true(st$n_nodes >= 0L)
})
