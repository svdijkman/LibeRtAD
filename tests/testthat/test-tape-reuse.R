test_that("ad_tape_reuse matches fresh autodiff for simple function", {
  f <- function(x) x^2
  autodiff(f, at = list(x = 2), mode = "reverse", backend = "cpp", record_tape = TRUE)
  ad_tape_save("quad")
  g1 <- autodiff(f, at = list(x = 2.5), mode = "reverse", backend = "cpp", record_tape = TRUE)
  g2 <- ad_tape_reuse(f, at = list(x = 2.5), cache_key = "quad", backend = "cpp")
  expect_equal(g2$value, g1$value, tolerance = 1e-10)
  expect_equal(g2$partials_flat[["x"]], g1$partials_flat[["x"]], tolerance = 1e-8)
})

test_that("ad_tape_reuse works for closure functions", {
  f <- local({
    offset <- 1
    function(x) (x + offset)^2
  })
  autodiff(f, at = list(x = 2), mode = "reverse", backend = "cpp", record_tape = TRUE)
  ad_tape_save("closure")
  g1 <- autodiff(f, at = list(x = 3), mode = "reverse", backend = "cpp", record_tape = TRUE)
  g2 <- ad_tape_reuse(f, at = list(x = 3), cache_key = "closure", backend = "cpp")
  expect_equal(g2$partials_flat[["x"]], g1$partials_flat[["x"]], tolerance = 1e-8)
})
