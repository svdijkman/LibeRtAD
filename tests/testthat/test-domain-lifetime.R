test_that("IEEE domain failures and nonsmooth conventions are explicit", {
  logarithm <- ad_compile(
    "Y = log(X)", inputs = "X", outputs = "Y",
    at = c(X = 1), wrt = "X"
  )
  square_root <- ad_compile(
    "Y = sqrt(X)", inputs = "X", outputs = "Y",
    at = c(X = 1), wrt = "X"
  )
  reciprocal <- ad_compile(
    "Y = 1 / X", inputs = "X", outputs = "Y",
    at = c(X = 1), wrt = "X"
  )
  absolute <- ad_compile(
    "Y = abs(X)", inputs = "X", outputs = "Y",
    at = c(X = 0), wrt = "X"
  )

  expect_true(is.nan(unname(logarithm$value(c(X = -1)))))
  expect_true(is.nan(unname(square_root$value(c(X = -1)))))
  expect_true(is.infinite(unname(reciprocal$value(c(X = 0)))))
  expect_true(is.infinite(unname(reciprocal$gradient(c(X = 0)))))

  # CppAD's documented sign-at-zero convention selects the zero subgradient.
  expect_equal(unname(absolute$value(c(X = 0))), 0)
  expect_equal(unname(absolute$gradient(c(X = 0))), 0)
})

test_that("external-pointer finalizers release repeated tape allocations", {
  gc()
  baseline <- ad_allocator_info(release_available = TRUE)$inuse_bytes
  models <- lapply(seq_len(100), function(index) {
    ad_compile(
      "Y = exp(X) + X^2", inputs = "X", outputs = "Y",
      at = c(X = index / 100), wrt = "X"
    )
  })
  expect_gt(ad_allocator_info()$inuse_bytes, baseline)
  rm(models)
  gc()
  gc()
  final <- ad_allocator_info(release_available = TRUE)

  # A small allowance covers process-level CppAD bookkeeping first touched by
  # the stress loop; live model tapes themselves must not accumulate.
  expect_lte(final$inuse_bytes, baseline + 65536)
  expect_false(final$in_parallel)
  expect_equal(final$thread, 0)
})
