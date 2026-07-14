test_that("IR compiles established THETA/ETA syntax", {
  ir <- ad_ir(
    "CL = THETA(1) * exp(ETA(1))\nV = THETA(2)\nK = CL / V",
    outputs = "K"
  )
  expect_s3_class(ir, "libertad_ir")
  expect_equal(ir$input_names, c("THETA_1", "ETA_1", "THETA_2"))
  expect_equal(ir$output_names, "K")
})

test_that("unsafe or unsupported control flow fails at compile time", {
  expect_error(ad_ir("if (X > 0) Y = X else Y = -X"), "ifelse")
  expect_error(ad_ir("Y = system('whoami')"), "Unsupported function")
})

test_that("ifelse is represented without R evaluation", {
  ir <- ad_ir("Y = ifelse(X <= 0, -X, X)", outputs = "Y")
  expect_true(any(vapply(ir$nodes, function(x) x$op == 21L, logical(1))))
})
