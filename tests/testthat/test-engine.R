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

test_that("non-active inputs are reusable CppAD dynamic parameters", {
  model <- ad_compile(
    "Y = X^P + P^X",
    inputs = c("X", "P"), outputs = "Y",
    at = c(X = 2, P = 0), wrt = "X"
  )

  expect_identical(model$dynamic, "P")
  expect_equal(model$tape_info()$dynamic_independent, 1)

  model$set_dynamic(c(P = 3))
  expect_equal(unname(model$value(c(X = 2))), 17, tolerance = 1e-12)
  expect_equal(
    unname(model$gradient(c(X = 2))),
    12 + 9 * log(3), tolerance = 1e-11
  )
  expect_equal(
    unname(model$hessian(c(X = 2))),
    matrix(12 + 9 * log(3)^2, 1, 1), tolerance = 1e-10
  )

  # Named full points can update dynamics and evaluate active inputs in one call.
  expect_equal(unname(model$value(c(X = 2, P = 4))), 32, tolerance = 1e-12)
  expect_equal(unname(model$dynamic_values), 4)
})

test_that("Jacobian evaluation selects multi-direction and subgraph kernels", {
  dense_code <- paste0(
    "Y", seq_len(20), " = X", seq_len(20), "^2 + X1"
  )
  dense <- ad_compile(
    paste(dense_code, collapse = "\n"),
    inputs = paste0("X", seq_len(20)),
    at = setNames(seq_len(20) / 10, paste0("X", seq_len(20))),
    wrt = paste0("X", seq_len(20))
  )
  dense$jacobian(setNames(seq_len(20) / 5, paste0("X", seq_len(20))))
  expect_identical(dense$tape_info()$derivative_strategy, "multi-forward")

  input_names <- paste0("X", seq_len(32))
  sparse_code <- paste0(
    "Y", seq_len(128), " = ", input_names[(seq_len(128) - 1L) %% 32L + 1L],
    "^2"
  )
  sparse <- ad_compile(
    paste(sparse_code, collapse = "\n"), inputs = input_names,
    at = setNames(rep(2, 32), input_names), wrt = input_names
  )
  jacobian <- sparse$jacobian(setNames(rep(3, 32), input_names))
  expect_identical(sparse$tape_info()$derivative_strategy, "subgraph-reverse")
  expect_equal(sparse$tape_info()$jacobian_nonzeros, 128)
  expect_equal(sum(jacobian != 0), 128)
})

test_that("optimized CppAD graph caches reconstruct without retaping", {
  model <- ad_compile(
    "Y = X^P + P^X", inputs = c("X", "P"), outputs = "Y",
    at = c(X = 2, P = 0), wrt = "X"
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  model$save_tape(path)
  restored <- ad_load_tape(path)
  restored$set_dynamic(c(P = 3))
  expect_equal(restored$value(c(X = 2)), c(Y = 17), tolerance = 1e-12)
  expect_equal(restored$gradient(c(X = 2)), 12 + 9 * log(3),
               tolerance = 1e-11, ignore_attr = TRUE)
})

test_that("checkpoint prototypes are exact and nested-AD safe", {
  probe <- ad_checkpoint_probe(repetitions = 8L, evaluations = 5L)
  for (case in probe[c("advan1", "matrix2")]) {
    expect_true(case$nested_ad_safe)
    expect_lt(case$max_value_difference, 1e-12)
    expect_lt(case$max_jacobian_difference, 1e-10)
    expect_true(is.finite(case$checkpoint_microseconds))
    expect_true(is.finite(case$direct_microseconds))
    expect_gt(case$checkpoint_operations, 0)
  }
})

test_that("engine reports the bundled CppAD provenance", {
  info <- ad_engine_info()

  expect_identical(info$backend, "CppAD (bundled by LibeRtAD)")
  expect_identical(info$cppad_version, "cppad-20260000.0")
  expect_identical(
    info$cppad_source_commit,
    "5d51b2aa6d6874c8d561da298a90b3721550d45d"
  )
  expect_identical(info$eigen_version, "3.4.0")
  expect_identical(
    info$eigen_source_commit,
    "3147391d946bb4b6c68edd901f2add6ac1f31f8c"
  )
})
