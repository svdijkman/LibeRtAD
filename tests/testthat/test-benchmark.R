test_that("native benchmark separates tape work and checks derivatives", {
  result <- ad_benchmark("jacobian", iterations = 5L, warmups = 0L)
  expect_s3_class(result, "ad_benchmark_result")
  expect_true(all(c("record tape", "value", "jacobian") %in% result$timings$operation))
  expect_true(any(result$timings$backend == "LibeRtAD C++ tape"))
  expect_true(all(is.finite(result$timings$microseconds_per_call)))
  expect_lt(max(result$accuracy$max_absolute_difference), 1e-5)
})

test_that("benchmark catalogue and GUI payload expose reproducibility metadata", {
  cases <- ad_benchmark_cases()
  expect_true(all(c("rosenbrock", "pk", "jacobian") %in% cases$id))
  payload <- getFromNamespace(".ad_gui_payload", "LibeRtAD")()
  expect_true(length(payload$cases) >= 3L)
  expect_identical(payload$engineNamed$persistent_tape, TRUE)
  expect_true(nzchar(payload$defaultOutput))
  expect_false(grepl("documents[/\\\\]+documents", payload$defaultOutput, ignore.case = TRUE))
})

test_that("ecosystem result collection tolerates empty optional CSV files", {
  output <- tempfile("libertad-benchmark-")
  dir.create(output)
  utils::write.csv(data.frame(
    engine = "LibeRation", workload = "estimation", method = "FO",
    median_end_to_end_seconds = 1, median_core_seconds = 0.5
  ), file.path(output, "summary.csv"), row.names = FALSE)
  writeLines("", file.path(output, "paired-timing-comparison.csv"))
  result <- getFromNamespace(".ad_gui_ecosystem_result", "LibeRtAD")(output, 0L)
  expect_length(result$summary, 1L)
  expect_length(result$paired, 0L)
  expect_identical(result$exitStatus, 0L)
})
