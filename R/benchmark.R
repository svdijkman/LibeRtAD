.ad_benchmark_cases <- function() {
  list(
    rosenbrock = list(
      label = "Rosenbrock curvature",
      description = "A nonlinear scalar objective with a narrow curved valley.",
      code = "Y = (1 - X)^2 + 100 * (Z - X^2)^2",
      inputs = c("X", "Z"), outputs = "Y",
      record_at = c(X = -1.2, Z = 1), wrt = c("X", "Z"),
      evaluate_at = c(X = -0.9, Z = 0.8),
      fun = function(x) c(Y = (1 - x[[1L]])^2 + 100 * (x[[2L]] - x[[1L]]^2)^2)
    ),
    pk = list(
      label = "One-compartment PK objective",
      description = "Exponential PK prediction differentiated with respect to clearance and volume.",
      code = paste(
        "K = CL / V",
        "C = DOSE / V * exp(-K * TIME)",
        "Y = log(C)^2 + C",
        sep = "\n"
      ),
      inputs = c("CL", "V", "DOSE", "TIME"), outputs = "Y",
      record_at = c(CL = 5, V = 50, DOSE = 500, TIME = 8),
      wrt = c("CL", "V"), evaluate_at = c(CL = 5.5, V = 48),
      fun = function(x) {
        concentration <- 500 / x[[2L]] * exp(-(x[[1L]] / x[[2L]]) * 8)
        c(Y = log(concentration)^2 + concentration)
      }
    ),
    jacobian = list(
      label = "Multi-output Jacobian",
      description = "Two coupled nonlinear outputs sharing the same active inputs.",
      code = "Y1 = X * exp(Z)\nY2 = sin(X) + Z^2",
      inputs = c("X", "Z"), outputs = c("Y1", "Y2"),
      record_at = c(X = 1.2, Z = 0.4), wrt = c("X", "Z"),
      evaluate_at = c(X = 1.1, Z = 0.35),
      fun = function(x) c(Y1 = x[[1L]] * exp(x[[2L]]), Y2 = sin(x[[1L]]) + x[[2L]]^2)
    )
  )
}

#' List the built-in LibeRtAD benchmark cases
#' @return A data frame describing the available differentiable workloads.
#' @export
ad_benchmark_cases <- function() {
  cases <- .ad_benchmark_cases()
  data.frame(
    id = names(cases),
    label = vapply(cases, `[[`, character(1), "label"),
    description = vapply(cases, `[[`, character(1), "description"),
    stringsAsFactors = FALSE
  )
}

.ad_benchmark_time <- function(fun, iterations, warmups) {
  for (index in seq_len(warmups)) fun()
  measured_iterations <- as.integer(iterations)
  maximum_iterations <- max(measured_iterations, 1L) * 1024L
  repeat {
    started <- proc.time()[["elapsed"]]
    for (index in seq_len(measured_iterations)) fun()
    elapsed <- proc.time()[["elapsed"]] - started
    if (elapsed >= 0.08 || measured_iterations >= maximum_iterations) break
    measured_iterations <- min(maximum_iterations, measured_iterations * 2L)
  }
  elapsed <- max(elapsed, .Machine$double.eps)
  c(iterations = measured_iterations, total_seconds = elapsed,
    microseconds_per_call = elapsed * 1e6 / measured_iterations,
    calls_per_second = measured_iterations / elapsed)
}

.ad_fd_jacobian <- function(fun, x, step = sqrt(.Machine$double.eps)) {
  x <- as.numeric(x)
  base <- as.numeric(fun(x))
  output <- matrix(NA_real_, length(base), length(x))
  for (column in seq_along(x)) {
    delta <- step * max(1, abs(x[[column]]))
    upper <- lower <- x
    upper[[column]] <- upper[[column]] + delta
    lower[[column]] <- lower[[column]] - delta
    output[, column] <- (as.numeric(fun(upper)) - as.numeric(fun(lower))) / (2 * delta)
  }
  output
}

.ad_fd_hessian <- function(fun, x, step = .Machine$double.eps^(1 / 4)) {
  x <- as.numeric(x)
  output <- matrix(NA_real_, length(x), length(x))
  for (column in seq_along(x)) {
    delta <- step * max(1, abs(x[[column]]))
    upper <- lower <- x
    upper[[column]] <- upper[[column]] + delta
    lower[[column]] <- lower[[column]] - delta
    output[, column] <- drop(
      .ad_fd_jacobian(fun, upper) - .ad_fd_jacobian(fun, lower)
    ) / (2 * delta)
  }
  (output + t(output)) / 2
}

.ad_benchmark_row <- function(operation, backend, iterations, measured) {
  measured_iterations <- measured[["iterations"]]
  if (is.null(measured_iterations)) measured_iterations <- iterations
  data.frame(
    operation = operation, backend = backend,
    iterations = as.integer(unname(measured_iterations)),
    total_seconds = unname(measured[["total_seconds"]]),
    microseconds_per_call = unname(measured[["microseconds_per_call"]]),
    calls_per_second = unname(measured[["calls_per_second"]]),
    stringsAsFactors = FALSE
  )
}

#' Benchmark the persistent C++ automatic-differentiation engine
#'
#' The benchmark separates recording from repeated tape evaluation and compares
#' exact CppAD derivatives with central finite differences evaluated in R. It
#' is intended for local regression and hardware comparison, not as a universal
#' performance claim.
#'
#' @param case Built-in workload returned by [ad_benchmark_cases()].
#' @param iterations Number of measured calls for value and derivative timing.
#' @param warmups Number of calls before timing.
#' @param optimize Whether CppAD tape optimisation is enabled.
#' @param finite_difference Include R central finite-difference comparators.
#' @return An `ad_benchmark_result` with timings, accuracy checks, engine
#'   metadata, and reproducibility settings.
#' @export
ad_benchmark <- function(case = c("rosenbrock", "pk", "jacobian"),
                         iterations = 1000L, warmups = 50L,
                         optimize = TRUE, finite_difference = TRUE) {
  case <- match.arg(case)
  iterations <- as.integer(iterations)
  warmups <- as.integer(warmups)
  if (is.na(iterations) || iterations < 1L) .ad_stop("`iterations` must be positive.")
  if (is.na(warmups) || warmups < 0L) .ad_stop("`warmups` must be non-negative.")
  definition <- .ad_benchmark_cases()[[case]]
  factory <- function() ad_compile(
    definition$code, inputs = definition$inputs, outputs = definition$outputs,
    at = definition$record_at, wrt = definition$wrt, optimize = isTRUE(optimize)
  )

  compile_iterations <- min(25L, max(3L, as.integer(ceiling(iterations / 100))))
  compile_time <- .ad_benchmark_time(factory, compile_iterations, min(warmups, 2L))
  model <- factory()
  point <- definition$evaluate_at
  r_point <- unname(point)
  rows <- list(.ad_benchmark_row("record tape", "LibeRtAD C++", compile_iterations, compile_time))
  rows[[length(rows) + 1L]] <- .ad_benchmark_row(
    "value", "LibeRtAD C++ tape", iterations,
    .ad_benchmark_time(function() model$value(point), iterations, warmups)
  )
  rows[[length(rows) + 1L]] <- .ad_benchmark_row(
    "value", "R expression", iterations,
    .ad_benchmark_time(function() definition$fun(r_point), iterations, warmups)
  )

  ad_jacobian <- model$jacobian(point)
  operation <- if (length(definition$outputs) == 1L) "gradient" else "jacobian"
  rows[[length(rows) + 1L]] <- .ad_benchmark_row(
    operation, "LibeRtAD C++ tape", iterations,
    .ad_benchmark_time(function() model$jacobian(point), iterations, warmups)
  )
  fd_jacobian <- .ad_fd_jacobian(definition$fun, r_point)
  if (isTRUE(finite_difference)) {
    fd_iterations <- max(10L, min(iterations, 250L))
    rows[[length(rows) + 1L]] <- .ad_benchmark_row(
      operation, "R central difference", fd_iterations,
      .ad_benchmark_time(function() .ad_fd_jacobian(definition$fun, r_point),
                         fd_iterations, min(warmups, 10L))
    )
  }

  accuracy <- data.frame(
    check = c("value", operation),
    max_absolute_difference = c(
      max(abs(as.numeric(model$value(point)) - as.numeric(definition$fun(r_point)))),
      max(abs(ad_jacobian - fd_jacobian))
    ),
    reference = c("R expression", "R central difference"),
    stringsAsFactors = FALSE
  )
  if (length(definition$outputs) == 1L) {
    ad_hessian <- model$hessian(point)
    fd_hessian <- .ad_fd_hessian(definition$fun, r_point)
    rows[[length(rows) + 1L]] <- .ad_benchmark_row(
      "hessian", "LibeRtAD C++ tape", iterations,
      .ad_benchmark_time(function() model$hessian(point), iterations, warmups)
    )
    if (isTRUE(finite_difference)) {
      fd_iterations <- max(5L, min(iterations, 100L))
      rows[[length(rows) + 1L]] <- .ad_benchmark_row(
        "hessian", "R central difference", fd_iterations,
        .ad_benchmark_time(function() .ad_fd_hessian(definition$fun, r_point),
                           fd_iterations, min(warmups, 5L))
      )
    }
    accuracy <- rbind(accuracy, data.frame(
      check = "hessian", max_absolute_difference = max(abs(ad_hessian - fd_hessian)),
      reference = "R central difference", stringsAsFactors = FALSE
    ))
  }
  structure(list(
    case = case, label = definition$label, description = definition$description,
    timings = do.call(rbind, rows), accuracy = accuracy,
    engine = ad_engine_info(),
    settings = list(iterations = iterations, warmups = warmups,
                    optimize = isTRUE(optimize), finite_difference = isTRUE(finite_difference)),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ), class = "ad_benchmark_result")
}

#' @export
print.ad_benchmark_result <- function(x, ...) {
  cat("LibeRtAD benchmark:", x$label, "\n")
  print(x$timings, row.names = FALSE)
  cat("Maximum derivative difference:",
      format(max(x$accuracy$max_absolute_difference), digits = 4), "\n")
  invisible(x)
}
