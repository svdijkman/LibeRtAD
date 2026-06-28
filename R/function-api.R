#' @keywords internal
.ad_function_expr <- function(f) {
  b <- body(f)
  if (is.call(b) && identical(b[[1L]], as.name("{"))) {
    stmts <- as.list(b)[-1L]
    if (length(stmts) == 0L) {
      stop("Empty function body.", call. = FALSE)
    }
    if (length(stmts) == 1L) {
      return(stmts[[1L]])
    }
    # Multi-statement: wrap as nested ifelse chain returning last assignment or value
    last <- stmts[[length(stmts)]]
    if (is.call(last) && identical(last[[1L]], as.name("<-"))) {
      return(last[[3L]])
    }
    return(last)
  }
  b
}

#' @keywords internal
.ad_parse_at <- function(at, ...) {
  dots <- list(...)
  if (length(dots) > 0L && !is.null(at)) {
    stop("Supply either `at` or named values in `...`, not both.", call. = FALSE)
  }
  if (length(dots) > 0L) {
    at <- dots
  }
  if (is.null(at) || length(at) == 0L) {
    stop(
      "Provide parameter values, e.g. backdiff(f, x = 2, y = 3).",
      call. = FALSE
    )
  }
  if (is.null(names(at)) || any(names(at) == "")) {
    stop("All parameter values must be named.", call. = FALSE)
  }
  at
}

#' @keywords internal
.ad_check_formals <- function(f, at) {
  if (!is.function(f)) {
    stop("`f` must be a function.", call. = FALSE)
  }
  fnames <- names(formals(f))
  fnames <- fnames[fnames != "..."]
  unknown <- setdiff(names(at), fnames)
  if (length(unknown) > 0L) {
    stop(
      "Unknown parameters: ", paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(at)
}

#' @keywords internal
.ad_values_to_parameters <- function(at) {
  lapply(names(at), function(nm) {
    val <- at[[nm]]
    if (!is.numeric(val) || any(is.na(val))) {
      stop(
        "Parameter `", nm, "` must be numeric without missing values.",
        call. = FALSE
      )
    }
    newVariable(name = nm, value = val, par = TRUE)
  })
}

#' @keywords internal
.ad_bind_formal_defaults <- function(f, env) {
  fmls <- formals(f)
  if (is.null(fmls)) {
    return(invisible(NULL))
  }
  for (nm in names(fmls)) {
    if (nm == "...") {
      next
    }
    if (exists(nm, envir = env, inherits = FALSE)) {
      next
    }
    default_val <- eval(fmls[[nm]], envir = baseenv())
    assign(
      nm,
      newConstant(name = paste0("const_", nm), value = default_val),
      envir = env
    )
  }
  invisible(NULL)
}

#' @keywords internal
.ad_as_ad_node <- function(x) {
  if (.ad_is_ad(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    return(newConstant(name = paste0("const_", paste(x, collapse = "_")), value = x))
  }
  stop("Function must return a numeric scalar or AD node.", call. = FALSE)
}

#' @keywords internal
.ad_bind_math_ops <- function(env) {
  bind_unary <- function(nm) {
    base_fn <- match.fun(nm)
    assign(nm, function(x) {
      if (.ad_is_ad(x)) {
        .ad_dispatch(nm, x)
      } else {
        base_fn(x)
      }
    }, envir = env)
  }

  bind_binary <- function(nm) {
    base_fn <- match.fun(nm)
    assign(nm, function(x, y) {
      if (.ad_is_ad(x) || .ad_is_ad(y)) {
        .ad_dispatch(nm, x, y)
      } else {
        base_fn(x, y)
      }
    }, envir = env)
  }

  for (nm in c("sin", "cos", "exp", "log", "abs", "sqrt")) {
    bind_unary(nm)
  }
  assign("-", function(e1, e2) {
    if (missing(e2)) {
      if (.ad_is_ad(e1)) {
        .ad_dispatch("neg", e1)
      } else {
        `-`(e1)
      }
    } else if (.ad_is_ad(e1) || .ad_is_ad(e2)) {
      .ad_dispatch("-", e1, e2)
    } else {
      e1 - e2
    }
  }, envir = env)
  for (nm in c("+", "*", "/", "^")) {
    bind_binary(nm)
  }
  for (nm in c("pmax", "pmin")) {
    bind_binary(nm)
  }

  assign("sum", function(x, ...) {
    if (.ad_is_ad(x)) {
      .ad_dispatch("sum", x)
    } else {
      base::sum(x, ...)
    }
  }, envir = env)

  assign("mean", function(x, ...) {
    if (.ad_is_ad(x)) {
      .ad_dispatch("mean", x)
    } else {
      base::mean(x, ...)
    }
  }, envir = env)

  assign("max", function(x, ...) {
    if (.ad_is_ad(x)) {
      .ad_dispatch("max", x)
    } else {
      base::max(x, ...)
    }
  }, envir = env)

  assign("%*%", function(x, y) {
    if (.ad_is_ad(x) || .ad_is_ad(y)) {
      .ad_dispatch("%*%", x, y)
    } else {
      base::`%*%`(x, y)
    }
  }, envir = env)

  assign("[", function(x, i, ...) {
    if (.ad_is_ad(x)) {
      .ad_subset(x, i)
    } else {
      base::`[`(x, i, ...)
    }
  }, envir = env)

  invisible(NULL)
}

#' @keywords internal
.ad_make_eval_env <- function(parameters) {
  env <- new.env(parent = baseenv())
  for (p in parameters) {
    assign(p$name, p, envir = env)
  }
  .ad_bind_math_ops(env)
  .ad_bind_control_ops(env)
  env
}

#' @keywords internal
.ad_make_value_env <- function(parameters) {
  vals <- lapply(parameters, function(p) p$value)
  nms <- vapply(parameters, function(p) p$name, character(1))
  list2env(setNames(vals, nms), parent = baseenv())
}

#' @keywords internal
.ad_eval_function <- function(f, parameters) {
  env <- .ad_make_eval_env(parameters)
  .ad_bind_formal_defaults(f, env)
  result <- .ad_as_ad_node(eval(body(f), envir = env))
  if (.ad_node_len(result$value) != 1L) {
    stop("Differentiation requires a scalar function output.", call. = FALSE)
  }
  result
}

#' @keywords internal
.ad_collect_result <- function(result, parameters, partials_flat) {
  partials <- list()
  for (p in parameters) {
    if (.ad_node_len(p$value) == 1L) {
      partials[[p$name]] <- unname(partials_flat[p$name])
    } else {
      nm <- paste0(p$name, "[", seq_along(p$value), "]")
      partials[[p$name]] <- unname(partials_flat[nm])
    }
  }

  list(
    value = result$value,
    gradient = sum(partials_flat),
    partials = partials,
    partials_flat = partials_flat,
    node = result
  )
}

#' @keywords internal
.ad_collect_reverse_partials <- function(parameters) {
  flat <- unlist(lapply(parameters, function(p) {
    if (.ad_node_len(p$value) == 1L) {
      stats::setNames(p$grad, p$name)
    } else {
      stats::setNames(p$grad, paste0(p$name, "[", seq_along(p$grad), "]"))
    }
  }))
  flat
}

#' @keywords internal
.ad_autodiff <- function(f, at, mode, backend, record_tape, print_result) {
  .ad_check_formals(f, at)

  reset_tape()
  if (record_tape) {
    .ad_state$active <- TRUE
  }
  set_ops(backend)
  parameters <- .ad_values_to_parameters(at)

  .ad_reset_gradients(parameters)
  .ad_reset_tangents(parameters)
  result <- .ad_eval_function(f, parameters)

  if (mode == "reverse") {
    .ad_run_reverse(result, backend)
    partials_flat <- .ad_collect_reverse_partials(parameters)
  } else {
    partials_flat <- .ad_run_forward(result, parameters, backend = backend)
  }

  out <- .ad_collect_result(result, parameters, partials_flat)
  if (print_result) {
    print(out)
  }
  out
}
