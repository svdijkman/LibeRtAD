#' @keywords internal
.ad_forward_rule <- function(node, t1, t2, p1, p2) {
  op <- node$op
  if (op == "+") {
    return(t1 + t2)
  }
  if (op == "-") {
    return(t1 - t2)
  }
  if (op == "*") {
    return(t1 * p2$value + t2 * p1$value)
  }
  if (op == "/") {
    return(t1 / p2$value - t2 * p1$value / (p2$value^2))
  }
  if (op == "^") {
    out <- t1 * p2$value * p1$value^(p2$value - 1)
    if (!is_constant(p2)) {
      out <- out + t2 * p1$value^p2$value * log(p1$value)
    }
    return(out)
  }
  if (op == "sin") {
    return(t1 * cos(p1$value))
  }
  if (op == "cos") {
    return(-t1 * sin(p1$value))
  }
  if (op == "exp") {
    return(t1 * exp(p1$value))
  }
  if (op == "log") {
    return(t1 / p1$value)
  }
  if (op == "abs") {
    return(t1 * .ad_sign(p1$value))
  }
  if (op == "sqrt") {
    return(t1 / (2 * sqrt(p1$value)))
  }
  if (op == "sum") {
    return(sum(t1))
  }
  if (op == "mean") {
    return(sum(t1) / .ad_node_len(p1$value))
  }
  if (op == "max") {
    idx <- node$meta$index
    return(t1[idx])
  }
  if (op == "[") {
    idx <- node$meta$index
    return(t1[idx])
  }
  if (op == "pmax") {
    branch <- node$meta$branch
    out <- .ad_zeros(node$value)
    if (.ad_node_len(node$value) == 1L) {
      if (branch == 1L) {
        out <- t1
      } else {
        out <- t2
      }
    } else {
      out[branch == 1L] <- t1[branch == 1L]
      out[branch == 2L] <- t2[branch == 2L]
    }
    return(out)
  }
  if (op == "pmin") {
    branch <- node$meta$branch
    out <- .ad_zeros(node$value)
    if (.ad_node_len(node$value) == 1L) {
      if (branch == 1L) {
        out <- t1
      } else {
        out <- t2
      }
    } else {
      out[branch == 1L] <- t1[branch == 1L]
      out[branch == 2L] <- t2[branch == 2L]
    }
    return(out)
  }
  if (op == "%*%") {
    return(t1 %*% p2$value + p1$value %*% t2)
  }
  stop("Unsupported operation: ", op, call. = FALSE)
}

#' @keywords internal
.ad_forward_propagate <- function(node, cache = new.env(parent = emptyenv())) {
  key <- .ad_node_key(node)
  if (exists(key, envir = cache, inherits = FALSE)) {
    return(get(key, envir = cache))
  }

  if (is_constant(node)) {
    tan <- .ad_zeros(node$value)
    assign(key, tan, envir = cache)
    return(tan)
  }

  if (node$op == "") {
    return(node$tangent)
  }

  p1 <- node$parents[[1]]
  t1 <- .ad_forward_propagate(p1, cache)
  p2 <- if (length(node$parents) >= 2) node$parents[[2]] else NULL
  t2 <- if (!is.null(p2)) .ad_forward_propagate(p2, cache) else NULL
  tan <- .ad_forward_rule(node, t1, t2, p1, p2)
  node$tangent <- tan
  assign(key, tan, envir = cache)
  tan
}

#' @keywords internal
.ad_run_forward <- function(result, parameters, backend = c("R", "cpp")) {
  backend <- match.arg(backend)
  if (.ad_node_len(result$value) != 1L) {
    stop("Forward mode requires a scalar output.", call. = FALSE)
  }

  dofs <- .ad_parameter_dofs(parameters)
  partial_values <- numeric(length(dofs))
  partial_names <- character(length(dofs))

  for (i in seq_along(dofs)) {
    result$tangent <- 0
    for (node in .ad_state$tape) {
      node$tangent <- .ad_zeros(node$value)
    }
    .ad_reset_tangents(parameters)
    .ad_seed_tangent(parameters, dofs[[i]])
    partial_values[i] <- .ad_forward_propagate(result)
    dof <- dofs[[i]]
    partial_names[i] <- if (is.null(dof$index)) {
      dof$name
    } else {
      paste0(dof$name, "[", dof$index, "]")
    }
  }

  stats::setNames(partial_values, partial_names)
}
