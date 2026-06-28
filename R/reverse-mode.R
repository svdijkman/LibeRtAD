#' @keywords internal
.ad_reverse_rule <- function(variable, grad) {
  p1 <- variable$parents[[1]]
  p2 <- if (length(variable$parents) >= 2) variable$parents[[2]] else NULL

  if (variable$op == "+") {
    if (!is_constant(p1)) p1$setGrad(grad)
    if (!is.null(p2) && !is_constant(p2)) p2$setGrad(grad)
  } else if (variable$op == "-") {
    if (!is_constant(p1)) p1$setGrad(grad)
    if (!is.null(p2) && !is_constant(p2)) p2$setGrad(-grad)
  } else if (variable$op == "*") {
    if (!is_constant(p1)) p1$setGrad(grad * p2$value)
    if (!is.null(p2) && !is_constant(p2)) p2$setGrad(grad * p1$value)
  } else if (variable$op == "/") {
    if (!is_constant(p1)) p1$setGrad(grad / p2$value)
    if (!is.null(p2) && !is_constant(p2)) {
      p2$setGrad(-grad * p1$value / (p2$value^2))
    }
  } else if (variable$op == "^") {
    if (!is_constant(p1)) {
      p1$setGrad(grad * p2$value * p1$value^(p2$value - 1))
    }
    if (!is.null(p2) && !is_constant(p2)) {
      p2$setGrad(grad * p1$value^p2$value * log(p1$value))
    }
  } else if (variable$op == "neg") {
    if (!is_constant(p1)) p1$setGrad(-grad)
  } else if (variable$op == "sin") {
    if (!is_constant(p1)) p1$setGrad(grad * cos(p1$value))
  } else if (variable$op == "cos") {
    if (!is_constant(p1)) p1$setGrad(grad * -sin(p1$value))
  } else if (variable$op == "exp") {
    if (!is_constant(p1)) p1$setGrad(grad * exp(p1$value))
  } else if (variable$op == "log") {
    if (!is_constant(p1)) p1$setGrad(grad / p1$value)
  } else if (variable$op == "abs") {
    if (!is_constant(p1)) p1$setGrad(grad * .ad_sign(p1$value))
  } else if (variable$op == "sqrt") {
    if (!is_constant(p1)) p1$setGrad(grad / (2 * sqrt(p1$value)))
  } else if (variable$op == "sum") {
    if (!is_constant(p1)) {
      p1$setGrad(rep(grad, .ad_node_len(p1$value)))
    }
  } else if (variable$op == "mean") {
    if (!is_constant(p1)) {
      n <- .ad_node_len(p1$value)
      p1$setGrad(rep(grad / n, n))
    }
  } else if (variable$op == "max") {
    if (!is_constant(p1)) {
      idx <- variable$meta$index
      inc <- .ad_zeros(p1$value)
      inc[idx] <- grad
      p1$setGrad(inc)
    }
  } else if (variable$op == "[") {
    if (!is_constant(p1)) {
      idx <- variable$meta$index
      inc <- .ad_zeros(p1$value)
      inc[idx] <- grad
      p1$setGrad(inc)
    }
  } else if (variable$op %in% c("pmax", "pmin")) {
    branch <- variable$meta$branch
    inc1 <- .ad_zeros(p1$value)
    inc2 <- .ad_zeros(p2$value)
    if (.ad_node_len(variable$value) == 1L) {
      if (branch == 1L) {
        inc1 <- grad
      } else {
        inc2 <- grad
      }
    } else {
      inc1[branch == 1L] <- grad[branch == 1L]
      inc2[branch == 2L] <- grad[branch == 2L]
    }
    if (!is_constant(p1)) p1$setGrad(inc1)
    if (!is_constant(p2)) p2$setGrad(inc2)
  } else if (variable$op == "%*%") {
    if (!is_constant(p1)) {
      p1$setGrad(grad %*% t(p2$value))
    }
    if (!is_constant(p2)) {
      p2$setGrad(t(p1$value) %*% grad)
    }
  } else {
    stop("Unsupported operation: ", variable$op, call. = FALSE)
  }
}

#' @keywords internal
.ad_reverse_node_r <- function(variable) {
  if (is_constant(variable)) {
    return(invisible(NULL))
  }
  if (variable$op == "") {
    return(invisible(NULL))
  }
  grad <- variable$grad
  .ad_reverse_rule(variable, grad)
  invisible(NULL)
}

#' Reverse-mode backward pass (R implementation)
#'
#' @param variable Root node of the expression graph.
#' @keywords internal
backwardDifferentiate <- function(variable) {
  if (is_constant(variable)) {
    return(invisible(0))
  }

  if (all(variable$grad == 0)) {
    variable$grad <- 1
  }

  if (variable$op == "") {
    return(invisible(NULL))
  }

  grad <- variable$grad
  .ad_reverse_rule(variable, grad)

  p1 <- variable$parents[[1]]
  p2 <- if (length(variable$parents) >= 2) variable$parents[[2]] else NULL
  backwardDifferentiate(p1)
  if (!is.null(p2)) {
    backwardDifferentiate(p2)
  }

  invisible(NULL)
}

#' @keywords internal
.ad_run_reverse <- function(result, backend) {
  if (.ad_node_len(result$value) != 1L) {
    stop("Reverse mode requires a scalar output.", call. = FALSE)
  }
  result$grad <- 1
  if (length(.ad_state$tape) > 0L) {
    reverse_tape_cpp(.ad_state$tape)
    return(invisible(NULL))
  }
  if (backend == "R") {
    backwardDifferentiate(result)
  } else {
    reverseDiff(result)
  }
}
