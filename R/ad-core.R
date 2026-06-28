#' @keywords internal
.ad_is_ad <- function(x) {
  is_variable(x) || is_constant(x)
}

#' @keywords internal
.ad_node_len <- function(value) {
  length(value)
}

#' @keywords internal
.ad_zeros <- function(value) {
  out <- rep(0, .ad_node_len(value))
  if (!is.null(dim(value))) {
    dim(out) <- dim(value)
  }
  out
}

#' @keywords internal
.ad_is_scalar <- function(x) {
  if (.ad_is_ad(x)) {
    return(.ad_node_len(x$value) == 1L)
  }
  length(x) == 1L
}

#' @keywords internal
.ad_scalar_value <- function(x) {
  if (.ad_is_ad(x)) {
    return(x$value)
  }
  if (is.numeric(x) || is.logical(x)) {
    return(x)
  }
  stop("Unsupported type in expression: ", typeof(x), call. = FALSE)
}

#' @keywords internal
.ad_scalar_logical <- function(x) {
  if (length(x) != 1L) {
    stop("Conditions must be length 1.", call. = FALSE)
  }
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    return(as.logical(x))
  }
  if (.ad_is_ad(x)) {
    stop(
      "AD variables cannot be used directly as logical conditions; compare them first (e.g. x > 0).",
      call. = FALSE
    )
  }
  as.logical(x)
}

#' @keywords internal
.ad_node_key <- function(node) {
  as.character(node$node_id)
}

#' @keywords internal
.ad_reset_tangents <- function(parameters) {
  for (p in parameters) {
    p$tangent <- .ad_zeros(p$value)
  }
}

#' @keywords internal
.ad_reset_gradients <- function(parameters) {
  for (p in parameters) {
    p$grad <- .ad_zeros(p$value)
  }
}

#' @keywords internal
.ad_parameter_dofs <- function(parameters) {
  dofs <- list()
  for (p in parameters) {
    if (.ad_node_len(p$value) == 1L) {
      dofs <- c(dofs, list(list(name = p$name, index = NULL)))
    } else {
      for (i in seq_along(p$value)) {
        dofs <- c(dofs, list(list(name = p$name, index = i)))
      }
    }
  }
  dofs
}

#' @keywords internal
.ad_seed_tangent <- function(parameters, dof) {
  .ad_reset_tangents(parameters)
  for (p in parameters) {
    if (p$name == dof$name) {
      if (is.null(dof$index)) {
        p$tangent <- 1
      } else {
        p$tangent[dof$index] <- 1
      }
    }
  }
}

#' @keywords internal
.ad_add_at_index <- function(grad_vec, idx, increment) {
  out <- grad_vec
  out[idx] <- out[idx] + increment
  out
}

#' @keywords internal
.ad_sign <- function(x) {
  ifelse(x > 0, 1, ifelse(x < 0, -1, 0))
}

#' @keywords internal
.ad_use_cpp <- function(...) {
  .ad_state$backend == "cpp"
}
