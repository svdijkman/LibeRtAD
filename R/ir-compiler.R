.ad_op <- c(
  input = 0L, constant = 1L, add = 2L, sub = 3L, mul = 4L,
  div = 5L, pow = 6L, neg = 7L, exp = 8L, log = 9L,
  sqrt = 10L, sin = 11L, cos = 12L, tan = 13L, tanh = 14L,
  abs = 15L, expm1 = 16L, log1p = 17L, min = 18L, max = 19L,
  cond_lt = 20L, cond_le = 21L, cond_gt = 22L, cond_ge = 23L,
  cond_eq = 24L, cond_ne = 25L
)

.ad_stop <- function(..., call. = FALSE) {
  stop(..., call. = call.)
}

.ad_clean_code <- function(code) {
  if (length(code) == 0L || all(is.na(code))) {
    return("")
  }
  paste(as.character(code), collapse = "\n")
}

.ad_compile_state <- function(inputs) {
  state <- new.env(parent = emptyenv())
  state$nodes <- list()
  state$symbols <- new.env(parent = emptyenv())
  state$inputs <- character()
  state$output_names <- character()
  state$output_nodes <- integer()

  add_node <- function(op, a = 0L, b = 0L, c = 0L, d = 0L,
                       value = 0, label = "") {
    node <- list(
      op = as.integer(op), a = as.integer(a), b = as.integer(b),
      c = as.integer(c), d = as.integer(d), value = as.numeric(value),
      label = as.character(label)
    )
    state$nodes[[length(state$nodes) + 1L]] <- node
    length(state$nodes)
  }

  add_input <- function(name) {
    name <- as.character(name)
    if (exists(name, envir = state$symbols, inherits = FALSE)) {
      return(get(name, envir = state$symbols, inherits = FALSE))
    }
    state$inputs <- c(state$inputs, name)
    idx <- add_node(.ad_op[["input"]], a = length(state$inputs), label = name)
    assign(name, idx, envir = state$symbols)
    idx
  }

  for (input in unique(as.character(inputs))) {
    if (nzchar(input)) {
      add_input(input)
    }
  }

  state$add_node <- add_node
  state$add_input <- add_input
  state
}

.ad_indexed_input <- function(call, state) {
  fn <- as.character(call[[1L]])
  if (length(call) != 2L || !is.numeric(call[[2L]]) || length(call[[2L]]) != 1L) {
    .ad_stop(fn, "() requires one literal positive integer index.")
  }
  idx <- as.integer(call[[2L]])
  if (is.na(idx) || idx < 1L || idx != as.numeric(call[[2L]])) {
    .ad_stop(fn, "() requires one literal positive integer index.")
  }
  state$add_input(paste0(fn, "_", idx))
}

.ad_compile_condition <- function(expr, state, compile_expr) {
  if (!is.call(expr) || length(expr) != 3L) {
    .ad_stop("ifelse() condition must be a binary comparison.")
  }
  op <- as.character(expr[[1L]])
  map <- c(
    "<" = "cond_lt", "<=" = "cond_le", ">" = "cond_gt",
    ">=" = "cond_ge", "==" = "cond_eq", "!=" = "cond_ne"
  )
  opname <- unname(map[[op]])
  if (is.null(opname)) {
    .ad_stop("Unsupported ifelse() comparison: ", op)
  }
  list(op = .ad_op[[opname]], a = compile_expr(expr[[2L]]), b = compile_expr(expr[[3L]]))
}

.ad_compile_expression <- function(expr, state) {
  compile_expr <- NULL
  compile_expr <- function(x) {
    if (is.numeric(x) && length(x) == 1L) {
      return(state$add_node(.ad_op[["constant"]], value = x))
    }
    if (is.logical(x) && length(x) == 1L && !is.na(x)) {
      return(state$add_node(.ad_op[["constant"]], value = as.numeric(x)))
    }
    if (is.symbol(x)) {
      name <- as.character(x)
      if (identical(name, "pi")) {
        return(state$add_node(.ad_op[["constant"]], value = pi, label = "pi"))
      }
      if (exists(name, envir = state$symbols, inherits = FALSE)) {
        return(get(name, envir = state$symbols, inherits = FALSE))
      }
      return(state$add_input(name))
    }
    if (!is.call(x)) {
      .ad_stop("Unsupported expression element: ", paste(deparse(x), collapse = " "))
    }

    fn <- as.character(x[[1L]])
    if (fn == "(") {
      return(compile_expr(x[[2L]]))
    }
    if (fn %in% c("THETA", "ETA", "SIGMA", "ERR")) {
      return(.ad_indexed_input(x, state))
    }
    if (fn %in% c("+", "-", "*", "/", "^")) {
      if (length(x) == 2L) {
        if (fn == "+") return(compile_expr(x[[2L]]))
        if (fn == "-") {
          return(state$add_node(.ad_op[["neg"]], a = compile_expr(x[[2L]])))
        }
      }
      if (length(x) != 3L) {
        .ad_stop("Operator '", fn, "' has unsupported arity.")
      }
      op <- c("+" = "add", "-" = "sub", "*" = "mul", "/" = "div", "^" = "pow")[[fn]]
      return(state$add_node(
        .ad_op[[op]], a = compile_expr(x[[2L]]), b = compile_expr(x[[3L]])
      ))
    }
    unary <- c(
      exp = "exp", log = "log", sqrt = "sqrt", sin = "sin",
      cos = "cos", tan = "tan", tanh = "tanh", abs = "abs",
      expm1 = "expm1", log1p = "log1p"
    )
    if (fn %in% names(unary)) {
      if (length(x) != 2L) .ad_stop(fn, "() requires one argument.")
      return(state$add_node(.ad_op[[unary[[fn]]]], a = compile_expr(x[[2L]])))
    }
    if (fn %in% c("pmin", "pmax", "min", "max")) {
      if (length(x) != 3L) .ad_stop(fn, "() currently requires two arguments.")
      op <- if (fn %in% c("pmin", "min")) "min" else "max"
      return(state$add_node(
        .ad_op[[op]], a = compile_expr(x[[2L]]), b = compile_expr(x[[3L]])
      ))
    }
    if (fn == "ifelse") {
      if (length(x) != 4L) .ad_stop("ifelse() requires condition, yes, and no.")
      cond <- .ad_compile_condition(x[[2L]], state, compile_expr)
      return(state$add_node(
        cond$op, a = cond$a, b = cond$b,
        c = compile_expr(x[[3L]]), d = compile_expr(x[[4L]])
      ))
    }
    if (fn == "if") {
      .ad_stop("Use vector-free ifelse(condition, yes, no); runtime if statements are not tape-safe.")
    }
    .ad_stop("Unsupported function in compiled model: ", fn, "()")
  }
  compile_expr(expr)
}

.ad_compile_statement <- function(expr, state, ordinal) {
  if (is.call(expr) && as.character(expr[[1L]]) %in% c("<-", "=")) {
    if (length(expr) != 3L || !is.symbol(expr[[2L]])) {
      .ad_stop("Only assignments to simple names are supported: ",
               paste(deparse(expr), collapse = " "))
    }
    name <- as.character(expr[[2L]])
    idx <- .ad_compile_expression(expr[[3L]], state)
  } else {
    name <- paste0(".value", ordinal)
    idx <- .ad_compile_expression(expr, state)
  }
  assign(name, idx, envir = state$symbols)
  existing <- match(name, state$output_names)
  if (is.na(existing)) {
    state$output_names <- c(state$output_names, name)
    state$output_nodes <- c(state$output_nodes, idx)
  } else {
    state$output_nodes[[existing]] <- idx
  }
  invisible(idx)
}

#' Compile mathematical model code to a serializable intermediate representation
#'
#' @param code Character model code. The established LibeRation assignment,
#'   `THETA(i)`, `ETA(i)`, `SIGMA(i)`, and `ERR(i)` syntax is supported.
#' @param inputs Optional external input names. Undeclared symbols are appended
#'   as inputs in first-use order.
#' @param outputs Optional assignment names to expose. The default exposes all
#'   assignments, with the most recent value for reassigned names.
#' @return A serializable `libertad_ir` object.
#' @examples
#' ir <- ad_ir(
#'   "CL = THETA(1) * exp(ETA(1))\nY = log(CL)",
#'   outputs = "Y"
#' )
#' ir
#' @export
ad_ir <- function(code, inputs = character(), outputs = NULL) {
  code <- .ad_clean_code(code)
  parsed <- tryCatch(
    parse(text = code, keep.source = TRUE),
    error = function(e) .ad_stop("Unable to parse model code: ", conditionMessage(e))
  )
  if (length(parsed) == 0L) {
    .ad_stop("Model code contains no expressions.")
  }
  state <- .ad_compile_state(inputs)
  for (i in seq_along(parsed)) {
    tryCatch(
      .ad_compile_statement(parsed[[i]], state, i),
      error = function(e) .ad_stop(
        "Expression ", i, " (`", paste(deparse(parsed[[i]]), collapse = " "),
        "`): ", conditionMessage(e)
      )
    )
  }
  if (is.null(outputs)) {
    outputs <- state$output_names
  }
  outputs <- unique(as.character(outputs))
  pos <- match(outputs, state$output_names)
  if (anyNA(pos)) {
    .ad_stop("Unknown requested output(s): ", paste(outputs[is.na(pos)], collapse = ", "))
  }
  structure(
    list(
      version = 1L,
      code = code,
      input_names = state$inputs,
      nodes = state$nodes,
      output_names = outputs,
      output_nodes = unname(state$output_nodes[pos])
    ),
    class = "libertad_ir"
  )
}

#' @export
print.libertad_ir <- function(x, ...) {
  cat("LibeRtAD expression IR\n")
  cat("  inputs:", length(x$input_names), " nodes:", length(x$nodes),
      " outputs:", length(x$output_names), "\n")
  if (length(x$input_names)) cat("  domain:", paste(x$input_names, collapse = ", "), "\n")
  cat("  range:", paste(x$output_names, collapse = ", "), "\n")
  invisible(x)
}
