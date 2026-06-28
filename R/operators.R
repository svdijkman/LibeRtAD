#' @keywords internal
.ad_next_name <- function() {
  .ad_state$node_id <- .ad_state$node_id + 1L
  paste0("adn", .ad_state$node_id)
}

#' @keywords internal
.ad_as_operand <- function(x) {
  if (is_variable(x)) {
    list(var = x, name = x$name)
  } else if (is_constant(x)) {
    list(var = x, name = x$name)
  } else {
    list(
      var = newConstant(name = paste0("const_", paste(x, collapse = "_")), value = x),
      name = paste(x, collapse = ",")
    )
  }
}

#' @keywords internal
.ad_binary_op <- function(e1, e2, op_symbol, op_fn, expr_fn) {
  left <- .ad_as_operand(e1)
  right <- .ad_as_operand(e2)
  newVariable(
    name = .ad_next_name(),
    value = op_fn(left$var$value, right$var$value),
    op = op_symbol,
    parents = list(left$var, right$var),
    expr = parse(text = expr_fn(left$name, right$name))
  )
}

#' @keywords internal
.ad_unary_op <- function(e1, op_symbol, op_fn, expr_fn) {
  operand <- .ad_as_operand(e1)
  newVariable(
    name = .ad_next_name(),
    value = op_fn(operand$var$value),
    op = op_symbol,
    parents = list(operand$var),
    expr = parse(text = expr_fn(operand$name))
  )
}

#' @keywords internal
.ad_op_add_r <- function(e1, e2) {
  .ad_binary_op(e1, e2, "+", function(a, b) a + b,
                function(a, b) paste0(a, "+", b))
}

#' @keywords internal
.ad_op_sub_r <- function(e1, e2) {
  .ad_binary_op(e1, e2, "-", function(a, b) a - b,
                function(a, b) paste0(a, "-", b))
}

#' @keywords internal
.ad_op_neg_r <- function(e1) {
  .ad_unary_op(e1, "neg", function(a) -a, function(a) paste0("-", a))
}

#' @keywords internal
.ad_op_mul_r <- function(e1, e2) {
  .ad_binary_op(e1, e2, "*", function(a, b) a * b,
                function(a, b) paste0(a, "*", b))
}

#' @keywords internal
.ad_op_div_r <- function(e1, e2) {
  .ad_binary_op(e1, e2, "/", function(a, b) a / b,
                function(a, b) paste0(a, "/", b))
}

#' @keywords internal
.ad_op_pow_r <- function(e1, e2) {
  .ad_binary_op(e1, e2, "^", function(a, b) a ^ b,
                function(a, b) paste0(a, "^", b))
}

#' @keywords internal
.ad_op_sin_r <- function(e1) {
  .ad_unary_op(e1, "sin", sin, function(a) paste0("sin(", a, ")"))
}

#' @keywords internal
.ad_op_cos_r <- function(e1) {
  .ad_unary_op(e1, "cos", cos, function(a) paste0("cos(", a, ")"))
}

#' @keywords internal
.ad_op_exp_r <- function(e1) {
  .ad_unary_op(e1, "exp", exp, function(a) paste0("exp(", a, ")"))
}

#' @keywords internal
.ad_op_log_r <- function(e1) {
  operand <- .ad_as_operand(e1)
  v <- operand$var$value
  if (all(is.finite(v)) && any(v <= 0)) {
    stop("Value must be positive for log function", call. = FALSE)
  }
  .ad_unary_op(e1, "log", log, function(a) paste0("log(", a, ")"))
}

#' @keywords internal
.ad_op_abs_r <- function(e1) {
  .ad_unary_op(e1, "abs", abs, function(a) paste0("abs(", a, ")"))
}

#' @keywords internal
.ad_op_sqrt_r <- function(e1) {
  operand <- .ad_as_operand(e1)
  if (any(operand$var$value < 0)) {
    stop("Value must be non-negative for sqrt function", call. = FALSE)
  }
  .ad_unary_op(e1, "sqrt", sqrt, function(a) paste0("sqrt(", a, ")"))
}

#' @keywords internal
.ad_branch_op <- function(e1, e2, op_symbol, pick_fn) {
  left <- .ad_as_operand(e1)
  right <- .ad_as_operand(e2)
  branch <- pick_fn(left$var$value, right$var$value)
  newVariable(
    name = .ad_next_name(),
    value = if (op_symbol == "pmax") {
      pmax(left$var$value, right$var$value)
    } else {
      pmin(left$var$value, right$var$value)
    },
    op = op_symbol,
    parents = list(left$var, right$var),
    meta = list(branch = branch)
  )
}

#' @keywords internal
.ad_op_pmax_r <- function(e1, e2) {
  .ad_branch_op(e1, e2, "pmax", function(a, b) ifelse(a >= b, 1L, 2L))
}

#' @keywords internal
.ad_op_pmin_r <- function(e1, e2) {
  .ad_branch_op(e1, e2, "pmin", function(a, b) ifelse(a <= b, 1L, 2L))
}

#' @keywords internal
.ad_op_sum_r <- function(e1) {
  operand <- .ad_as_operand(e1)
  newVariable(
    name = .ad_next_name(),
    value = sum(operand$var$value),
    op = "sum",
    parents = list(operand$var)
  )
}

#' @keywords internal
.ad_op_mean_r <- function(e1) {
  operand <- .ad_as_operand(e1)
  newVariable(
    name = .ad_next_name(),
    value = mean(operand$var$value),
    op = "mean",
    parents = list(operand$var)
  )
}

#' @keywords internal
.ad_op_max_r <- function(e1) {
  operand <- .ad_as_operand(e1)
  idx <- which.max(operand$var$value)
  newVariable(
    name = .ad_next_name(),
    value = max(operand$var$value),
    op = "max",
    parents = list(operand$var),
    meta = list(index = idx)
  )
}

#' @keywords internal
.ad_op_matmul_r <- function(e1, e2) {
  left <- .ad_as_operand(e1)
  right <- .ad_as_operand(e2)
  newVariable(
    name = .ad_next_name(),
    value = left$var$value %*% right$var$value,
    op = "%*%",
    parents = list(left$var, right$var)
  )
}

#' @keywords internal
.ad_subset <- function(x, i) {
  if (!.ad_is_ad(x)) {
    stop("Subsetting is only supported for AD nodes.", call. = FALSE)
  }
  idx <- as.integer(i)
  if (length(idx) != 1L) {
    stop("Only a single index is supported for AD vector subsetting.", call. = FALSE)
  }
  if (.ad_use_cpp()) {
    return(subset_var(x, idx))
  }
  val <- x$value[idx]
  if (is_constant(x)) {
    return(newConstant(name = paste0(x$name, "[", idx, "]"), value = val))
  }
  newVariable(
    name = .ad_next_name(),
    value = val,
    op = "[",
    parents = list(x),
    meta = list(index = idx)
  )
}

#' @keywords internal
.ad_dispatch <- function(op, ...) {
  args <- list(...)
  if (length(args) > 0L && .ad_use_cpp()) {
    fn <- switch(
      op,
      "+" = add_var,
      "-" = sub_var,
      neg = neg_var,
      "*" = mul_var,
      "/" = div_var,
      "^" = pow_var,
      sin = sin_var,
      cos = cos_var,
      exp = exp_var,
      log = log_var,
      abs = abs_var,
      sqrt = sqrt_var,
      pmax = pmax_var,
      pmin = pmin_var,
      sum = sum_var,
      mean = mean_var,
      max = max_var,
      "%*%" = matmul_var,
      stop("Unsupported operation: ", op, call. = FALSE)
    )
    return(do.call(fn, args))
  }

  fn <- switch(
    op,
    "+" = .ad_op_add_r,
    "-" = .ad_op_sub_r,
    neg = .ad_op_neg_r,
    "*" = .ad_op_mul_r,
    "/" = .ad_op_div_r,
    "^" = .ad_op_pow_r,
    sin = .ad_op_sin_r,
    cos = .ad_op_cos_r,
    exp = .ad_op_exp_r,
    log = .ad_op_log_r,
    abs = .ad_op_abs_r,
    sqrt = .ad_op_sqrt_r,
    pmax = .ad_op_pmax_r,
    pmin = .ad_op_pmin_r,
    sum = .ad_op_sum_r,
    mean = .ad_op_mean_r,
    max = .ad_op_max_r,
    "%*%" = .ad_op_matmul_r,
    stop("Unsupported operation: ", op, call. = FALSE)
  )
  do.call(fn, args)
}

#' @keywords internal
.ad_numeric_binary <- function(e1, e2, op) {
  if (.ad_is_ad(e2)) {
    .ad_dispatch(op, e1, e2)
  } else {
    base::get(op, mode = "function")(e1, e2)
  }
}

#' @export
#' @method + numeric
`+.numeric` <- function(e1, e2) {
  .ad_numeric_binary(e1, e2, "+")
}

#' @export
#' @method - numeric
`-.numeric` <- function(e1, e2) {
  .ad_numeric_binary(e1, e2, "-")
}

#' @export
#' @method * numeric
`*.numeric` <- function(e1, e2) {
  .ad_numeric_binary(e1, e2, "*")
}

#' @export
#' @method / numeric
`/.numeric` <- function(e1, e2) {
  .ad_numeric_binary(e1, e2, "/")
}

#' @export
#' @method ^ numeric
`^.numeric` <- function(e1, e2) {
  .ad_numeric_binary(e1, e2, "^")
}

.ad_constant_binary <- function(e1, e2, op) {
  if (.ad_is_ad(e2)) {
    .ad_dispatch(op, e1, e2)
  } else {
    base::get(op, mode = "function")(e1, e2)
  }
}

#' @export
#' @method * Constant
`*.Constant` <- function(e1, e2) {
  .ad_constant_binary(e1, e2, "*")
}

#' @export
#' @method + Constant
`+.Constant` <- function(e1, e2) {
  .ad_constant_binary(e1, e2, "+")
}

#' @export
#' @method - Constant
`-.Constant` <- function(e1, e2) {
  if (missing(e2)) {
    .ad_dispatch("neg", e1)
  } else {
    .ad_constant_binary(e1, e2, "-")
  }
}

#' @export
#' @method / Constant
`/.Constant` <- function(e1, e2) {
  .ad_constant_binary(e1, e2, "/")
}

#' @export
#' @method ^ Constant
`^.Constant` <- function(e1, e2) {
  .ad_constant_binary(e1, e2, "^")
}

#' @export
#' @method + Variable
`+.Variable` <- function(e1, e2) {
  .ad_dispatch("+", e1, e2)
}

#' @export
#' @method - Variable
`-.Variable` <- function(e1, e2) {
  if (missing(e2)) {
    .ad_dispatch("neg", e1)
  } else {
    .ad_dispatch("-", e1, e2)
  }
}

#' @export
#' @method * Variable
`*.Variable` <- function(e1, e2) {
  .ad_dispatch("*", e1, e2)
}

#' @export
#' @method / Variable
`/.Variable` <- function(e1, e2) {
  .ad_dispatch("/", e1, e2)
}

#' @export
#' @method ^ Variable
`^.Variable` <- function(e1, e2) {
  .ad_dispatch("^", e1, e2)
}

#' @export
#' @method [ Variable
`[.Variable` <- function(x, i, ...) {
  .ad_subset(x, i)
}

#' @method sin Variable
#' @export
sin.Variable <- function(x) {
  .ad_dispatch("sin", x)
}

#' @method cos Variable
#' @export
cos.Variable <- function(x) {
  .ad_dispatch("cos", x)
}

#' @method exp Variable
#' @export
exp.Variable <- function(x) {
  .ad_dispatch("exp", x)
}

#' @method log Variable
#' @export
log.Variable <- function(x, base = exp(1)) {
  .ad_dispatch("log", x)
}

#' @method abs Variable
#' @export
abs.Variable <- function(x) {
  .ad_dispatch("abs", x)
}

#' @method sqrt Variable
#' @export
sqrt.Variable <- function(x) {
  .ad_dispatch("sqrt", x)
}

#' @export
`%*%.Variable` <- function(x, y) {
  .ad_dispatch("%*%", x, y)
}

#' @export
`%*%.Constant` <- function(x, y) {
  .ad_dispatch("%*%", x, y)
}

#' @export
`%*%.numeric` <- function(x, y) {
  if (.ad_is_ad(y)) {
    .ad_dispatch("%*%", x, y)
  } else {
    base::`%*%`(x, y)
  }
}

#' @export
set_ops <- function(backend = c("R", "cpp")) {
  .ad_state$backend <- match.arg(backend)
  invisible(.ad_state$backend)
}
