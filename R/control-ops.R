#' @keywords internal
.ad_cmp <- function(e1, e2, op_fn) {
  v1 <- .ad_scalar_value(e1)
  v2 <- .ad_scalar_value(e2)
  op_fn(v1, v2)
}

#' @keywords internal
.ad_ifelse <- function(test, yes, no) {
  if (isTRUE(.ad_scalar_logical(test))) {
    .ad_as_ad_node(yes)
  } else {
    .ad_as_ad_node(no)
  }
}

#' @keywords internal
.ad_bind_control_ops <- function(env) {
  for (op_nm in c(">", "<", ">=", "<=", "==", "!=")) {
    local({
      op_fn <- match.fun(op_nm)
      assign(op_nm, function(e1, e2) .ad_cmp(e1, e2, op_fn), envir = env)
    })
  }

  assign("&", function(e1, e2) {
    as.logical(.ad_scalar_logical(e1) & .ad_scalar_logical(e2))
  }, envir = env)
  assign("|", function(e1, e2) {
    as.logical(.ad_scalar_logical(e1) | .ad_scalar_logical(e2))
  }, envir = env)
  assign("!", function(e1) {
    !.ad_scalar_logical(e1)
  }, envir = env)
  assign("&&", function(e1, e2) {
    l1 <- .ad_scalar_logical(e1)
    if (!isTRUE(l1)) {
      return(FALSE)
    }
    isTRUE(.ad_scalar_logical(e2))
  }, envir = env)
  assign("||", function(e1, e2) {
    l1 <- .ad_scalar_logical(e1)
    if (isTRUE(l1)) {
      return(TRUE)
    }
    isTRUE(.ad_scalar_logical(e2))
  }, envir = env)
  assign("ifelse", .ad_ifelse, envir = env)

  invisible(NULL)
}

#' @export
#' @method > Variable
`>.Variable` <- function(e1, e2) {
  .ad_cmp(e1, e2, `>`)
}

#' @export
#' @method < Variable
`<.Variable` <- function(e1, e2) {
  .ad_cmp(e1, e2, `<`)
}

#' @export
#' @method >= Variable
`>=.Variable` <- function(e1, e2) {
  .ad_cmp(e1, e2, `>=`)
}

#' @export
#' @method <= Variable
`<=.Variable` <- function(e1, e2) {
  .ad_cmp(e1, e2, `<=`)
}

#' @export
#' @method == Variable
`==.Variable` <- function(e1, e2) {
  .ad_cmp(e1, e2, `==`)
}

#' @export
#' @method != Variable
`!=.Variable` <- function(e1, e2) {
  .ad_cmp(e1, e2, `!=`)
}
