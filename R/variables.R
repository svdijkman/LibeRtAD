#' Create an AD variable
#'
#' @param name Variable name used in symbolic expressions.
#' @param value Numeric value at which to evaluate.
#' @param op Internal operation label.
#' @param parents Internal parent nodes.
#' @param par Logical; mark as a differentiation parameter.
#' @param expr Internal parsed expression.
#' @param meta Internal metadata for some operations.
#' @return A \code{Variable} R6 object.
#' @export
#' @examples
#' x <- newVariable("x", 2, par = TRUE)
#' x$value
newVariable <- function(name, value, op = NULL, parents = list(),
                        par = FALSE, expr = NULL, meta = NULL) {
  if (is.null(op)) {
    Variable$new(
      name = name, value = value, op = "", par = par, meta = meta
    )
  } else {
    Variable$new(
      name = name, value = value, op = op, parents = parents,
      par = par, expr = expr, meta = meta
    )
  }
}

#' Create an AD constant
#'
#' @param name Constant name.
#' @param value Numeric value.
#' @param grad Initial gradient (usually zero).
#' @return A \code{Constant} R6 object.
#' @export
#' @examples
#' c <- newConstant("c", 3)
#' c$value
newConstant <- function(name, value, grad = NULL) {
  Constant$new(name = name, value = value, grad = grad)
}

#' Create an AD variable manually
#'
#' Normally you do not need this function: \code{\link{backdiff}} converts
#' named parameter values to AD variables automatically. Use \code{ad_var} only
#' for advanced/manual graph construction.
#'
#' @param name Variable name.
#' @param value Numeric scalar or vector value.
#' @return A \code{Variable} R6 object.
#' @export
#' @examples
#' ad_var("x", 2)
ad_var <- function(name, value) {
  newVariable(name, value, par = TRUE)
}
