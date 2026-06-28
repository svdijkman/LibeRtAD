#' @keywords internal
is_constant <- function(x) {
  inherits(x, "Constant")
}

#' @keywords internal
is_variable <- function(x) {
  inherits(x, "Variable")
}

#' R6 class representing an AD variable node
#'
#' Nodes form a computational graph recorded on the active tape. Public fields
#' include \code{name}, \code{value}, \code{grad}, \code{tangent}, \code{op},
#' \code{parents}, and \code{TapePos}. Use [newVariable()] or overloaded
#' operators rather than calling \code{initialize()} directly.
#'
#' @export
#' @format An [R6::R6Class] generator object.
#' @examples
#' x <- newVariable("x", 2, par = TRUE)
#' x$value
Variable <- R6::R6Class(
  "Variable",
  cloneable = FALSE,
  portable = FALSE,
  public = list(
    name = NULL,
    value = NULL,
    grad = NULL,
    tangent = NULL,
    op = NULL,
    parents = list(),
    TapePos = 0L,
    par = FALSE,
    expr = NULL,
    meta = NULL,
    node_id = 0L,
    initialize = function(name = NA, value = NA, grad = NULL, tangent = NULL,
                          op = NA, parents = list(), par = FALSE, expr = NULL,
                          meta = NULL) {
      .ad_state$node_id <- .ad_state$node_id + 1L
      self$node_id <- .ad_state$node_id
      self$name <- name
      self$value <- value
      self$grad <- if (is.null(grad)) .ad_zeros(value) else grad
      self$tangent <- if (is.null(tangent)) .ad_zeros(value) else tangent
      self$op <- op
      self$parents <- parents
      self$par <- par
      self$expr <- expr
      self$meta <- meta
      if (.ad_state$active) {
        self$TapePos <- length(.ad_state$tape) + 1L
        .ad_state$tape[[self$TapePos]] <- self
      }
    },
    setGrad = function(value) {
      self$grad <- self$grad + value
    },
    setTangent = function(value) {
      self$tangent <- self$tangent + value
    }
  )
)

#' R6 class representing a constant node
#'
#' Constants participate in the graph but are not differentiated with respect to.
#'
#' @export
#' @format An [R6::R6Class] generator object.
#' @examples
#' c <- newConstant("c", 3)
#' c$value
Constant <- R6::R6Class(
  "Constant",
  cloneable = FALSE,
  portable = FALSE,
  public = list(
    name = NULL,
    value = NULL,
    grad = NULL,
    tangent = NULL,
    node_id = 0L,
    initialize = function(name, value, grad = NULL, tangent = NULL) {
      .ad_state$node_id <- .ad_state$node_id + 1L
      self$node_id <- .ad_state$node_id
      self$name <- name
      self$value <- value
      self$grad <- if (is.null(grad)) .ad_zeros(value) else grad
      self$tangent <- if (is.null(tangent)) .ad_zeros(value) else tangent
    },
    setGrad = function(value) {
      self$grad <- self$grad + value
    },
    setTangent = function(value) {
      self$tangent <- self$tangent + value
    }
  )
)
