#' Normalize NONMEM/Fortran power operator in expression text
#'
#' Converts \code{**} to R/C++ power \code{^} for parsing and AD.
#'
#' @param expr Character scalar or vector of expression strings.
#' @return Character vector with \code{**} replaced by \code{^}.
#' @examples
#' ad_nm_expr_normalize("CL = THETA(1) ** 2")
#' @export
ad_nm_expr_normalize <- function(expr) {
  if (length(expr) == 0L) {
    return(expr)
  }
  gsub("\\*\\*", "^", as.character(expr), fixed = FALSE)
}
