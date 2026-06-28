#' @keywords internal
.ad_state <- new.env(parent = emptyenv())
.ad_tape_cache <- new.env(parent = emptyenv())
.ad_state$tape <- list()
.ad_state$active <- FALSE
.ad_state$backend <- "R"
.ad_state$node_id <- 0L
.ad_state$graph_gen <- 0L
#' Clears recorded operations and disables tape recording until re-enabled.
#'
#' @return Invisibly \code{NULL}.
#' @export
#' @examples
#' f <- function(x) x^2
#' autodiff(f, x = 2, record_tape = TRUE)
#' reset_tape()
reset_tape <- function() {
  if (length(.ad_state$tape) == 0L) {
    .ad_state$active <- FALSE
    .ad_state$node_id <- 0L
    if (exists("reset_ad_node_seq_cpp", mode = "function")) {
      reset_ad_node_seq_cpp()
    }
    if (exists("reset_grad_sidecar_cpp", mode = "function")) {
      reset_grad_sidecar_cpp()
    }
    if (exists("clear_const_pool_export", mode = "function")) {
      clear_const_pool_export()
    }
    return(invisible(NULL))
  }
  .ad_state$tape <- list()
  .ad_state$active <- FALSE
  .ad_state$node_id <- 0L
  .ad_state$graph_gen <- .ad_state$graph_gen + 1L
  if (exists("reset_ad_node_seq_cpp", mode = "function")) {
    reset_ad_node_seq_cpp()
  }
  if (exists("reset_grad_sidecar_cpp", mode = "function")) {
    reset_grad_sidecar_cpp()
  }
  if (exists("clear_const_pool_export", mode = "function")) {
    clear_const_pool_export()
  }
  invisible(NULL)
}
