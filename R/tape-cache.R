#' Persist and reuse AD tapes
#'
#' @param key Character cache key; defaults to a hash of the function body.
#' @param env Environment to store cache (default: package-private).
#' @return Invisibly, the stored tape entry.
#' @export
#' @examples
#' f <- function(x) x^2
#' autodiff(f, x = 2, record_tape = TRUE)
#' ad_tape_save("quad")
ad_tape_save <- function(key = NULL, env = .ad_tape_cache_env()) {
  if (!isTRUE(.ad_state$active) && length(.ad_state$tape) == 0L) {
    stop("No active tape to save. Run autodiff with record_tape = TRUE first.")
  }
  if (is.null(key)) {
    key <- paste0("tape_", .ad_state$graph_gen)
  }
  entry <- list(
    tape = .ad_state$tape,
    graph_gen = .ad_state$graph_gen,
    n_nodes = length(.ad_state$tape)
  )
  assign(key, entry, envir = env)
  invisible(entry)
}

#' Load a cached AD tape
#' @param key Cache key used with \code{ad_tape_save}.
#' @param env Cache environment.
#' @return Tape entry list or \code{NULL}.
#' @export
#' @examples
#' f <- function(x) x^2
#' autodiff(f, x = 2, record_tape = TRUE)
#' ad_tape_save("quad")
#' ad_tape_load("quad")
ad_tape_load <- function(key, env = .ad_tape_cache_env()) {
  if (!exists(key, envir = env, inherits = FALSE)) {
    return(NULL)
  }
  get(key, envir = env, inherits = FALSE)
}

#' @keywords internal
.ad_tape_cache_env <- function() {
  .ad_tape_cache
}

#' Reuse a cached tape for reverse-mode AD when structure matches
#' @param f Function
#' @param at Named list of parameter values
#' @param cache_key Optional key; if \code{NULL}, tape is not reused
#' @param backend AD backend
#' @return Same structure as \code{autodiff(..., mode = "reverse")}.
#' @export
#' @examples
#' f <- function(x) x^2
#' autodiff(f, x = 2, record_tape = TRUE)
#' ad_tape_save("quad")
#' ad_tape_reuse(f, at = list(x = 3), cache_key = "quad")
#' @keywords internal
.ad_bind_tape_parameters <- function(tape, at) {
  lapply(names(at), function(nm) {
    for (node in tape) {
      if (is_variable(node) && isTRUE(node$par) && identical(node$name, nm)) {
        return(node)
      }
    }
    stop("Cached tape missing parameter `", nm, "`.", call. = FALSE)
  })
}

#' @keywords internal
.ad_tape_replay_grad <- function(tape, at, backend = "cpp") {
  at <- .ad_parse_at(at)
  parameters <- .ad_bind_tape_parameters(tape, at)
  replay_tape_values_cpp(tape, at)
  reset_tape_grads_cpp(tape)
  set_ops(backend)
  on.exit(set_ops("R"), add = TRUE)
  .ad_run_reverse(tape[[length(tape)]], backend)
  partials_flat <- .ad_collect_reverse_partials(parameters)
  list(
    value = tape_scalar_value_cpp(tape),
    partials_flat = partials_flat,
    parameters = parameters
  )
}

ad_tape_reuse <- function(f, at = NULL, cache_key = NULL, backend = c("R", "cpp")) {
  backend <- match.arg(backend)
  at <- .ad_parse_at(at)
  .ad_check_formals(f, at)
  cached <- if (!is.null(cache_key)) ad_tape_load(cache_key) else NULL
  if (!is.null(cached) && cached$n_nodes > 0L) {
    .ad_state$tape <- cached$tape
    .ad_state$active <- FALSE
    .ad_state$graph_gen <- cached$graph_gen
    replayed <- .ad_tape_replay_grad(cached$tape, at, backend)
    root <- cached$tape[[length(cached$tape)]]
    return(.ad_collect_result(root, replayed$parameters, replayed$partials_flat))
  }
  autodiff(f, at = at, mode = "reverse", backend = backend, record_tape = TRUE)
}
