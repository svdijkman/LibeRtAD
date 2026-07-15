.ad_named_values <- function(x, required, what = "values") {
  nms <- names(x)
  x <- as.numeric(x)
  if (!is.null(nms)) names(x) <- nms
  if (is.null(nms)) {
    if (length(x) != length(required)) {
      .ad_stop("Unnamed ", what, " must have length ", length(required), ".")
    }
    names(x) <- required
    return(x)
  }
  missing <- setdiff(required, nms)
  if (length(missing)) {
    .ad_stop("Missing ", what, ": ", paste(missing, collapse = ", "))
  }
  x[required]
}

#' Pointer-backed compiled automatic-differentiation model
#'
#' `ADModel` is deliberately light: the serializable IR is retained for worker
#' reconstruction, while C++ owns the executable program and persistent tape.
#'
#' @export
ADModel <- R6::R6Class(
  "ADModel",
  public = list(
    #' @field ir Serializable intermediate representation created by [ad_ir()].
    ir = NULL,
    #' @field program_ptr External pointer to the compiled C++ expression program.
    program_ptr = NULL,
    #' @field tape_ptr External pointer to the persistent CppAD tape, or `NULL`
    #'   until [ADModel]$record() is called.
    tape_ptr = NULL,
    #' @field wrt Character vector naming the active differentiation inputs.
    wrt = NULL,
    #' @field outputs Character vector naming the active model outputs.
    outputs = NULL,

    #' @description
    #' Create a pointer-backed model from a validated intermediate representation.
    #' @param ir A `libertad_ir` object created by [ad_ir()].
    #' @return A new `ADModel` object.
    initialize = function(ir) {
      if (!inherits(ir, "libertad_ir")) {
        .ad_stop("`ir` must be created by ad_ir().")
      }
      self$ir <- ir
      self$outputs <- ir$output_names
      self$program_ptr <- .libertad_program_create(ir)
    },

    #' @description
    #' Record a persistent CppAD tape at a named parameter point.
    #' @param at Named numeric values for every model input.
    #' @param wrt Input names with respect to which derivatives are required.
    #' @param outputs Output assignment names to place on the tape.
    #' @param optimize Whether to run CppAD tape optimization.
    #' @return The model, invisibly.
    record = function(at, wrt = names(at), outputs = self$outputs,
                      optimize = TRUE) {
      at <- .ad_named_values(at, self$ir$input_names, "recording values")
      if (is.null(wrt) || !length(wrt)) wrt <- self$ir$input_names
      wrt <- unique(as.character(wrt))
      unknown <- setdiff(wrt, self$ir$input_names)
      if (length(unknown)) .ad_stop("Unknown differentiation input(s): ", paste(unknown, collapse = ", "))
      outputs <- unique(as.character(outputs))
      unknown_out <- setdiff(outputs, self$ir$output_names)
      if (length(unknown_out)) .ad_stop("Unknown tape output(s): ", paste(unknown_out, collapse = ", "))
      self$tape_ptr <- .libertad_tape_create(
        self$program_ptr, at, wrt, outputs, isTRUE(optimize)
      )
      self$wrt <- wrt
      self$outputs <- outputs
      invisible(self)
    },

    #' @description
    #' Evaluate model outputs with the compiled program or recorded tape.
    #' @param at Named input values. Taped evaluation requires the active `wrt`
    #'   inputs; untaped evaluation requires every IR input.
    #' @param taped Use the persistent tape when available.
    #' @return A named numeric vector of model outputs.
    value = function(at, taped = !is.null(self$tape_ptr)) {
      if (isTRUE(taped)) {
        private$require_tape()
        x <- .ad_named_values(at, self$wrt, "tape values")
        return(.libertad_tape_value(self$tape_ptr, x))
      }
      at <- .ad_named_values(at, self$ir$input_names, "program values")
      .libertad_program_value(self$program_ptr, at, self$outputs)
    },

    #' @description
    #' Evaluate the exact output-by-input Jacobian of the recorded tape.
    #' @param at Named values for the active differentiation inputs.
    #' @return A numeric matrix with one row per output and one column per input.
    jacobian = function(at) {
      private$require_tape()
      x <- .ad_named_values(at, self$wrt, "tape values")
      .libertad_tape_jacobian(self$tape_ptr, x)
    },

    #' @description
    #' Evaluate the exact gradient of a single-output tape.
    #' @param at Named values for the active differentiation inputs.
    #' @return A numeric gradient vector.
    gradient = function(at) {
      private$require_tape()
      if (length(self$outputs) != 1L) {
        .ad_stop("gradient() requires a tape with exactly one output; use jacobian().")
      }
      drop(self$jacobian(at))
    },

    #' @description
    #' Evaluate the exact Hessian of a single-output tape.
    #' @param at Named values for the active differentiation inputs.
    #' @return A square numeric Hessian matrix.
    hessian = function(at) {
      private$require_tape()
      if (length(self$outputs) != 1L) {
        .ad_stop("hessian() requires a tape with exactly one output.")
      }
      x <- .ad_named_values(at, self$wrt, "tape values")
      .libertad_tape_hessian(self$tape_ptr, x)
    },

    #' @description
    #' Evaluate a single-output value and its exact gradient together.
    #' @param at Named values for the active differentiation inputs.
    #' @return A list containing `value` and `gradient`.
    value_gradient = function(at) {
      private$require_tape()
      if (length(self$outputs) != 1L) {
        .ad_stop("value_gradient() requires a tape with exactly one output.")
      }
      x <- .ad_named_values(at, self$wrt, "tape values")
      .libertad_tape_value_gradient(self$tape_ptr, x)
    },

    #' @description
    #' Print a concise summary of the compiled program and tape state.
    #' @param ... Unused.
    #' @return The model, invisibly.
    print = function(...) {
      cat("LibeRtAD compiled model\n")
      cat("  inputs:", paste(self$ir$input_names, collapse = ", "), "\n")
      cat("  outputs:", paste(self$outputs, collapse = ", "), "\n")
      cat("  tape:", if (is.null(self$tape_ptr)) "not recorded" else paste("recorded wrt", paste(self$wrt, collapse = ", ")), "\n")
      invisible(self)
    }
  ),
  private = list(
    require_tape = function() {
      if (is.null(self$tape_ptr)) {
        .ad_stop("No tape has been recorded. Call $record(at, wrt, outputs) first.")
      }
    }
  )
)

#' Compile an R-like mathematical model
#'
#' @inheritParams ad_ir
#' @param at Optional named recording point. When supplied, a persistent tape
#'   is recorded immediately.
#' @param wrt Inputs with respect to which derivatives are required.
#' @param optimize Whether CppAD should optimize the recorded tape.
#' @return An `ADModel` R6 object.
#' @examples
#' model <- ad_compile(
#'   "CL = THETA(1) * exp(ETA(1))\nPENALTY = log(CL)^2",
#'   at = c(THETA_1 = 2, ETA_1 = 0),
#'   wrt = c("THETA_1", "ETA_1"),
#'   outputs = "PENALTY"
#' )
#' model$value_gradient(c(THETA_1 = 2, ETA_1 = 0))
#' model$hessian(c(THETA_1 = 2, ETA_1 = 0))
#' @export
ad_compile <- function(code, inputs = character(), outputs = NULL, at = NULL,
                       wrt = names(at), optimize = TRUE) {
  model <- ADModel$new(ad_ir(code, inputs = inputs, outputs = outputs))
  if (!is.null(at)) {
    model$record(at, wrt = wrt, outputs = outputs %||% model$outputs,
                 optimize = optimize)
  }
  model
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Report supported compiler constructs
#' @return A named list of accepted indexed inputs, operators, mathematical
#'   functions, conditionals, and deliberate language limitations.
#' @examples
#' ad_supported()
#' @export
ad_supported <- function() {
  list(
    indexed_inputs = c("THETA", "ETA", "SIGMA", "ERR"),
    operators = c("+", "-", "*", "/", "^"),
    math = c("exp", "log", "sqrt", "sin", "cos", "tan", "tanh", "abs", "expm1", "log1p"),
    conditionals = "ifelse() with <, <=, >, >=, ==, or !=",
    limitations = c(
      "scalar expressions only in the first IR version",
      "runtime if/for/while constructs are rejected",
      "unknown function calls are rejected rather than evaluated in R"
    )
  )
}

#' C++ engine and dependency information
#' @return A named list describing the compiled backend and dependency versions.
#' @examples
#' ad_engine_info()
#' @export
ad_engine_info <- function() {
  .libertad_engine_info()
}
