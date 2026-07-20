test_that("GUI process cleanup is safe outside a reactive consumer", {
  idle <- shiny::reactiveValues(process = NULL)
  expect_no_error(LibeRtAD:::.ad_gui_stop_process(idle))

  killed <- FALSE
  process <- list(
    is_alive = function() TRUE,
    kill = function() {
      killed <<- TRUE
      invisible(TRUE)
    }
  )
  active <- shiny::reactiveValues(process = process)

  expect_no_error(LibeRtAD:::.ad_gui_stop_process(active))
  expect_true(killed)
})
