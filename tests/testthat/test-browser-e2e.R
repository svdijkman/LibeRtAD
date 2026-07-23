test_that("LibeRtAD renders in a real browser", {
  skip_if_not_installed("shinytest2")
  skip_if(Sys.getenv("LIBER_RUN_BROWSER_TESTS") != "true")
  driver <- shinytest2::AppDriver$new(
    LibeRtAD::libertad_gui(launch.browser = NULL), name = "libertad-browser",
    width = 1366, height = 768, load_timeout = 120000, seed = 20260723
  )
  on.exit(driver$stop(), add = TRUE)
  driver$wait_for_idle()
  expect_identical(driver$get_js("document.title"), "LibeRtAD")
  expect_match(driver$get_js("document.body.innerText"), "Benchmark")
  expect_false(driver$get_js(
    "document.documentElement.scrollWidth > document.documentElement.clientWidth + 2"
  ))
})
