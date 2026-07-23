test_that("benchmark GUI retains shared theme and responsive controls", {
  script <- paste(readLines(
    system.file("htmlwidgets", "libertadWorkbench.js", package = "LibeRtAD"),
    warn = FALSE
  ), collapse = "\n")
  css <- paste(readLines(
    system.file("htmlwidgets", "libertadWorkbench.css", package = "LibeRtAD"),
    warn = FALSE
  ), collapse = "\n")

  expect_match(script, 'localStorage\\.getItem\\("liber\\.theme"\\)')
  expect_match(script, "ad-nav-toggle")
  expect_match(script, "ad-config-toggle")
  expect_match(script, "aria-expanded", fixed = TRUE)
  expect_match(css, "\\.ad-sidebar\\.open")
  expect_match(css, "\\.ad-config\\.open")
  expect_match(css, "focus-visible", fixed = TRUE)
  expect_match(
    css,
    "grid-template-rows:58px 32px minmax(0,1fr) 27px",
    fixed = TRUE
  )
  expect_match(css, ".ad-logo{width:42px;height:42px", fixed = TRUE)
  expect_match(css, ".ad-panel{margin-bottom:11px;border:1px solid var(--line);border-radius:10px", fixed = TRUE)
})
