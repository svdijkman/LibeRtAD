.onLoad <- function(libname, pkgname) {
  .ad_state$backend <- "R"
}

.onUnload <- function(libpath) {
  library.dynam.unload("LibeRtAD", libpath)
}
