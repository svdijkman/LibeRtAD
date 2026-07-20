\name{libertad_gui}
\alias{libertad_gui}
\title{Launch the LibeRtAD benchmark laboratory}
\usage{
libertad_gui(
  benchmark_root = NULL,
  host = "127.0.0.1",
  port = NULL,
  launch.browser = TRUE,
  allow_remote = FALSE
)
}
\arguments{
\item{benchmark_root}{Optional LibeR repository root or benchmark script path.}
\item{host, port, launch.browser}{Passed to \code{shiny::runApp()}.}
\item{allow_remote}{Explicitly permit a non-loopback bind.}
}
\value{Invisibly, the Shiny app.}
\description{
Runs native tape microbenchmarks and, when the LibeR repository harness is
available, launches the paired LibeRation/NONMEM workflow benchmark in a
cancellable background process.
}
