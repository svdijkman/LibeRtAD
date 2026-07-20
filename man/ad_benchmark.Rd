\name{ad_benchmark}
\alias{ad_benchmark}
\title{Benchmark the persistent C++ automatic-differentiation engine}
\usage{
ad_benchmark(
  case = c("rosenbrock", "pk", "jacobian"),
  iterations = 1000L,
  warmups = 50L,
  optimize = TRUE,
  finite_difference = TRUE
)
}
\arguments{
\item{case}{Built-in workload returned by \code{ad_benchmark_cases()}.}
\item{iterations}{Number of requested measured value and derivative calls. The timer may increase this count to obtain stable resolution.}
\item{warmups}{Number of calls before timing.}
\item{optimize}{Whether CppAD tape optimisation is enabled.}
\item{finite_difference}{Include R central finite-difference comparators.}
}
\value{
An \code{ad_benchmark_result} containing timings, accuracy checks, engine
metadata, and settings.
}
\description{
Separates recording from repeated tape evaluation and compares exact CppAD
derivatives with central finite differences evaluated in R. The results are
intended for local regression and hardware comparison rather than as a
universal performance claim.
}
\examples{
ad_benchmark_cases()
\donttest{ad_benchmark("pk", iterations = 100, warmups = 5)}
}
