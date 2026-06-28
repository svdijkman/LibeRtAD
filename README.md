# LibeRtAD

Reverse- and forward-mode **automatic differentiation** for scalar objectives in R, with an optional C++ backward pass.

## Installation

```r
# From source (requires Rtools on Windows)
devtools::install("path/to/LibeRtAD")
```

## Quick start

```r
library(LibeRtAD)

f <- function(x, y, z) 3 * x^4 + y / z / x + z * 3
backdiff(f, x = 2, y = 2, z = 3)

# Vector arguments
autodiff(function(x) sum(x^2), x = c(1, 2, 3), mode = "reverse")

# Forward mode + C++ backend
autodiff(function(x) sum(sqrt(abs(x))), x = c(1, 4, 9),
         mode = "forward", backend = "cpp")
```

See `inst/examples/basic-example.R` for matrix arguments and piecewise functions.

## Main functions

| Function | Purpose |
|----------|---------|
| `autodiff()` | Unified AD entry (reverse or forward) |
| `backdiff()` | Reverse-mode wrapper |
| `forwarddiff()` | Forward-mode wrapper |
| `autodiff_hessian()` | Hessian matrix |
| `sparse_jacobian()` | Sparse Jacobian |
| `ad_tape_save()` / `ad_tape_load()` | Tape persistence |

## Documentation

After installation:

```r
?LibeRtAD
?autodiff
```

Build manuals from source:

```r
roxygen2::roxygenise("path/to/LibeRtAD")
```

## Vignette

```r
vignette("getting-started", package = "LibeRtAD")
# or: browseVignettes("LibeRtAD")
```

Requires **knitr**, **rmarkdown**, and [Pandoc](https://pandoc.org) (bundled with RStudio).

## License

MIT — see `LICENSE.md` (full text) and `LICENSE` (CRAN stub).
