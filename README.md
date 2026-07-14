# LibeRtAD

LibeRtAD is the automatic-differentiation engine for the LibeR population
PK/PD modelling system. It compiles a restricted R-like mathematical language
to a serializable intermediate representation and evaluates persistent CppAD
tapes through RcppEigenAD. R owns only a light R6/external-pointer wrapper;
values, gradients, Jacobians, and Hessians are evaluated in C++.

## Example

```r
library(LibeRtAD)

model <- ad_compile(
  "CL = THETA(1) * exp(ETA(1))\nPENALTY = log(CL)^2",
  at = c(THETA_1 = 2, ETA_1 = 0),
  wrt = c("THETA_1", "ETA_1"),
  outputs = "PENALTY"
)

model$value_gradient(c(THETA_1 = 2, ETA_1 = 0))
model$hessian(c(THETA_1 = 2, ETA_1 = 0))
```

## Installation

LibeRtAD requires R 4.1 or newer, a C++17 toolchain, Rcpp, RcppEigen,
RcppEigenAD, BH, and R6. From a source checkout:

```text
R CMD INSTALL .
```

LibeRtAD is MIT licensed.
