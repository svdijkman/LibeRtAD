# LibeRtAD 0.4.1

* Updated manuals (`roxygen2`), README, and getting-started vignette (tape reuse,
  NONMEM expression normalization, sparse Jacobian examples).
* Documented C++ tape helpers `replay_tape_values_cpp()`, `reset_tape_grads_cpp()`,
  and `tape_scalar_value_cpp()`.

# LibeRtAD 0.4.0

* NONMEM expression AD (`nm-expr`), custom forward rules, tape reuse tests, and C++ forward-mode extensions.

# LibeRtAD 0.3.0

* Initial public release preparation.
* Reverse- and forward-mode AD with optional C++ backend.
* Hessian support, tape save/load/reuse, and sparse Jacobian triplets.
* Renamed from RcppAD; package options use `LibeRtAD.*` prefix.
