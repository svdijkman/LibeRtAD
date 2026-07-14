# LibeRtAD 0.6.0

- Replaced the legacy R-level AD implementation with a persistent
  RcppEigenAD/CppAD C++ engine.
- Added a serializable, validated expression intermediate representation.
- Added a light R6/external-pointer interface for values, gradients,
  Jacobians, Hessians, and combined value/gradient evaluation.
- Added conditional-expression support and strict rejection of unsupported
  runtime constructs.
- Added registered native routines and C++17 package integration for use by
  LibeRation.

This release is an architectural and API break from the 0.4.x series.
