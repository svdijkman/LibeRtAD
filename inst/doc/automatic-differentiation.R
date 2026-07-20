knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

library(LibeRtAD)
ad_supported()

ir <- ad_ir(
  paste(
    "CL = THETA(1) * exp(ETA(1))",
    "V = THETA(2)",
    "K = CL / V",
    "PENALTY = log(K)^2",
    sep = "\n"
  ),
  outputs = c("K", "PENALTY")
)
ir

model <- ad_compile(
  "CL = THETA(1) * exp(ETA(1))\nPENALTY = log(CL)^2",
  at = c(THETA_1 = 2, ETA_1 = 0),
  wrt = c("THETA_1", "ETA_1"),
  outputs = "PENALTY"
)
model

point <- c(THETA_1 = 2.2, ETA_1 = 0.1)
model$value(point)
model$gradient(point)
model$hessian(point)
model$value_gradient(point)

multi <- ad_compile(
  "SUM = X + Y\nPRODUCT = X * Y",
  inputs = c("X", "Y"),
  outputs = c("SUM", "PRODUCT"),
  at = c(X = 2, Y = 3),
  wrt = c("X", "Y")
)
multi$jacobian(c(X = 4, Y = 5))
multi$tape_info()

dynamic <- ad_compile(
  "Y = X^P + P^X",
  inputs = c("X", "P"), outputs = "Y",
  at = c(X = 2, P = 0), wrt = "X"
)
dynamic$set_dynamic(c(P = 3))
dynamic$value_gradient(c(X = 2))

multi$record(
  at = c(X = 2, Y = 3),
  wrt = "X",
  outputs = "PRODUCT"
)
multi$value_gradient(c(X = 4))

path <- tempfile(fileext = ".rds")
dynamic$save_tape(path)
restored <- ad_load_tape(path)
restored$value_gradient(c(X = 2, P = 4))
unlink(path)

ad_engine_info()

# ad_benchmark_cases()
# result <- ad_benchmark("pk", iterations = 1000, warmups = 50)
# result
# 
# libertad_gui()
