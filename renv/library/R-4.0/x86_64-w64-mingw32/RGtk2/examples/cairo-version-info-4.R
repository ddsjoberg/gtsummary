# R users should not be concerned with compile-time checking, but at runtime:

if (compareVersion(cairoVersionString(), "1.0.0") == 1)
  cat("Running with suitable cairo version:", cairoVersionString(), "\n")

