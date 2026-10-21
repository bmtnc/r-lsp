# A standalone script using the package, as pipelines do.
library(pkgscope)
result <- flag_missing(c(1, NA, 3))
print(result)
