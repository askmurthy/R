# An one line way of solving it.
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

matequal(a, b)   # FALSE
matequal(d, b)   # TRUE
