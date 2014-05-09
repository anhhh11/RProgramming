f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 1
  x + g(x)
}